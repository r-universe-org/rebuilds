#' Rerun a single job
#'
#'
rerun_one_job <- function(url, name = 'Deploy to package server', skip_success = FALSE){
  endpoint <- sub("https://github.com/", "/repos/", url)
  job <- get_job_info(endpoint, name = name)
  pkgname <- sub(" .*", "", job$workflow_name)
  if(isTRUE(skip_success) && (job$conclusion ==  'success')){
    message('Skipping ', pkgname)
    return()
  } else {
    message("Triggering ", pkgname, ": ", endpoint)
  }
  endpoint <- sub("/actions/.*", paste0("/actions/jobs/", job$id, '/rerun'), endpoint)
  gh::gh(endpoint, .method = 'POST')
}

get_job_info <- function(url, name){
  endpoint <- sub("https://github.com/", "/repos/", url)
  res <- gh::gh(paste0(endpoint, '/jobs'))
  jobs <- Filter(function(x) grepl(name, x$name, ignore.case = TRUE), res$jobs)
  if(!length(jobs)) stop("No job found matching name: ", name)
  return(jobs[[1]])
}

rerun_one_job_for_universe <- function(universe, job_name, skip_success = FALSE){
  descriptions <- jsonlite::stream_in(url(sprintf("https://%s.r-universe.dev/stats/descriptions", universe)))
  urls <- descriptions[['_buildurl']]
  lapply(urls, function(url){
    try(rerun_one_job(url, job_name, skip_success = skip_success))
  })
}

rebuild_all_webassembly <- function(){
  files <- jsonlite::stream_in(url("https://r-universe.dev/stats/files?type=wasm&before=2024-09-11T19:00:00.000Z&fields=_buildurl,Packaged.Date"))
  files$age <- Sys.Date() - as.Date(rebuilds:::parse_time(files$Packaged$Date))
  files <- subset(files, age < 30 & r > '4.4')
  urls <- unique(files[['_buildurl']])
  lapply(urls, function(url){
    tryCatch(rerun_one_job(url, 'R-release for WebAssembly', skip_success = FALSE), error = wait_for_api_limit_reset)
  })
}

redeploy_everything <- function(){
  files <- jsonlite::stream_in(url("https://bioc.r-universe.dev/stats/files?type=src&fields=_buildurl"))
  urls <- rev(unique(files[['_buildurl']]))
  #lapply(urls, function(url){
  #  message(url)
  #  tryCatch(rerun_one_job(url, 'Deploy to package server', skip_success = FALSE), error = wait_for_api_limit_reset)
  #})
  failures <- jsonlite::stream_in(url("https://bioc.r-universe.dev/stats/files?type=failure&fields=_buildurl"))
  urls <- unique(failures[['_buildurl']])
  lapply(urls, function(url){
    message(url)
    tryCatch(rerun_one_job(url, 'Deploy to package server', skip_success = FALSE), error = message)
  })
}

redeploy_one_for_each <- function(){
  files <- jsonlite::stream_in(url("https://r-universe.dev/stats/files?type=src&fields=_buildurl"))
  files <- files[as.Date(files$published) > '2024-11-13',]
  files <- files[order(files$published, decreasing = TRUE),]
  files <- files[!duplicated(files$user),]
  urls <- files[['_buildurl']]
  lapply(urls, function(url){
    message(url)
    tryCatch(rerun_one_job(url, 'Deploy to package server', skip_success = FALSE), error = message)
  })
}

rebuild_ropensci_docs <- function(){
  rerun_one_job_for_universe('ropensci', 'pkgdown')
}

wait_for_api_limit_reset <- function(err){
  message(err)
  limits <- gh::gh_rate_limit()
  if(limits$remaining == 0){
    secs <- difftime(limits$reset, Sys.time(), units='secs')
    Sys.sleep(secs + 10)
  }
}
