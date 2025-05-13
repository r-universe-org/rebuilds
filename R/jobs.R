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
  files <- jsonlite::stream_in(url(sprintf("https://%s.r-universe.dev/stats/files?type=src&limit=9999999&fields=_buildurl", universe)))
  urls <- files[['_buildurl']]
  lapply(urls, function(url){
    try(rerun_one_job(url, job_name, skip_success = skip_success))
    Sys.sleep(6)
  })
}

rerun_all_r_devel_for_universe <- function(universe = 'cran', type = 'linux', version = '4.6.0'){
  files <- jsonlite::stream_in(url(sprintf("https://%s.r-universe.dev/stats/files?limit=9999999&fields=_buildurl,_usedby", universe)))
  skiplist <- files[files$r == '4.6.0' & files$type=='linux','package']
  sources <- files[files$type=='src',]
  sources <- sources[is.na(match(sources$package, skiplist)),]
  sources <- sources[order(sources[['_usedby']], decreasing = TRUE), ]
  urls <- sources[['_buildurl']]
  lapply(urls, function(url){
    try(rerun_one_job(url, name = paste("R-devel for", type), skip_success = FALSE))
    Sys.sleep(2)
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
  files <- read_ndjson("https://r-universe.dev/stats/files?type=src&fields=_buildurl")
  files <- files[order(files$published),]
  failures <- read_ndjson("https://r-universe.dev/stats/files?type=failure&fields=_buildurl")
  failures <- failures[order(failures$published),]
  all_urls <- unique(c(files[['_buildurl']], failures[['_buildurl']]))
  lapply(all_urls, function(url){
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

new_server_files <- function(){
  h <- curl::new_handle(resolve = "*:443:165.227.211.221")
  jsonlite::stream_in(curl::curl("https://r-universe.dev/stats/files?type=src&fields=_buildurl", handle = h))
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
