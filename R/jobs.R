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

rerun_one_job_for_all <- function(universe, job_name, skip_success = FALSE){
  descriptions <- jsonlite::stream_in(url(sprintf("https://%s.r-universe.dev/stats/descriptions", universe)))
  urls <- descriptions[['_buildurl']]
  lapply(urls, function(url){
    try(rerun_one_job(url, job_name, skip_success = skip_success))
  })
}

rebuild_ropensci_docs <- function(){
  rerun_one_job_for_all('ropensci', 'documentation')
}

rebuild_webassembly <- function(universe){
  rerun_one_job_for_all(universe, 'webassembly', skip_success = TRUE)
}

rebuild_all_webassembly <- function(){
  universes <- gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(universes, function(x){
    rebuild_webassembly(x$name)
  })
}

