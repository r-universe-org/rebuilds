#' Rerun a single job
#'
#'
rerun_one_job <- function(url, name = 'Deploy to package server'){
  endpoint <- sub("https://github.com/", "/repos/", url)
  job <- get_job_info(endpoint, name = name)
  endpoint <- sub("/actions/.*", paste0("/actions/jobs/", job$id, '/rerun'), endpoint)
  message("Triggering: ", endpoint)
  gh::gh(endpoint, .method = 'POST')
}

get_job_info <- function(url, name){
  endpoint <- sub("https://github.com/", "/repos/", url)
  res <- gh::gh(paste0(endpoint, '/jobs'))
  jobs <- Filter(function(x) grepl(name, x$name, ignore.case = TRUE), res$jobs)
  if(!length(jobs)) stop("No job found matching name: ", name)
  return(jobs[[1]])
}

rerun_one_job_for_all <- function(universe, job_name){
  descriptions <- jsonlite::stream_in(url(sprintf("https://%s.r-universe.dev/stats/descriptions", universe)))
  urls <- descriptions[['_builder']]$url
  lapply(urls, function(url){
    try(rerun_one_job(url, job_name))
  })
}

rebuild_all_ropensci_docs <- function(){
  rerun_one_job_for_all('ropensci', 'documentation')
}
