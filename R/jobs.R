#' Rerun a single job
#'
#'
rerun_one_job <- function(url, name = 'Deploy to package server'){
  endpoint <- sub("https://github.com/", "/repos/", url)
  job <- get_job_info(endpoint, name = name)
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
