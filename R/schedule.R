#' Trigger rebuilds
#'
#' Invoke retries and full rebuilds for all the packages.
#'
#' @export
#' @param retry_days number of days to retry failures builds
#' @param rebuild_days number of days after which to do a full fresh rebuild
trigger_all_rebuilds <- function(retry_days = 3, rebuild_days = 30){
  con <- url('https://r-universe.dev/stats/files?fields=OS_type,_builder.url,_builder.winbinary,_builder.macbinary')
  files <- jsonlite::stream_in(con, verbose = FALSE)
  files$age <- as.numeric(Sys.Date() - as.Date(files$published))
  failures <- subset(files, type == 'failure' & age < retry_days)
  sources <- subset(files, type == 'src' & age < retry_days)
  sources$OS_type[is.na(sources$OS_type)] <- ""
  failtypes <- c("none", "cancelled") # do not retry for check failures right now.
  sources$winfail <- sources[["_builder"]]$winbinary %in% failtypes & sources$OS_type != 'unix'
  sources$macfail <- sources[["_builder"]]$macbinary %in% failtypes & sources$OS_type != 'windows'
  retries <- subset(sources, winfail | macfail)
  retry_urls <- unique(c(retries[['_builder']]$url, failures[['_builder']]$url))

  # Retry failures only, set max_age to check against date of first run attempt
  lapply(retry_urls, retry_run, max_age = retry_days)

  # Fresh full rebuilds (not just retries)
  builds <- subset(files, (type %in% c('src', 'failure')))
  do_rebuild <- (builds$age > 0) & (builds$age %% rebuild_days == 0)
  average_size <- round(length(do_rebuild) / rebuild_days)
  min_rebuilds <- average_size - 100
  need_more <-  min_rebuilds - sum(do_rebuild)
  if(need_more > 0){
    # Select some extra to get to 1/30th of the total to relieve the busy days
    weights <- get_oversize(as.character(builds$age), average_size)
    candidates <- which(!do_rebuild)
    do_rebuild[sample(candidates, need_more, prob = weights[candidates])] <- TRUE
  }

  # print some diagnostics
  rebuilds <- builds[do_rebuild,]
  cat(sprintf("Rebuilding %d packages\n", sum(do_rebuild)))
  cat("Age distribution after rebuilds:\n")
  print(as.data.frame(table(Age = builds[!do_rebuild,'age'])))

  # Trigger rebuilds with pauzes in between
  for(i in seq_len(nrow(rebuilds))){
    rebuild_package(rebuilds[i,'user'], rebuilds[i,'package'])
    if(i %% 50 == 0) {
      print_message("Triggered %d rebuilds. Waiting for a few minutes.", i)
      Sys.sleep(900)
    }
  }
}

get_oversize <- function(x, target){
  oversize <- unname(table(x)[x]) - target
  oversize[oversize < 0] <- 0
  oversize
}

# max_age below refers to the date of the 1st attempt of this run. The 'published' field
# above OTOH refers the most recent attempt/failure, so it resets to 0 for every new attempt.
retry_run <- function(url, max_age = 5){
  endpoint <- sub("https://github.com/", "/repos/", url)
  res <- gh::gh(endpoint) #this errors if the run is expired or deleted
  created <- parse_time(res$created_at)
  age <- difftime(Sys.time(), created, units = 'days')
  if(age > max_age) {
    print_message("Too old to retry (%d days): %s", as.integer(age), endpoint)
  } else {
    print_message("Retrying (%d days old): %s", as.integer(age), endpoint)
    try(gh::gh(sprintf('%s/rerun-failed-jobs', endpoint), .method = 'POST'))
  }
}

rebuild_package <- function(universe, pkg){
  print_message("Rebuilding %s/%s", basename(universe), pkg)
  trigger_workflow(repository = paste0('r-universe/', universe), workflow = 'build.yml', inputs = list(package = pkg))
}

delete_package <- function(universe, pkg){
  userpwd <- Sys.getenv("CRANLIKEPWD")
  message("Deleting: ", pkg)
  h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
  url <- sprintf("https://%s.r-universe.dev/packages/%s", universe, pkg)
  out <- parse_res(curl::curl_fetch_memory(url, handle = h))
  stopifnot(out$Package == pkg)
}

list_universes <- function(){
  res <- gh::gh('/users/r-universe/repos', per_page = 100, .limit = 1e5)
  names <- tolower(vapply(res, function(x){x$name}, character(1)))
  updated <- parse_time(vapply(res, function(x){x$pushed_at}, character(1)))
  created <- parse_time(vapply(res, function(x){x$created_at}, character(1)))
  data.frame(user = names, updated = updated, created = created)
}

parse_time <- function(str){
  as.POSIXct(chartr('TZ', '  ', str))
}

print_message <- function(...){
  message(sprintf(...))
}
