#' Trigger rebuilds
#'
#' Invoke retries and full rebuilds for all the packages.
#'
#' @export
#' @param retry_days number of days to retry failures builds
#' @param rebuild_days number of days after which to do a full fresh rebuild
trigger_all_rebuilds <- function(retry_days = 3, rebuild_days = 30){
  failures <- jsonlite::stream_in(url('https://r-universe.dev/stats/files?type=failure&fields=_buildurl'), verbose = FALSE)
  sources <- jsonlite::stream_in(url('https://r-universe.dev/stats/files?type=src&fields=_buildurl,_jobs'), verbose = FALSE)
  dupes <- paste(sources$user, sources$package) %in% paste(failures$user, failures$package)
  sources <- sources[!dupes,]
  sources$hasfail <- vapply(sources[['_jobs']], function(jobs){
    any(jobs$check == 'FAIL' & !grepl('wasm', jobs$config))
  }, logical(1))
  failures$age <- as.numeric(Sys.Date() - as.Date(failures$published))
  sources$age <- as.numeric(Sys.Date() - as.Date(sources$published))
  retry_failures <- subset(failures, age < retry_days)
  retry_sources <- subset(sources, hasfail & age < retry_days)

  # Retry failures only, set max_age to check against date of first run attempt
  retry_urls <- unique(c(retry_failures[['_buildurl']], retry_sources[['_buildurl']]))
  lapply(retry_urls, retry_run, max_age = retry_days)

  # Fresh full rebuilds (not just retries)
  builds <- rbind(failures[c('user', 'package', 'age')], sources[c('user', 'package', 'age')])

  cat("=== NON CRAN universes ===\n\n")
  notcran <- subset(builds, user != 'cran')
  trigger_full_rebuilds(notcran, rebuild_days = rebuild_days)

  # Workaround for GitHub problems
  if(curl::curl_fetch_memory('https://github.com/cran')$status != 200){
    stop("https://github.com/cran unavailable")
  }

  cat("=== CRAN universe ===\n\n")
  oncran <- subset(builds, user == 'cran')
  trigger_full_rebuilds(oncran, rebuild_days = rebuild_days)

  # Delete files older than 100 days
  delete_old_files(Sys.Date() - 100)
}

trigger_full_rebuilds <- function(builds, rebuild_days, delay = 5){
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
  print(as.data.frame(table(Age = builds[!do_rebuild,'age'])), row.names = FALSE)

  # Trigger rebuilds with pauzes in between
  for(i in seq_len(nrow(rebuilds))){
    rebuild_package(rebuilds[i,'user'], rebuilds[i,'package'])
    Sys.sleep(delay)
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
  tryCatch({
    endpoint <- sub("https://github.com/", "/repos/", url)
    res <- gh::gh(endpoint) #this errors if the run is expired or deleted
    created <- parse_time(res$created_at)
    age <- difftime(Sys.time(), created, units = 'days')
    if(age > max_age) {
      print_message("Too old to retry (%d days): %s", as.integer(age), endpoint)
    } else {
      print_message("Retrying (%d days old): %s", as.integer(age), endpoint)
      gh::gh(sprintf('%s/rerun-failed-jobs', endpoint), .method = 'POST')
    }
  }, error = function(e){
    print_message("ERROR for %s: %s", url, e$message)
  })
}

rebuild_package <- function(universe, pkg){
  print_message("Rebuilding %s/%s", basename(universe), pkg)
  tryCatch(trigger_workflow(repository = paste0('r-universe/', universe), workflow = 'build.yml', inputs = list(package = pkg)), error = function(e){
    message(sprintf('ERROR rebuilding %s/%s: %s', basename(universe), pkg, e$message))
  })
}

delete_package <- function(universe, pkg){
  userpwd <- Sys.getenv("CRANLIKEPWD")
  message("Deleting: ", pkg)
  h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
  url <- sprintf("https://%s.r-universe.dev/api/packages/%s", universe, pkg)
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
