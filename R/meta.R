#' Trigger rebuilds
#'
#' Invoke retries and full rebuilds for all the packages.
#'
#' @export
#' @param cycle number of days to rebuild things
trigger_all_rebuilds <- function(cycle = 30){
  files <- jsonlite::stream_in(url('https://r-universe.dev/stats/files?fields=_builder.url'))
  files$age <- as.numeric(Sys.Date() - as.Date(files$published))
  sources <- subset(files, files$type == 'src')
  sources$published <- as.Date(sources$published)
  regex <- paste0("^", substring(getRversion(), 1, 3))
  win_bins <- subset(files, files$type == 'win' & grepl(regex, files$r))
  mac_bins <- subset(files, files$type == 'mac' & grepl(regex, files$r))
  missing_win <- is.na(match(paste(sources$package, sources$version), paste(win_bins$package, win_bins$version)))
  missing_mac <- is.na(match(paste(sources$package, sources$version), paste(mac_bins$package, mac_bins$version)))
  retries <- sources[missing_win | missing_mac,]
  retries <- subset(retries, retries$age < 30)
  failures <- subset(files, files$type == 'failure')
  failures <- subset(failures, failures$age < 30)

  # Retry existing builds
  retry_urls <- unique(c(retries[['_builder']]$url, failures[['_builder']]$url))
  lapply(retry_urls, retry_run)

  # Full rebuilds (not retries)
  rebuilds <- subset(sources, (sources$age > 0) & (sources$age %% cycle == 0))
  for(i in seq_len(nrow(rebuilds))){
    rebuild_package(rebuilds[i,'user'], rebuilds[i,'package'])
  }
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
