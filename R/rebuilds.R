#' Rebuild packages
#'
#' Automatically trigger workflows once every n days.
#'
#' @export
#' @rdname rebuilds
#' @param repository name of the github repository
#' @param workflow name of the workflow to trigger
#' @param days trigger rebuild every n days
trigger_rebuilds <- function(repository = 'r-universe/jeroen', workflow = 'build.yml', days = 90){
  if(basename(repository) == 'cran') {
    return()
  }
  url <- sprintf('https://github.com/%s', repository)
  stats <- package_stats(monorepo = url)
  age <- unclass(Sys.Date() - as.Date(stats$modified))
  select <- (age %/% days > 0) & (age %% days == 0)
  rebuilds <- stats[select,]
  print(rebuilds)
  for(pkg in rebuilds$file){
    rebuild_one(repository = repository, pkg = pkg, workflow = workflow)
  }
  print("Retrying failed builds...")
  rebuild_failures(basename(repository))
  rebuild_missing_sources(basename(repository))
  print("All done!")
  invisible()
}

#' @export
#' @rdname rebuilds
rebuild_failures <- function(universe = NULL){
  subdomain <- paste(sprintf('%s.', universe), collapse = '')
  endpoint <- sprintf('https://%sr-universe.dev/stats/failures', subdomain)
  df <- jsonlite::stream_in(url(endpoint), verbose = FALSE)
  for(i in seq_len(nrow(df))){
    rebuild_one(paste0('r-universe/', df[['_user']][i]), df$Package[i])
  }
  df
}

#' @export
#' @rdname rebuilds
#' @param universe name of the universe, use NULL for all universes
rebuild_vignettes <- function(universe = 'jeroen'){
  subdomain <- paste(sprintf('%s.', universe), collapse = '')
  endpoint <- sprintf('https://%sr-universe.dev/stats/vignettes?limit=100000', subdomain)
  df <- jsonlite::stream_in(url(endpoint), verbose = FALSE)
  df <- unique(df[c('universe', 'package')])
  row.names(df) <- NULL
  for(i in seq_len(nrow(df))){
    rebuild_one(paste0('r-universe/', df$universe[i]), df$package[i])
  }
  df
}

#' @export
#' @rdname rebuilds
#' @param fields extra fields you want to get
list_all_packages <- function(fields = '_builder.upstream', type = 'src'){
  endpoint <- sprintf('https://r-universe.dev/stats/files?type=%s&fields=%s', type, paste(fields, collapse = ','))
  jsonlite::stream_in(url(endpoint))
}

rebuild_has_sysdeps <- function(skip = 'gcc'){
  df <- list_all_packages('_builder.sysdeps')
  hasdeps = sapply(df[['_builder']]$sysdeps, function(x){length(setdiff(x$source, skip)) > 0})
  df <- df[hasdeps,]
  for(i in seq_len(nrow(df))){
    rebuild_one(paste0('r-universe/', df$user[i]), df$package[i])
  }
  df
}

#' @export
#' @param before date before which to rebuild
#' @rdname rebuilds
rebuild_oldies <- function(universe, before = '2022-05-10', type = 'src'){
  subdomain <- paste(sprintf('%s.', universe), collapse = '')
  endpoint <- sprintf('https://%sr-universe.dev/stats/files?before=%s', subdomain, before)
  oldies <- jsonlite::stream_in(url(endpoint), verbose = FALSE)
  df <- oldies[oldies$type == type,]
  if(length(universe))
    cat(sprintf("Rebuilding %d packages in: %s\n", nrow(df), universe))
  for(i in seq_len(nrow(df))){
    cat(sprintf('\r[%d] %s', i, df$package[i]))
    rebuild_one(paste0('r-universe/', df$user[i]), df$package[i])
  }
  df
}

#' @export
#' @rdname rebuilds
rebuild_failed_vignettes <- function(universe = NULL){
  subdomain <- paste(sprintf('%s.', universe), collapse = '')
  endpoint <- sprintf('https://%sr-universe.dev/stats/files?type=src&fields=_builder.status', subdomain)
  oldies <- jsonlite::stream_in(url(endpoint), verbose = FALSE)
  df <- oldies[oldies[['_builder']]$status == 'failure',]
  if(length(universe))
    cat(sprintf("Rebuilding %d packages in: %s\n", nrow(df), universe))
  message("Rebuilding ", nrow(df), " packages!")
  for(i in rev(seq_len(nrow(df)))){
    cat(sprintf('\r[%d] %s', i, df$package[i]))
    rebuild_one(paste0('r-universe/', df$user[i]), df$package[i])
  }
  df
}

#' @export
#' @rdname rebuilds
rebuild_bioc_packages <- function(){
  df <- read.csv('https://r-universe-org.github.io/cran-to-git/crantogit.csv', stringsAsFactors = FALSE)
  df <- df[df$registry == 'bioc',]
  df$user <- basename(dirname(df$url))
  for(i in 37:nrow(df)){
    cat(sprintf('[%d] %s/%s\n', i, df$user[i], df$package[i]))
    try(rebuild_one(paste0('r-universe/', df$user[i]), df$package[i]))
    if(i %% 50 == 0) {
      print_message("Triggered %d rebuilds. Waiting for a few minutes.", i)
      Sys.sleep(900)
    }
  }
}

#' @export
#' @rdname rebuilds
rebuild_all_oldies <- function(before = '2022-04-01'){
  rebuild_oldies(universe = NULL, before = before)
}

#' @export
#' @rdname rebuilds
rebuild_missing_binaries <- function(universe = 'ropensci'){
  endpoint <- sprintf('https://%s.r-universe.dev', universe)
  packages <- jsonlite::stream_in(url(paste0(endpoint, '/src/contrib')), verbose = FALSE)
  macos <- jsonlite::stream_in(url(paste0(endpoint, '/bin/macosx/contrib/4.2')), verbose = FALSE)
  windows <- jsonlite::stream_in(url(paste0(endpoint, '/bin/windows/contrib/4.2')), verbose = FALSE)
  missing_mac <- which(!paste(packages$Package, packages$Version) %in% paste(macos$Package, macos$Version))
  missing_win <- which(!paste(packages$Package, packages$Version) %in% paste(windows$Package, windows$Version))
  missing <- unique(c(missing_mac, missing_win))
  message("Checking: ", universe)
  sapply(packages$Package[missing], function(pkg){
    rebuild_one(paste0('r-universe/', universe), pkg)
  })
}

#' @export
#' @rdname rebuilds
rebuild_missing_sources <- function(universe = 'ropensci'){
  available <- jsonlite::fromJSON(sprintf('https://%s.r-universe.dev/packages', universe))
  tryCatch({
    all <- gh::gh(sprintf('/repos/r-universe/%s/contents', universe))
    packages <- vapply(all, function(x){x$name}, character(1))
    packages <- grep('^\\.', packages, value = TRUE, invert = TRUE)
    missing <- packages[!(packages %in% available)]
    sapply(missing, function(pkg){
      rebuild_one(paste0('r-universe/', universe), pkg)
      return(pkg)
    }, USE.NAMES = FALSE)
  }, error = function(e){
      cat("Failure for: ", universe, "\n")
      return(e)
  })
}

#' @export
#' @rdname rebuilds
rebuild_mixed_case_login <- function(){
  checks <- jsonlite::stream_in(url('https://r-universe.dev/stats/checks?limit=100000'), verbose = FALSE)
  different <- vapply(checks$runs, function(x){any(x$builder$maintainerlogin != tolower(x$builder$maintainerlogin))}, logical(1))
  logins <- vapply(checks$runs, function(x){as.character(x$builder$maintainerlogin[1])}, character(1))
  mixed <- which(different)
  lapply(mixed, function(i){
    pkg <- checks$package[i]
    universe <- checks$user[i]
    cat(sprintf("Rebuilding package %s/%s by %s\n", universe, pkg, logins[i]))
    rebuild_one(paste0('r-universe/', universe), pkg)
  })
  invisible()
}

#' @export
#' @rdname rebuilds
rebuild_all_missing_sources <- function(){
  orgstats <- jsonlite::stream_in(url('https://r-universe.dev/stats/universes'), verbose = FALSE)
  all_orgs <- rev(orgstats$universe)
  out <- sapply(all_orgs, function(orgname){
    rebuild_missing_sources(orgname)
  })
  Filter(length, out)
}

#' @export
#' @rdname rebuilds
rebuild_all_missing_binaries <- function(){
  orgstats <- jsonlite::stream_in(url('https://r-universe.dev/stats/universes'), verbose = FALSE)
  total <- 0
  lapply(orgstats$universe, function(orgname){
    total <<- total + length(rebuild_missing_binaries(orgname))
    message("total: ", total)
  })
}

#' @export
#' @rdname rebuilds
cancel_queued_builds <- function(universe = 'ropensci'){
  # limit here looks at .limit most recent runs, and then filters by status. So it needs to be high.
  runs <- gh::gh(sprintf('/repos/r-universe/%s/actions/runs', universe), status = 'queued', .limit = 1000)
  lapply(runs$workflow_runs, function(run){
    cat("Cancelling build", run$id, "in", universe, "\n")
    url <- sprintf('/repos/r-universe/%s/actions/runs/%d/cancel', universe, run$id)
    gh::gh(url, .method = 'POST')
  })
}

#' @export
#' @rdname rebuilds
cancel_all_queued_builds <- function(){
  universes <- gh::gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(universes, function(universe){
    cat("Looking for queued builds in:", universe$name, "\n")
    cancel_queued_builds(universe$name)
  })
}

#' @export
#' @rdname rebuilds
#' @param pkg name of package to delete
delete_one <- function(universe, pkg, version = 'all', type = 'all', build = 'all'){
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  message(sprintf("Deleting: %s/%s (%s-%s)", universe, pkg, type, build))
  h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
  url <- sprintf("https://%s.r-universe.dev/packages/%s", universe, pkg)
  if(!identical(version, 'all')){
    url <- paste0(url, '/', version)
    if(!identical(type, 'all')){
      url <- paste0(url, '/', type)
      if(!identical(type, 'all')){
        url <- paste0(url, '/', build)
      }
    }
  }
  res <- curl::curl_fetch_memory(url, handle = h)
  if(res$status_code > 200) stop("Failed to delete")
  out <- jsonlite::fromJSON(rawToChar(res$content))
  stopifnot(out$Package == pkg)
}

#' @export
#' @rdname rebuilds
delete_all_old_binaries <- function(){
  universes <- gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(universes, function(x){
    cat("Checking universe:", x$name, "\n")
    delete_old_binaries(x$name)
  })
}

delete_old_binaries <- function(universe){
  endpoint <- sprintf('https://%s.r-universe.dev', universe)
  macos <- jsonlite::stream_in(url(paste0(endpoint, '/bin/macosx/contrib/4.1')), verbose = FALSE)
  for(i in seq_len(nrow(macos))){
    x <- as.list(macos[i,])
    delete_one(universe, x$Package, x$Version, 'mac', '4.1')
  }
  windows <- jsonlite::stream_in(url(paste0(endpoint, '/bin/windows/contrib/4.1')), verbose = FALSE)
  for(i in seq_len(nrow(windows))){
    x <- as.list(windows[i,])
    delete_one(universe, x$Package, x$Version, 'win', '4.1')
  }
}

delete_old_builds <- function(before = '2021-04-01'){
  checks <- jsonlite::stream_in(url('https://r-universe.dev/stats/checks?limit=9999999'), verbose = FALSE)
  checks$builddate <- structure(sapply(checks$runs, function(df){df$builder$date[1]}), class = class(Sys.time()))
  oldies <- checks[checks$builddate < before & checks$user != 'hrbrmstr_gitlab.com',]
  for(i in seq_along(oldies$user)){
    cat(sprintf("Deleting %s from %s\n", oldies$package[i], oldies$user[i]))
    delete_one(oldies$user[i], oldies$package[i])
    rebuild_one(paste0('r-universe/', oldies$user[i]), oldies$package[i])
  }
}

#' @import gert
package_stats <- function(monorepo){
  repo <- git_clone(monorepo, tempfile())
  modules <- git_ls(repo = repo)
  pkgs <- modules[!grepl("^\\.", modules$path),]
  git_stat_files(pkgs$path, repo = repo)
}

rebuild_one <- function(repository, pkg, workflow = 'build.yml'){
  cat(sprintf("Rebuilding %s/%s\n", basename(repository), pkg))
  trigger_workflow(repository = repository, workflow = 'build.yml', inputs = list(package = pkg))
}

#' @importFrom gh gh
trigger_workflow <- function(repository, workflow = 'sync.yml', inputs = NULL){
  url <- sprintf('/repos/%s/actions/workflows/%s/dispatches', repository, workflow)
  gh(url, .method = 'POST', ref = 'master', inputs = inputs)
}

#' @importFrom gh gh
enable_workflows <- function(repository, workflow = c('sync.yml', 'rebuilds.yml')){
  lapply(workflow, function(script){
    url <- sprintf('/repos/%s/actions/workflows/%s/enable', repository, script)
    gh(url, .method = 'PUT')
  })
}

#' @export
enable_all_workflows <- function(){
  universes <- gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(rev(universes), function(x){
    repo <- paste0('r-universe/', x$name)
    cat("Enabling workflows for:", repo, "\n")
    enable_workflows(repo)
  })
}

#' @export
#' @rdname rebuilds
rebuild_all_remotes <- function(){
  universes <- gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(universes, function(x){
    cat("Checking universe:", x$name, "\n")
    rebuild_universe_remotes_only(x$name)
  })
}

#' @export
#' @rdname rebuilds
rebuild_universe_remotes_only <- function(universe){
  pkgs <- list_remote_packages(universe)
  lapply(pkgs, function(pkg){
    rebuild_one(paste0('r-universe/', universe), pkg = pkg)
  })
}

list_remote_packages <- function(user){
  tmp <- tempfile('.config')
  on.exit(unlink(tmp))
  url <- sprintf('https://raw.githubusercontent.com/r-universe/%s/master/.gitmodules', user)
  curl::curl_download(url, tmp)
  txt <- system(sprintf('git config --file %s --list', tmp), intern = T)
  lines <- grep('registered=false', txt, fixed = TRUE, value = TRUE)
  vapply(strsplit(lines, '.', fixed = TRUE), function(x){x[2]}, character(1))
}

#' @export
#' @rdname rebuilds
remove_packages_with_remotes <- function(org){
  withr::with_tempdir({
    gert::git_clone(paste0('https://github.com/r-universe/', org))
    setwd(org)
    if(file.exists('.remotes.json')){
      remotes <- jsonlite::read_json('.remotes.json')
      owners <- unique(vapply(remotes, function(x){x$from}, character(1)))
      cat("Removing packages", paste(owners, collapse = ", "), "from universe:", org, "\n")
      lapply(owners, function(pkg){
        system2("git", c("rm", pkg))
      })
      gert::git_commit("Force reset packages with remotes")
      gert::git_push()
      trigger_workflow(paste0('r-universe/', org), workflow = 'sync.yml')
    } else {
      cat("No remotes for universe:", org, "\n")
    }
  })
}

#' @export
#' @rdname rebuilds
remove_all_packages_with_remotes <- function(){
  universes <- gh('/orgs/r-universe/repos', .limit = Inf)
  lapply(universes, function(x){
    if(x$name == 'r-lib') return()
    cat("Checking universe:", x$name, "\n")
    remove_packages_with_remotes(x$name)
  })
}
