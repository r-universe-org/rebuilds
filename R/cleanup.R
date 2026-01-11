#' Cleanup old files
#'
#' Remove old files from the CRANlike server.
#'
#' @export
#' @param before date before for which to delete older files
delete_old_files <- function(before = '2021-12-03'){
  # Some input validation
  userpwd <- Sys.getenv("CRANLIKEPWD", NA)
  if(is.na(userpwd)) stop("No CRANLIKEPWD set, cannot deploy")
  df <- jsonlite::stream_in(url(paste0('https://r-universe.dev/api/files?before=', before, '&nocache=', runif(1))), verbose = FALSE)

  message("Doing to delete ", nrow(df), " files")
  print(df, row.names = FALSE)
  for(i in seq_len(nrow(df))){
    x <- as.list(df[i,])
    url <- sprintf("https://%s.r-universe.dev/api/packages/%s/%s/%s", x$user, x$package, x$version, x$type)
    if(x$type != 'src')
      url <- paste0(url, '/', x$r)
    message("DELETE ", url)
    h <- curl::new_handle(customrequest = 'DELETE', userpwd = userpwd)
    out <- parse_res(curl::curl_fetch_memory(url, handle = h))
    stopifnot(out$Package == x$package)
  }
}

parse_res <- function(res){
  text <- rawToChar(res$content)
  if(res$status >= 400)
    stop(text)
  jsonlite::fromJSON(text)
}
