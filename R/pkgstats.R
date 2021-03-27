

#' Download cran daily logs and builds data.table
#'
#' Downloads daily build logs (these are around 75MB per day so
#' don't try to download too many), combines them and returns.
#' A combined tmp file is stored under the directory as defined by the TMPDIR
#' environment variable that should be added to ~/.Renviron.
#'
#' @param from start date
#' @param to end date
#'
#' @return data.table
#' @export
get_cran_logs <- function(from = as.Date("2021-03-02"), to = as.Date("2021-03-04")){

  message("Downloading daily logs from ", from, " to ", to)
  logdate <- seq(from, to, by = 'day')
  year <- as.POSIXlt(logdate)$year + 1900
  urls <- paste0('http://cran-logs.rstudio.com/', year, '/', logdate, '.csv.gz')

  if(length(urls) > 10){
    message("More than 10 files to download, this could take a while. Please be patient.")
  }

  dir.create(file.path(Sys.getenv('TMPDIR'), "cranlogs"))
  for(i in 1:length(urls)){
    if(url_exists(urls[i])){
      utils::download.file(urls[i],
                    file.path(Sys.getenv('TMPDIR'), "cranlogs", paste0(logdate[i], '.csv.gz')))
    } else {
      warning("Skipping, URL ", urls[i], " does not exist")
    }
  }

  # Combine
  fns <- list.files(file.path(Sys.getenv('TMPDIR'), "cranlogs"), full.names=TRUE)
  logs <- list()
  for (f in fns) {
    logs[[f]] <- data.table::fread(f)
  }
  d <- data.table::rbindlist(logs)
  d[, date:=as.Date(date)]

  fst::write_fst(d, file.path(Sys.getenv('TMPDIR'), "cranlogs", "CRANlogs.fst"), 100)
  d
}



#' Check if URL exists
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {

  # suppressPackageStartupMessages({
  #   require("httr", quietly = FALSE, warn.conflicts = FALSE)
  # })

  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.

  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)

        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }

  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }

  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)

  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)

  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {

    res <- sGET(x, ...)

    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }

    return(TRUE)

  } else {
    return(TRUE)
  }

}
