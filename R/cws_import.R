#' @export
cws_import <- function(id, start, end, stations = NULL, proxy = ".") {
  start <- check_date(start)
  end <- check_date(end)

  if (end < start) {
    stop("Data final incoerente")
  }

  dates <- split_dates(start, end)

  bdmep <- list()
  sonabra <- list()

  if (!isTRUE(dates$bdmep)) {
    bdmep <- import_bdmep(id, dates$bdmep[1], dates$bdmep[2], proxy)
  }

  if (!isTRUE(dates$sonabra)) {
    sonabra <- import_sonabra(id, dates$sonabra[1], dates$sonabra[2], stations, proxy)
  }

  dplyr::bind_rows(bdmep, sonabra)
}

split_dates <- function(start, end, min = 90) {

  seq <- seq.Date(start, end, '1 day')

  dummy <- (seq < (lubridate::today("GMT") - min))

  if (length(seq[dummy]) != 0) {
    bdmep <- range(seq[dummy])
  } else {
    bdmep <- TRUE
  }

  if (length(seq[!dummy]) != 0) {
    sonabra <- range(seq[!dummy])
  } else {
    sonabra <- TRUE
  }

  list(bdmep = bdmep, sonabra = sonabra)
}
