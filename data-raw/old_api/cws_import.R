# cws_import <- function(id, start, end, proxy = ".", stations = NULL) {
#
#   start <- check_date(start)
#   end <- check_date(end)
#
#   if (end < start) {
#     stop("Data final incoerente")
#   }
#
#   dates <- split_dates(start, end)
#
#   sonabra <- list()
#   bdmep <- list()
#
#   if (!isTRUE(dates$sonabra)) {
#     sonabra <- import_sonabra(id, dates$sonabra[1], dates$sonabra[2], stations, proxy)
#   }
#
#   if (!isTRUE(dates$bdmep)) {
#     bdmep <- import_bdmep(id, dates$bdmep[1], dates$bdmep[2], stations, proxy)
#   }
#
#
#
#   dplyr::bind_rows(bdmep, sonabra)
# }
