# source("R/get_stations.R")
#
# # Get URL's station
# get_url <- function(x, id) {
#   df <- get_stations(x = x)
#
#   aux <- df$id %in% id
#
#   if (!any(aux)) stop(sprintf("'%s' station not fund.", id))
#
#   df[aux, "url"][[1]]
# }
#
# check_date <- function(x) {
#   if (!lubridate::is.Date(x)) {
#     test1 <- tryCatch(lubridate::dmy(x), warning=function(w) w)
#     if (!any((class(test1) == "warning") == TRUE)) {
#       z <- test1
#     } else {
#       test2 <- tryCatch(lubridate::ymd(x), warning=function(w) w)
#       if (lubridate::is.Date(test2)) {
#         z <- test2
#       } else {
#         stop("All formats failed to parse. No formats found.")
#       }
#     }
#   } else {
#     z <- x
#   }
#
#   return(z)
# }
#
#
#
# #  ------------------------------------------------------------------------
#
#
#
# x = "aws"
# id = "A123"
# start =  Sys.Date() -2
# end =  Sys.Date() - 1
# load("R/sysdata.rda")
#
#
# lubridate::now(tzone = "UTC")
