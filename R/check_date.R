# Parse and chek date
check_date <- function(x) {
  if (!lubridate::is.Date(x)) {
    test1 <- tryCatch(lubridate::dmy(x), warning=function(w) w)
    if (!any((class(test1) == "warning") == TRUE)) {
      z <- test1
    } else {
      test2 <- tryCatch(lubridate::ymd(x), warning=function(w) w)
      if (lubridate::is.Date(test2)) {
        z <- test2
      } else {
        stop("All formats failed to parse to date. No formats found.")
      }
    }
  } else {
    z <- x
  }

  if (Sys.Date() - z > 365) {
    stop("Search only the last 365 days")
  }

  return(z)
}
