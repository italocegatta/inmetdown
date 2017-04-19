#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
aws_import <- function(id, start, end, small = TRUE) {

  # debug
  # id = "A108"
  # start =  "18/04/2017"
  # end =  "18/04/2017"
  # load("R/sysdata.rda")
  # library(magrittr)

  start <- check_date(start)
  end <- check_date(end)

  session <- aws_get_url(id) %>%
    rvest::html_session()

  img_cript <- session %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr('src')

  code_cript <- img_cript %>% stringr::str_extract("(?<==)(.*?)(?==)")

  p1 <- key[key$code == stringr::str_sub(code_cript, 1, 3), "key"][[1]]
  p2 <- key[key$code == stringr::str_sub(code_cript, 4, 6), "key"][[1]]

  form <- rvest::set_values(
    rvest::html_form(session)[[1]],
    `dtaini` = format(start, "%d/%m/%Y"),
    `dtafim` = format(end, "%d/%m/%Y"),
    `aleaNum` = paste0(p1, p2)
  )

  data <- suppressMessages(rvest::submit_form(session, form))

  x <- 0
  repeat {
    x <- x + 1
    table <- aws_try_query(data)
    if (!is.null(table)) break
    if (x > 2) break
  }

  names(table) <- c(
    "data",
    "hora",
    "t_ins",  "t_max", "t_min",
    "ur_ins", "ur_max", "ur_min",
    "pto_orv_ins", "pto_orv_max", "pto_orv_min",
    "pa_int", "pa_max", "pa_min",
    "v_vel", "ento_dir", "v_raj",
    "rad",
    "p"
  )

  z <- table %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(dplyr::vars(hora:rad), as.numeric) %>%
    dplyr::mutate(data = lubridate::dmy(data))

  if (small) {
    z <- z %>%
      dplyr::select(data, hora, t_max, t_min, ur_max, ur_min, rad, p)
  }

  return(z)
}


# Get URL's station
aws_get_url <- function(id) {
  df <- aws_stations()
  df[df$id %in% id, "url"][[1]]
}

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
        stop("All formats failed to parse. No formats found.")
      }
    }
  }

  return(z)
}

aws_try_query <- function(x) {
  tryCatch({
    x %>%
      rvest::html_nodes("table") %>%
      `[[`(6) %>%
      rvest::html_table(header = TRUE) %>%
      `[`(-1, )
  },
  error=function(e) NULL,
  warning=function(w) NULL
  )
}
