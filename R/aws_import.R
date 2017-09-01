#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
aws_import <- function(id, start, end, small = TRUE) {
  ## testar valores unicos
  id = dplyr::enquo(id)

  stations <- aws_station() %>%
    dplyr::filter(id %in% !!id)

  start <- check_date(start)
  end <- check_date(end)
  end_hour <- ifelse(
    end == Sys.Date(),
    lubridate::hour(lubridate::now("UTC"))-1,
    23
  )

  session <- suppressWarnings(rvest::html_session(stations$url))

  code_cript <- session %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr('src') %>%
    stringr::str_extract("(?<==)(.*?)(?==)")

  p1 <- key[key$code == stringr::str_sub(code_cript, 1, 3), "key"][[1]]
  p2 <- key[key$code == stringr::str_sub(code_cript, 4, 6), "key"][[1]]

  form <- rvest::set_values(
    rvest::html_form(session)[[1]],
    `dtaini` = format(start, "%d/%m/%Y"),
    `dtafim` = format(end, "%d/%m/%Y"),
    `aleaNum` = paste0(p1, p2)
  )

  x <- 0
  repeat {
    data <- tryCatch(
      {
        suppressMessages(rvest::submit_form(session, form))
      },
      error=function(e) NULL,
      warning=function(w) NULL
    )

    x <- x + 1
    if (!is.null(data)) break
    if (x > 15) break
  }

  nodes_table  <- try(rvest::html_nodes(data, "table")[[6]], silent = TRUE)

  if (inherits(nodes_table, "try-error")) {
    table <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = 19))
    table[, 1] <- c(start, end)

    if (end == Sys.Date()) {
      table[, 2] <- c(0, end_hour)
    } else {
      table[, 2] <- c(0, 23)
    }

  } else {
    table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
  }

  names(table) <- c(
    "date",
    "hour",
    "t_ins",  "t_max", "t_min",
    "rh_ins", "rh_max", "rh_min",
    "dp_ins", "dp_max", "dp_min",
    "ap_ins", "ap_max", "ap_min",
    "ws", "wd", "wg",
    "rad",
    "prec"
  )

  table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hour:prec), as.double))

  # quando esta tudo NA cria data, do contrario vem como caractere precia converter
  table <- dplyr::mutate(
    table,
    date = as.Date(ifelse(is.character(date), lubridate::dmy(date), date),  origin = "1970-01-01"),
    rad = ifelse(rad < 0, NA_real_, rad) / 1000,
    date_time = lubridate::ymd_hms(paste0(end, "-", hour, ":0:0"))
  )

  if (max(table$date_time) < lubridate::ymd_hms(paste0(end, "-", end_hour, ":0:0"))) {
    table <- dplyr::add_row(table, date = end, hour = end_hour)
  }

  table <-  table %>%
    {suppressMessages(padr::pad(., by = "date_time", interval = "hour"))} %>%
    dplyr::mutate(
      id = !!id,
      date = lubridate::date(date_time),
      hour = lubridate::hour(date_time)
    ) %>%
    dplyr::select(id, dplyr::everything(), -date_time) %>%
    dplyr::as_data_frame()

  if (small) {
    table <- dplyr::select(
      table,
      -dplyr::ends_with("_ins")
    )
  }

  table
}

# library(magrittr)
# library(inmetdown)
# source("R/check_date.R")
# load("R/sysdata.rda")
# id = "A137" # id = "A104"
#
# start =  Sys.Date()
# end =  Sys.Date()
# small = TRUE
#
# aws_import2( "A303",Sys.Date(), Sys.Date() )
