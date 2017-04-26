#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'

aws_import <- function(id, start, end, small = TRUE) {
  purrr::map_df(
    id, ~get_aws(.x, start = start, end = end, small = small)
  )
}

# Get URL's station
get_url <- function(x, id) {
  df <- get_stations(x = x)

  aux <- df$id %in% id

  if (!any(aux)) stop(sprintf("'%s' station not fund.", id))

  df[aux, "url"][[1]]
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
  } else {
    z <- x
  }

  return(z)
}

get_aws <- function(id, start, end, small) {

  # debug
  # "A108","16/02/17", "17/04/17"
  # x = "aws"
  # id = "A108"
  # start =  Sys.Date() - 5
  # end =  Sys.Date() - 5
  # load("R/sysdata.rda")

  start <- check_date(start)
  end <- check_date(end)

  session <- rvest::html_session(get_url("aws", id))

  nodes_img <- rvest::html_nodes(session, "img")

  img_cript <- rvest::html_attr(nodes_img, 'src')

  code_cript <- stringr::str_extract(img_cript, "(?<==)(.*?)(?==)")

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
    if (x > 2) break
  }

  nodes_table  <- try(rvest::html_nodes(data, "table")[[6]], silent = TRUE)

  if (inherits(nodes_table, "try-error")) {
    table <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = 19))
    table[, 1] <- c(start, end)
    table[, 2] <- c(0, 23) # limitar as linhas até o momento da consulta, não o dia inteiro
  } else {
    table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
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

  table <- dplyr::mutate_at(table, dplyr::vars(hora:p), as.double)

  table <- dplyr::mutate(
    table,
    data = if (is.character(data)) {
      lubridate::dmy_hms(paste(data, paste0(hora, ":0:0")))
    } else {
      lubridate::ymd_hms(paste(data, paste0(hora, ":0:0")))
    }
    ,
    rad = ifelse(rad < 0, NA_real_, rad) / 1000
  )

  table <- padr::pad(table)

  table <- dplyr::mutate(table, id = id)

  table <- dplyr::select(table, id, dplyr::everything(), -hora)

  z <- dplyr::as_data_frame(table)

  if (small) {
    z <- z %>%
      dplyr::select(id, data, t_max, t_min, ur_max, ur_min, rad, p)
  }

  return(z)
}
