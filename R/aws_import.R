#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
aws_import <- function(id, start = Sys.Date(), end = Sys.Date(), small = TRUE) {
  purrr::map_df(
    id, ~get_aws(.x, start = start, end = end, small = small)
  )
}

get_aws <- function(id, start, end, small) {

  start <- check_date(start)
  end <- check_date(end)

  if (end == Sys.Date()) {
    end_hour <- lubridate::ymd_hms(
      paste(end, paste0(lubridate::hour(lubridate::now("UTC"))-1, ":0:0"))
    )
  } else {
    end_hour <-  lubridate::ymd_hms(paste(end, paste0(23, ":0:0")))
  }

  session <- suppressWarnings(
    rvest::html_session(get_url("aws", id))
  )

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
    if (x > 15) break
  }

  nodes_table  <- try(rvest::html_nodes(data, "table")[[6]], silent = TRUE)

  if (inherits(nodes_table, "try-error")) {
    table <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = 19))
    table[, 1] <- c(start, end)

    if (end == Sys.Date()) {
      table[, 2] <- c(0, lubridate::hour(end_hour))
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

  table <- suppressWarnings(
    dplyr::mutate_at(table, dplyr::vars(hour:prec), as.double)
  )

  table <- dplyr::mutate(
    table,
    date = if (is.character(date)) {
      lubridate::dmy_hms(paste(date, paste0(hour, ":0:0")))
    } else {
      lubridate::ymd_hms(paste(date, paste0(hour, ":0:0")))
    }
    ,
    rad = ifelse(rad < 0, NA_real_, rad) / 1000
  )

  if (table$date[nrow(table)] != end_hour) {
    table <- dplyr::add_row(table, date = end_hour)
  }

  table <- suppressMessages(padr::pad(table, interval = "hour"))

  table <- dplyr::mutate(table, id = id)

  table <- dplyr::select(table, id, dplyr::everything(), -hour)

  z <- dplyr::as_data_frame(table)

  if (small) {
    z <- dplyr::select(
      z,
      id, date,
      t_max, t_min,
      rh_max, rh_min,
      dp_max, dp_min,
      ap_max, ap_min,
      ws, wg, wd,
      rad,
      prec
    )
  }

  return(z)
}

