#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
cws_import <- function(id, start = Sys.Date(), end = Sys.Date()) {
  purrr::map_df(
    id, ~get_cws(.x, start = start, end = end)
  )
}

get_cws <- function(id, start, end) {

  start <- check_date(start)
  end <- check_date(end)
  n_row <- as.numeric(end - start + 1) * 3
  t <- lubridate::hour(lubridate::now("UTC"))

  session <- suppressWarnings(
    rvest::html_session(get_url("cws", id))
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
      error = function(e) NULL,
      warning = function(w) NULL
      )

    x <- x + 1
    if (!is.null(data)) break
    if (x > 2) break
  }

  nodes_table  <- try(rvest::html_nodes(data, "table")[[7]], silent = TRUE)

  if (inherits(nodes_table, "try-error")) {
    table <- as.data.frame(matrix(NA_real_, nrow = n_row, ncol = 12))
    table[ , 1] <- rep(seq(start, end, by = "day"), each = 3)
    table[ , 2] <- c(0, 12, 18)

    if (end == Sys.Date()) {
      if (t < 13) {
        table <- table[-c(nrow(table),nrow(table)-1), ]
      } else if (t < 19) {
        table <- table[-nrow(table), ]
      }
    }
  } else {
    table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
  }

  names(table) <- c(
    "date", "hour",
    "t", "rh", "ap",
    "ws", "wd",
    "neb","ins",
    "t_max", "t_min",
    "prec"
  )

  table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hour:prec), as.double))

  table <- dplyr::mutate(
    table,
    date = if (is.character(date)) {
      lubridate::dmy_hms(paste(date, paste0(hour, ":0:0")))
    } else {
      lubridate::ymd_hms(paste(date, paste0(hour, ":0:0")))
    }
  )

  if (nrow(table) != as.numeric(end - start + 1) * 3) {
    table <- padr::pad(table)

    table <- table[lubridate::hour(table$date) %in% c(0, 12, 18), ]
  }

  table <- dplyr::mutate(table, id = id)

  table <- dplyr::select(table, id, dplyr::everything(), -hour)

  z <- dplyr::as_data_frame(table)

  return(z)
}
