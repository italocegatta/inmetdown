#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
cws_import <- function(id, start = Sys.Date(), end = Sys.Date()) {

  start <- check_date(start)
  end <- check_date(end)

  n_row <- as.numeric(end - start + 1) * 3
  t <- lubridate::hour(lubridate::now("UTC"))

  stations <- cws_station() %>%
    dplyr::filter(id %in% !!id)

  seq <- seq_along(stations$id)
  out <- vector("list", length(seq))
  for (i in seq) {

    session <- suppressWarnings(rvest::html_session(stations$url[i]))

    form <- get_form(session, start, end)

    data <- get_data(session, form)

    nodes_table  <- try(rvest::html_nodes(data, "table")[[7]], silent = TRUE)

    table <- get_table_cws(nodes_table, start, end, n_row)

    names(table) <- c(
      "date", "hour",
      "t", "rh", "ap",
      "ws", "wd",
      "neb","ins",
      "t_max", "t_min",
      "prec"
    )

    table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hour:prec), as.double))

    table <- table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        date = as.Date(ifelse(is.character(date), lubridate::dmy(date), date),  origin = "1970-01-01"),
        date_time = lubridate::ymd_hms(paste0(date, "-", hour, ":0:0"))
      ) %>%
      dplyr::ungroup()

    if (nrow(table) != as.numeric(end - start + 1) * 3) { # ponto fragil. deve testar para o hora final nas etapas 0, 12 e 18h
      range_dttm <- range(table$date_time)
      seq_dttm <- data.frame(date_time = seq.POSIXt(range_dttm[1], range_dttm[2], 'hour')) %>%
        dplyr::filter(lubridate::hour(date_time) %in% c(0, 12, 18))

      table <- dplyr::full_join(table, seq_dttm, by = "date_time")
    }

    out[[i]] <- dplyr::mutate(
      table,
      id = stations$id[i],
      date = lubridate::date(date_time),
      hour = lubridate::hour(date_time)
    ) %>%
      dplyr::select(id, dplyr::everything(), -date_time) %>%
      dplyr::arrange(id, date, hour) %>%
      dplyr::as_data_frame()
  }

  dplyr::bind_rows(out)
}
