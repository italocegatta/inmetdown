cws_sonabra <- function(id, start, end) {

  id <- dplyr::enquo(id)

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

    out[[i]] <- table %>%
      dplyr::mutate(
        id = stations$id[i],
        date = lubridate::date(date_time)
      ) %>%
      dplyr::group_by(id, date) %>%
      dplyr::summarise(
        prec = mean(prec, na.rm = TRUE),
        t = mean(t, na.rm = TRUE),
        t_min = mean(t_min, na.rm = TRUE),
        t_max = mean(t_max, na.rm = TRUE),
        rh = mean(rh, na.rm = TRUE),
        ws = mean(ws, na.rm = TRUE),
        ins = mean(ins, na.rm = TRUE)
      ) %>%
      dplyr::mutate_if(is.double, round, digits = 1) %>%
      tidyr::replace_na(list(
        prec = NA, t = NA, t_min = NA,
        t_max = NA, rh = NA,
        ws = NA, ins = NA
      )) %>%
      dplyr::arrange(id, date) %>%
      dplyr::as_data_frame()
  }

  dplyr::bind_rows(out)
}
