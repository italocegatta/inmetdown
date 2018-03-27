import_sonabra <- function(id, start, end, stations) {

  n_row <- as.numeric(end - start + 1) * 3
  t <- lubridate::hour(lubridate::now("UTC"))

  if (is.null(stations)) {
    stations <- cws_station() %>%
      dplyr::filter(id %in% !!id)
  } else {
    stations <- stations %>%
      dplyr::filter(id %in% !!id)
  }

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
        date_time = lubridate::ymd_hms(paste0(date, "-", hour, "-00-00"))
      ) %>%
      dplyr::ungroup()

    if (nrow(table) != as.numeric(end - start + 1) * 3) {
      range_dttm <- lubridate::ymd_hms(paste0(c(start, end), "-", "00-00-0"))

      if (range_dttm[2] == Sys.Date()) {
        lubridate::hour(range_dttm[2]) <- lubridate::hour(lubridate::now(tzone = "UTC"))
      }

      seq_dttm <- data.frame(date_time = seq.POSIXt(range_dttm[1], range_dttm[2], 'hour')) %>%
        dplyr::filter(lubridate::hour(date_time) %in% c(0, 12, 18))

      table <- dplyr::full_join(table, seq_dttm, by = "date_time")
    }

    out[[i]] <- table %>%
      dplyr::mutate(
        id = stations$id[i],
        data = lubridate::date(date_time)
      ) %>%
      dplyr::group_by(id, data) %>%
      dplyr::summarise(
        ppt = mean(prec, na.rm = TRUE),
        t_max = mean(t_max, na.rm = TRUE),
        t_med = mean(t, na.rm = TRUE),
        t_min = mean(t_min, na.rm = TRUE),
        ur_med = mean(rh, na.rm = TRUE),
        ins = mean(ins, na.rm = TRUE),
        pa_med = mean(ap, na.rm = TRUE),
        v_dir = mean(wd, na.rm = TRUE),
        v_med = mean(ws, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.double, round, digits = 1) %>%
      tidyr::replace_na(list(
        ppt = NA, t_max = NA, t_med = NA, t_min = NA,
        ur_med = NA, ins = NA, pa_med = NA,
        v_dir = NA, v_med = NA
      )) %>%
      dplyr::arrange(id, data) %>%
      dplyr::as_tibble()
  }

  dplyr::bind_rows(out)
}
