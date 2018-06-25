import_bdmep <- function(id, start, end, proxy) {

  n_row <- as.numeric(end - start + 1) * 2

  seq <- seq_along(id)
  out <- vector("list", length(seq))


  for (i in seq) {
# table <- inmetdown:::get_table_bdmep(id[i], start, end, n_row)
    table <- get_table_bdmep(id[i], start, end, n_row, "^.+instruções\n--------------------\n", proxy)

    names(table) <- c(
      "id",
      "data", "hora",
      "ppt", "t_max", "t_min",
      "ins", "ur", "v_med"
    )

    table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hora:v_med), as.double))

    table <- table %>%
      dplyr::mutate(id = as.character(id)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        data = as.Date(ifelse(lubridate::is.Date(data), data, lubridate::dmy(data)),  origin = "1970-01-01"),
        date_time = lubridate::ymd_h(paste(data, stringr::str_pad(hora / 100, 2, pad = "0")))
      ) %>%
      dplyr::ungroup()

    if (nrow(table) != as.numeric(end - start + 1) * 2) { # ponto fragil. deve testar para o hora final nas etapas 0, 12 e 18h
      range_dttm <- range(table$date_time)
      seq_dttm <- data.frame(date_time = seq.POSIXt(range_dttm[1], range_dttm[2], 'hour')) %>%
        dplyr::filter(lubridate::hour(date_time) %in% c(0, 12))

      table <- dplyr::full_join(table, seq_dttm, by = "date_time")
    }

    out[[i]] <- table %>%
      dplyr::mutate(
        id = id[i],
        data = lubridate::date(date_time)
      ) %>%
      dplyr::group_by(id, data) %>%
      dplyr::summarise(
        ppt = mean(ppt, na.rm = TRUE),
        t_min = mean(t_min, na.rm = TRUE),
        t_max = mean(t_max, na.rm = TRUE),
        ur_med = mean(ur, na.rm = TRUE),
        v_med = mean(v_med, na.rm = TRUE),
        ins = mean(ins, na.rm = TRUE)
      ) %>%
      dplyr::mutate_if(is.double, round, digits = 1) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(t_med = (t_max + t_min) / 2) %>%
      dplyr::ungroup() %>%
      tidyr::replace_na(list(
        ppt = NA, t_med = NA, t_min = NA,
        t_max = NA, ur_med = NA,
        v_med = NA, ins = NA
      )) %>%
      dplyr::select(
        id, data,
        ppt,
        t_max, t_med, t_min,
        ur_med,
        ins,
        v_med
      ) %>%
      dplyr::arrange(id, data)
  }

  dplyr::bind_rows(out)
}



