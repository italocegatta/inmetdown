import_bdmep <- function(id, start, end, stations, proxy) {

  n_row <- as.numeric(end - start + 1) * 2

  if (is.null(stations)) {
    stations <- cws_station() %>%
      dplyr::filter(id %in% !!id)
  } else {
    stations <- stations %>%
      dplyr::filter(id %in% !!id)
  }

  seq <- seq_along(id)
  out <- vector("list", length(seq))

  for (i in seq) {

    table <- get_table_bdmep(id[i], start, end, n_row, "^.+instruções\n--------------------\n", proxy)

    table <- table %>%
      dplyr::select(
        id = Estacao,
        data = Data,
        hora = Hora,
        ppt = Precipitacao,
        t_bs = TempBulboSeco,
        t_bu = TempBulboUmido,
        t_max = TempMaxima,
        t_med = Temp.Comp.Media,
        t_min = TempMinima,
        ur_med = UmidadeRelativa,
        ur_med_comp = Umidade.Relativa.Media,
        pa_med = PressaoAtmEstacao,
        pa_mar = PressaoAtmMar,
        v_dir = DirecaoVento,
        v_med = VelocidadeVento,
        v_med_comp = Velocidade.do.Vento.Media,
        ins = Insolacao,
        neb = Nebulosidade,
        evp_piche = Evaporacao.Piche
      )

    table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(-id, -data), as.double))

    table <- table %>%
      dplyr::mutate(id = as.character(id)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        data = as.Date(ifelse(lubridate::is.Date(data), data, lubridate::dmy(data)),  origin = "1970-01-01"),
        date_time = lubridate::ymd_h(paste(data, stringr::str_pad(hora / 100, 2, pad = "0")))
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
        id = id[i],
        data = lubridate::date(date_time)
      ) %>%
      dplyr::select(-date_time) %>%
      dplyr::mutate_if(is.double, round, digits = 4) %>%
      dplyr::arrange(id, data, hora)

      # dplyr::group_by(id, data) %>%
      # dplyr::summarise(
      #   ppt = mean(ppt, na.rm = TRUE),
      #   t_min = mean(t_min, na.rm = TRUE),
      #   t_max = mean(t_max, na.rm = TRUE),
      #   ur_med = mean(ur_med, na.rm = TRUE),
      #   pa_med = mean(pa_med, na.rm = TRUE),
      #   v_dir = mean(v_dir, na.rm = TRUE),
      #   v_med = mean(v_med, na.rm = TRUE),
      #   ins = mean(ins, na.rm = TRUE)
      # ) %>%
      # dplyr::mutate_if(is.double, round, digits = 1) %>%
      # dplyr::rowwise() %>%
      # dplyr::mutate(t_med = (t_max + t_min) / 2) %>%
      # dplyr::ungroup() %>%
      # tidyr::replace_na(list(
      #   ppt = NA, t_med = NA, t_min = NA, t_max = NA,
      #   ur_med = NA, pa_med = NA,
      #   v_med = NA, v_dir = NA, ins = NA
      # )) %>%
      # dplyr::select(
      #   id, data,
      #   ppt,
      #   t_max, t_med, t_min,
      #   ur_med, pa_med,
      #   ins,
      #   v_dir, v_med
      # ) %>%
      # dplyr::arrange(id, data)
  }

  dplyr::bind_rows(out)
}
