#' Download de dados de estacoes convencionais no portal BDMEP
#'
#' @export
#'
inmet_download_bdmep_convencional <- function(id, inicio, fim, estacoes = NULL, proxy = ".") {

  inicio <- check_date(inicio)
  fim <- check_date(fim)

  n_row <- as.numeric(fim - inicio + 1) * 3

  if (is.null(estacoes)) {
    estacoes <- inmet_estacoes(proxy) %>%
      dplyr::filter(id %in% !!id)
  } else {
    estacoes <- estacoes %>%
      dplyr::filter(id %in% !!id)
  }

  seq <- seq_along(id)
  out <- vector("list", length(seq))

  for (i in seq) {

    table <- get_table_bdmep(id[i], inicio, fim, n_row, "^.+instruções\n--------------------\n", proxy)

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
        date_time = lubridate::ymd_h(paste(data, stringr::str_pad(hora, 2, pad = "0")))
      ) %>%
      dplyr::ungroup()

    if (nrow(table) != as.numeric(fim - inicio + 1) * 3) { # ponto fragil. deve testar para o hora final nas etapas 0, 12 e 18h

      range_dttm <- range(table$date_time)
      seq_dttm <- data.frame(date_time = seq.POSIXt(range_dttm[1], range_dttm[2], 'hour')) %>%
        dplyr::filter(lubridate::hour(date_time) %in% c(0, 12, 18)) %>%
        dplyr::mutate(
          id = id[i],
          data = as.Date(date_time),
          hora = lubridate::hour(date_time)
        )

      table <- dplyr::full_join(table, seq_dttm, by = c("id", "data", "hora", "date_time"))
    }

    out[[i]] <- table %>%
      dplyr::mutate(
        id = id[i],
        data = lubridate::date(date_time)
      ) %>%
      dplyr::select(-date_time) %>%
      dplyr::mutate_if(is.double, round, digits = 4) %>%
      dplyr::arrange(id, data, hora)
  }

  dplyr::bind_rows(out)
}
