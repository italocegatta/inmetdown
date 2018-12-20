#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
inmet_download_sonabra_automatica <- function(id, inicio, fim, estacoes = NULL, proxy = ".") {

  id_filter <- id

  inicio <- check_date(inicio)
  fim <- check_date(fim)

  if (inicio > fim) {
    stop("Data de inicio deve ser anterior a data de fim")
  }

  if (Sys.Date() - inicio > 365) {
    stop("Dados disponiveis apenas para os ultimos 365 dias")
  }

  end_hour <- ifelse(
    fim == Sys.Date(),
    lubridate::hour(lubridate::now("UTC")) - 1,
    23
  )
  end_date_time <- lubridate::ymd_hms(paste0(fim, "-", end_hour, ":0:0"))

  if (is.null(estacoes)) {
    estacoes <- inmet_estacoes(proxy) %>%
      dplyr::filter(id %in% !!id)
  } else {
    estacoes <- estacoes %>%
      dplyr::filter(id %in% id_filter)
  }

  seq <- seq_along(estacoes$id)
  out <- vector("list", length(seq))

  for (i in seq) {

    session <- suppressWarnings(rvest::html_session(estacoes$url[i], proxy))

    form <- get_form(session, inicio, fim)

    data <- get_data(session, form)

    nodes_table  <- try(rvest::html_nodes(data, "table")[[6]], silent = TRUE)

    if (inherits(nodes_table, "try-error")) {
      table <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = 19))
      table[, 1] <- c(inicio, fim)

      if (fim == Sys.Date()) {
        table[, 2] <- c(0, end_hour)
      } else {
        table[, 2] <- c(0, 23)
      }

    } else {
      table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
    }

    names(table) <- c(
      "data",
      "hora",
      "t_ins",  "t_max", "t_min",
      "ur_ins", "ur_max", "ur_min",
      "pto_ins", "pto_max", "pto_min",
      "pa_ins", "pa_max", "pa_min",
      "v_med", "v_dir", "v_max",
      "rad",
      "ppt"
    )

    table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hora:ppt), as.double))

    table <- table %>%
      dplyr::filter(!is.na(hora)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        data = as.Date(ifelse(is.character(data), lubridate::dmy(data), data),  origin = "1970-01-01"),
        rad = ifelse(rad < 0, NA_real_, rad) / 1000,
        date_time = lubridate::ymd_hms(paste0(data, "-", hora, ":0:0")),
        t_med = mean(c(t_min, t_max), na.rm = TRUE),
        ur_med = mean(c(ur_min, ur_max), na.rm = TRUE),
        pto_med = mean(c(pto_min, pto_max), na.rm = TRUE),
        pa_med = mean(c(pa_min, pa_max), na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    if (max(table$date_time) < end_date_time) {
      table <- dplyr::add_row(table,  date_time = end_date_time)
    }

    range_dttm <- range(table$date_time)
    seq_dttm <- data.frame(date_time = seq.POSIXt(range_dttm[1], range_dttm[2], 'hour'))

    table <-  table %>%
      dplyr::full_join(seq_dttm, by = "date_time") %>%
      dplyr::mutate(
        id = estacoes$id[i],
        data = lubridate::date(date_time),
        hora = lubridate::hour(date_time)
      ) %>%
      dplyr::select(
        -dplyr::ends_with("_ins"),
        -date_time
      ) %>%
      tidyr::replace_na(list(
        t_med = NA, ur_med = NA,
        pto_med = NA, pa_med = NA
      )) %>%
      dplyr::select(
        id, data, hora,
        ppt,
        t_min, t_med, t_max,
        ur_max, ur_med, ur_min,
        pa_max, pa_med, pa_min,
        rad,
        pto_max, pto_med, pto_min,
        v_dir, v_max, v_med
      ) %>%
      dplyr::mutate_if(is.double, round, digits = 1) %>%
      dplyr::arrange(id, data, hora) %>%
      dplyr::as_data_frame()

    out[[i]] <- table
  }

  dplyr::bind_rows(out)
}
