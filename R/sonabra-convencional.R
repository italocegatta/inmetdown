#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
inmet_download_sonabra_convencional <- function(id, inicio, fim, estacoes = NULL, proxy = ".") {

  inicio <- check_date(inicio)
  fim <- check_date(fim)

  if (inicio > fim) {
    stop("Data de inicio deve ser anterior a data de fim")
  }

  if (Sys.Date() - inicio > 90) {
    stop("Dados disponiveis apenas para os ultimos 90 dias")
  }

  n_row <- as.numeric(fim - inicio + 1) * 3
  t <- lubridate::hour(lubridate::now("UTC"))

  if (is.null(estacoes)) {
    estacoes <- inmet_estacoes(proxy) %>%
      dplyr::filter(id %in% !!id)
  } else {
    estacoes <- estacoes %>%
      dplyr::filter(id %in% !!id)
  }

  seq <- seq_along(estacoes$id)
  out <- vector("list", length(seq))

  for (i in seq) {

    session <- suppressWarnings(rvest::html_session(estacoes$url[i], proxy))

    form <- get_form(session, inicio, fim)

    data <- get_data(session, form)

    nodes_table <- try(rvest::html_nodes(data, "table")[[7]], silent = TRUE)

    if (inherits(nodes_table, "try-error")) {
      table <- as.data.frame(matrix(NA_real_, nrow = n_row, ncol = 12))
      table[ , 1] <- rep(seq(inicio, fim, by = "day"), each = 3)
      table[ , 2] <- c(0, 12, 18)

      if (fim == Sys.Date()) {
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
      "data", "hora",
      "t_med", "ur_med", "pa_med",
      "v_med", "v_dir",
      "neb","ins",
      "t_max", "t_min",
      "ppt"
    )

    table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hora:ppt), as.double))

    table <- table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        data = as.Date(ifelse(is.character(data), lubridate::dmy(data), data),  origin = "1970-01-01"),
        date_time = lubridate::ymd_hms(paste0(data, "-", hora, "-00-00"))
      ) %>%
      dplyr::ungroup()

    if (nrow(table) != as.numeric(fim - inicio + 1) * 3) {

      range_dttm <- lubridate::ymd_hms(paste0(c(inicio, fim), "-", "00-00-0"))

      if (range_dttm[2] == Sys.Date()) {
        lubridate::hour(range_dttm[2]) <- lubridate::hour(lubridate::now(tzone = "UTC"))
      }

      seq_dttm <- data.frame(date_time = seq.POSIXt(range_dttm[1], range_dttm[2], 'hour')) %>%
        dplyr::filter(lubridate::hour(date_time) %in% c(0, 12, 18))

      table <- dplyr::full_join(table, seq_dttm, by = "date_time")
    }

    out[[i]] <- table %>%
      dplyr::mutate(
        id = estacoes$id[i],
        data = lubridate::date(date_time)
      ) %>%
      dplyr::select(
        id, data, hora,
        ppt, t_max, t_med, t_min,
        ur_med, pa_med, v_dir, v_med, ins, neb
      ) %>%
      dplyr::arrange(id, data) %>%
      dplyr::as_tibble()
  }

  dplyr::bind_rows(out)
}
