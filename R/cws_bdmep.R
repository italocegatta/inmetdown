#' @export
#'
cws_bdmep <- function(id, start, end) {

  id <- dplyr::enquo(id)

  start <- check_date(start)
  end <- check_date(end)
  n_row <- as.numeric(end - start + 1) * 2

  seq <- seq_along(id)
  out <- vector("list", length(seq))
  for (i in seq) {

    table <- get_table_cws_bdmep(id[i], start, end, n_row)

    names(table) <- c(
      "id",
      "date", "hour",
      "prec", "t_max", "t_min",
      "ins", "t", "rh", "ws"
    )

    table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hour:prec), as.double))

    table <- table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        date = as.Date(ifelse(lubridate::is.Date(date), date, lubridate::dmy(date)),  origin = "1970-01-01"),
        date_time = lubridate::ymd_h(paste(date, stringr::str_pad(hour / 100, 2, pad="0")))
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

#' @export
#'
get_table_cws_bdmep <- function(id, start, end, n_row) {

  start_f = format(start, "%d/%m/%Y")
  end_f = format(end, "%d/%m/%Y")

  con <- con_bdmep()

  param <- glue::glue(
    "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao={id}&btnProcesso=serie&mRelDtInicio={start_f}&mRelDtFim={end_f}&mAtributos=,,1,1,,,,,,1,1,,,1,1,1,"
  )

  txt <- rvest::jump_to(con, param) %>%
    rvest::html_nodes(xpath = "//pre") %>%
    rvest::html_text() %>%
    gsub("^.+instruções\n--------------------\n", "", .)

  table <- try(read.csv(text = txt, header = T, sep = ";"), silent = TRUE)


  if (inherits(table, "try-error")) {
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
    table$X <- NULL
    table <- tibble::as_tibble(table)
  }

  table
}

