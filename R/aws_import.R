#' Download de dados INMET
#'
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado
#'
#' @export
#'
aws_import <- function(id, start, end, ins = FALSE) {
  ## testar valores unicos
  id = dplyr::enquo(id)

  stations <- aws_station() %>%
    dplyr::filter(id %in% !!id)

  start <- check_date(start)
  end <- check_date(end)
  end_hour <- ifelse(
    end == Sys.Date(),
    lubridate::hour(lubridate::now("UTC")) - 1,
    23
  )
  end_date_time <- lubridate::ymd_hms(paste0(end, "-", end_hour, ":0:0"))

  seq <- seq_along(stations$id)
  out <- vector("list", length(seq))
  for (i in seq) {

    session <- suppressWarnings(rvest::html_session(stations$url[i]))

    form <- get_form(session, start, end)

    data <- get_data(session, form)

    nodes_table  <- try(rvest::html_nodes(data, "table")[[6]], silent = TRUE)

    table <- get_table_aws(nodes_table, start, end, end_hour)

    names(table) <- c(
      "date",
      "hour",
      "t_ins",  "t_max", "t_min",
      "rh_ins", "rh_max", "rh_min",
      "dp_ins", "dp_max", "dp_min",
      "ap_ins", "ap_max", "ap_min",
      "ws", "wd", "wg",
      "rad",
      "prec"
    )

    table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hour:prec), as.double))

    table <- table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        date = as.Date(ifelse(is.character(date), lubridate::dmy(date), date),  origin = "1970-01-01"),
        rad = ifelse(rad < 0, NA_real_, rad) / 1000,
        date_time = lubridate::ymd_hms(paste0(date, "-", hour, ":0:0"))
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
        id = stations$id[i],
        date = lubridate::date(date_time),
        hour = lubridate::hour(date_time)
      ) %>%
      dplyr::select(id, dplyr::everything(), -date_time) %>%
      dplyr::arrange(id, date, hour) %>%
      dplyr::as_data_frame()

    if (!ins) {
      table <- dplyr::select(
        table,
        -dplyr::ends_with("_ins")
      )
    }

    out[[i]] <- table
  }

  dplyr::bind_rows(out)
}
