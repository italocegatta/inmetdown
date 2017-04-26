
get_cws <- function(x, id, start, end, small) {

  # debug
  x = "cws"
  id =
  start =  as.Date("2017-02-16")
  end =  as.Date("2017-04-17")
  load("R/sysdata.rda")

  start <- check_date(start)
  end <- check_date(end)

  session <- rvest::html_session(get_url(x, id))

  nodes_img <- rvest::html_nodes(session, "img")

  img_cript <- rvest::html_attr(nodes_img, 'src')

  code_cript <- stringr::str_extract(img_cript, "(?<==)(.*?)(?==)")

  p1 <- key[key$code == stringr::str_sub(code_cript, 1, 3), "key"][[1]]
  p2 <- key[key$code == stringr::str_sub(code_cript, 4, 6), "key"][[1]]

  form <- rvest::set_values(
    rvest::html_form(session)[[1]],
    `dtaini` = format(start, "%d/%m/%Y"),
    `dtafim` = format(end, "%d/%m/%Y"),
    `aleaNum` = paste0(p1, p2)
  )

  x <- 0
  repeat {
    data <- tryCatch(
      {
        suppressMessages(rvest::submit_form(session, form))
      },
      error=function(e) NULL,
      warning=function(w) NULL
      )

    x <- x + 1
    if (!is.null(data)) break
    if (x > 2) break
  }

  nodes_table <- rvest::html_nodes(data, "table")[[6]]

  table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]

  if (is.null(table)) {
    table <- as.data.frame(matrix(NA_real_, nrow = 2, ncol = 19))
    table[, 1] <- c(start, end)
    table[, 2] <- c(0, 23)
  }

  names(table) <- c(
    "data",
    "hora",
    "t_ins",  "t_max", "t_min",
    "ur_ins", "ur_max", "ur_min",
    "pto_orv_ins", "pto_orv_max", "pto_orv_min",
    "pa_int", "pa_max", "pa_min",
    "v_vel", "ento_dir", "v_raj",
    "rad",
    "p"
  )

  table <- dplyr::mutate_at(table, dplyr::vars(hora:p), as.double)

  table <- dplyr::mutate(
    table,
    data = if (is.character(data)) {
      lubridate::dmy_hms(paste(data, paste0(hora, ":0:0")))
    } else {
      lubridate::ymd_hms(paste(data, paste0(hora, ":0:0")))
    }
    ,
    rad = ifelse(rad < 0, NA_real_, rad) / 1000
  )

  table <- padr::pad(table)

  table <- dplyr::mutate(table, id = id)

  table <- dplyr::select(table, id, dplyr::everything(), -hora)

  z <- dplyr::as_data_frame(table)

  if (small) {
    z <- z %>%
      dplyr::select(id, data, t_max, t_min, ur_max, ur_min, rad, p)
  }

  return(z)
}


