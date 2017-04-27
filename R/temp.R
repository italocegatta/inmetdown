
get_cws <- function(id, start, end) {

  # debug
  id = "82067"
  # start =  Sys.Date() - 5
  # end =  Sys.Date()
  # load("R/sysdata.rda")

  start <- check_date(start)
  end <- check_date(end)
  n_row <- as.numeric(end - start + 1) * 3

  session <- rvest::html_session(get_url("cws", id))

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
      error = function(e) NULL,
      warning = function(w) NULL
      )

    x <- x + 1
    if (!is.null(data)) break
    if (x > 2) break
  }

  nodes_table  <- try(rvest::html_nodes(data, "table")[[7]], silent = TRUE)

  if (inherits(nodes_table, "try-error")) {
    table <- as.data.frame(matrix(NA_real_, nrow = n_row, ncol = 12))
    table[ , 1] <- rep(seq(start, end, by = "day"), each = 3)
    table[ , 2] <- c(0, 12, 18)

    if (end == Sys.Date()) {
      ## filtrar hora maior que hora do sistema
      table$V2[(nrow(table)-2:nrow(table))]

      table[nrow(table):(nrow(table)-3), ]

      table[table$V1 == end | table$V1 <= lubridate::hour(Sys.time()), ]
      table[, 2] <- c(0, 12, 18)
    } else {
      table[, 2] <- c(0, 18)
    }

  } else {
    table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
  }

  names(table) <- c(
    "data", "hora",
    "temp", "ur", "pa",
    "v_vel", "v_dir",
    "nebul","insol",
    "t_max", "t_min",
    "p"
  )

  table <- suppressWarnings(dplyr::mutate_at(table, dplyr::vars(hora:p), as.double))

  table <- dplyr::mutate(
    table,
    data = if (is.character(data)) {
      lubridate::dmy_hms(paste(data, paste0(hora, ":0:0")))
    } else {
      lubridate::ymd_hms(paste(data, paste0(hora, ":0:0")))
    }
  )

  if (nrow(table) != as.numeric(end - start + 1) * 3) {
    table <- padr::pad(table)

    table <- table[table$hora %in% c(0, 12, 18), ]
  }

  table <- dplyr::mutate(table, id = id)

  table <- dplyr::select(table, id, dplyr::everything(), -hora)

  z <- dplyr::as_data_frame(table)

  return(z)
}

print(cws_stations(), n= 15)
x <- cws_stations()$id
w <- list()
for (i in x) {
  w[i] <- cws_import(i, Sys.Date()-30, Sys.Date())
}
