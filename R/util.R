# Parse and chek date
check_date <- function(x) {

  if (!lubridate::is.Date(x)) {
    test1 <- tryCatch(lubridate::dmy(x), warning=function(w) w)
    if (!any((class(test1) == "warning") == TRUE)) {
      z <- test1
    } else {
      test2 <- tryCatch(lubridate::ymd(x), warning=function(w) w)
      if (lubridate::is.Date(test2)) {
        z <- test2
      } else {
        stop("All formats failed to parse to date. No formats found.")
      }
    }
  } else {
    z <- x
  }

  z
}

split_dates <- function(start, end, min = 90) {

  seq <- seq.Date(start, end, '1 day')

  dummy <- (seq < (lubridate::today("GMT") - min))

  if (length(seq[dummy]) != 0) {
    bdmep <- range(seq[dummy])
  } else {
    bdmep <- TRUE
  }

  if (length(seq[!dummy]) != 0) {
    sonabra <- range(seq[!dummy])
  } else {
    sonabra <- TRUE
  }

  list(bdmep = bdmep, sonabra = sonabra)
}

get_form <- function(session, start, end) {

  code_cript <- session %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr('src') %>%
    stringr::str_extract("(?<==)(.*?)(?==)")

  p1 <- key[key$code == stringr::str_sub(code_cript, 1, 3), "key"][[1]]
  p2 <- key[key$code == stringr::str_sub(code_cript, 4, 6), "key"][[1]]

  form <- rvest::set_values(
    rvest::html_form(session)[[1]],
    `dtaini` = format(start, "%d/%m/%Y"),
    `dtafim` = format(end, "%d/%m/%Y"),
    `aleaNum` = paste0(p1, p2)
  )
}

get_data <- function(session, form) {

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
    if (x > 15) break
  }

  data
}

get_table_cws <- function(nodes_table, start, end, n_row) {

  if (inherits(nodes_table, "try-error")) {
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
    table <- rvest::html_table(nodes_table, header = TRUE)[-1, ]
  }

  table
}

get_table_bdmep <- function(id, start, end, n_row, token, proxy) {

  start_f = format(start, "%d/%m/%Y")
  end_f = format(end, "%d/%m/%Y")

  con <- con_bdmep(proxy)

  param <- glue::glue(
    "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao={id}&btnProcesso=serie&mRelDtInicio={end_f}&mRelDtFim={start_f}&mAtributos=1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,"
  )

  txt <- try(get_txt_bdmep(con, param, token), silent = TRUE)

  aux_error <- stringr::str_detect(txt, "Não.existem.dados.disponiveis.da.estação")

  if (inherits(txt, "try-error") | aux_error) {

    table <- table_if_error(id, start, end, n_row)
    return(dplyr::as_tibble(table))

  } else {

    table <- try(
      txt %>%
        stringr::str_replace("VelocidadeVentoInsolacao", "VelocidadeVento;Insolacao") %>%
        read.csv(text = ., header = T, sep = ";"),
      silent = TRUE
    )

    if (inherits(txt, "try-error") | aux_error) {

      table <- table_if_error(id, start, end, n_row)
      return(dplyr::as_tibble(table))

    }

    table$X <- NULL
    table <- tibble::as_tibble(table)
  }

  dplyr::as_tibble(table)
}

con_bdmep <- function(proxy) {

  session <- rvest::html_session(
    "http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php",
    proxy
  )

  form <- rvest::set_values(
    rvest::html_form(session)[[1]],
    mCod = "repunuwof@golemico.com",
    mSenha = "qs7jsxwr"
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
    if (x > 15) break
  }

  data
}

get_txt_bdmep <- function(con, param, token) {

  rvest::jump_to(con, param) %>%
    rvest::html_nodes(xpath = "//pre") %>%
    rvest::html_text() %>%
    gsub(token, "", .)
}

table_if_error <- function(id, start, end, n_row) {

  table <- as.data.frame(matrix(NA_real_, nrow = n_row, ncol = 11))
  table[ , 1] <- id
  table[ , 2] <- rep(seq(start, end, by = "day"), each = 2)
  table[ , 3] <- c(0, 12) * 100

  if (end == Sys.Date()) {
    table <- table[-nrow(table), ]
  }
  table
}
