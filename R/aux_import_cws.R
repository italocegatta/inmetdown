get_table_bdmep <- function(id, start, end, n_row, token) {

  start_f = format(start, "%d/%m/%Y")
  end_f = format(end, "%d/%m/%Y")

  con <- con_bdmep()

  param <- glue::glue(
    "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao={id}&btnProcesso=serie&mRelDtInicio={start_f}&mRelDtFim={end_f}&mAtributos=,,1,1,,,,,,1,1,,,,1,1,"
  )

  txt <- get_txt_bdmep(con, param, token)

  aux_error <- stringr::str_detect(txt, "Não existem dados disponiveis da estação")

  table <- try(read.csv(text = txt, header = T, sep = ";"), silent = TRUE)


  if (inherits(table, "try-error") | aux_error) {
    table <- as.data.frame(matrix(NA_real_, nrow = n_row, ncol = 9))
    table[ , 1] <- id
    table[ , 2] <- rep(seq(start, end, by = "day"), each = 2)
    table[ , 3] <- c(0, 12) * 100

    if (end == Sys.Date()) {
      table <- table[-nrow(table), ]
    }
  } else {
    table$X <- NULL

    table <- tibble::as_tibble(table)
  }

  dplyr::as_tibble(table)
}

con_bdmep <- function() {
  session <- rvest::html_session(
    "http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php"
  )

  form <- rvest::set_values(
    rvest::html_form(session)[[1]],
    mCod = "repunuwof@golemico.com",
    mSenha = "qs7jsxwr"
  )

  suppressMessages(rvest::submit_form(session, form))
}

get_txt_bdmep <- function(con, param, token) {
  rvest::jump_to(con, param) %>%
    rvest::html_nodes(xpath = "//pre") %>%
    rvest::html_text() %>%
    gsub(token, "", .)
}
