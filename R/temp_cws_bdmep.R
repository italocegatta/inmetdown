get_cws_bdmep <- function(id, start, end) {

  #start <- check_date(start)
  #end <- check_date(end)

  s1 <- rvest::html_session(
    "http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php"
  )

  f1 <- rvest::set_values(
    rvest::html_form(s1)[[1]],
    mCod = "repunuwof@golemico.com",
    mSenha = "qs7jsxwr"
  )

  s2 <- suppressMessages(rvest::submit_form(s1, f1))

  f2 <- "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=82191&btnProcesso=serie&mRelDtInicio=01/12/2016&mRelDtFim=31/12/2016&mAtributos=,,1,1,,,,,,1,1,,1,1,1,1,"

  s3 <- rvest::jump_to(s2, f2)

  table <- s3 %>%  rvest::html_nodes(xpath = "//pre") %>%
    rvest::html_text() %>%
    gsub("^.+instruções\n--------------------\n", "", .) %>%
    read.csv(text = ., header = T, sep = ";") %>%
    dplyr::select(-X) %>%
    tibble::as_tibble()

  names(table) <- c(
    "id",
    "date", "hour",
    "prec",
    "t_max", "t_min",
    "inso", "evap",
    "t_med", "rh", "ws"
  )

  table <- dplyr::summarise_all(
    dplyr::group_by(table, id, date),
    mean, na.rm =TRUE
  )
}
