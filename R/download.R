#' Download de dados INMET
#' Importa uma série de dados bruta do INMET de uma estação a partir do período
#' especificado

#' @export
#'

inmet_download <- function(inicio, fim, estacao) {

  sessao <- estacao %>%
    rvest::html_session()

  imgsrc <- sessao %>%
    rvest::html_nodes("img") %>%
    rvest::html_attr('src')

  codigo <- imgsrc %>% stringr::str_extract("(?<==)(.*?)(?==)")

  p1 <- solucao[solucao$codigo == stringr::str_sub(codigo, 1, 3), "solucao"][[1]]
  p2 <- solucao[solucao$codigo == stringr::str_sub(codigo, 4, 6), "solucao"][[1]]

  formulario <- rvest::set_values(
    rvest::html_form(sessao)[[1]],
    `dtaini` = inicio,
    `dtafim` = fim,
    `aleaNum` = paste0(p1, p2)
  )

  dados <-  suppressMessages(rvest::submit_form(sessao, formulario))

  tabela <- dados %>%
    rvest::html_nodes("table") %>%
    `[[`(6) %>% rvest::html_table(header = TRUE) %>%
    `[`(-1, )

  names(tabela) <- c(
    "Data",
    "Hora",
    "Temperatura_ins",  "Temperatura_max", "Temperatura_min",
    "Umidade_ins", "Umidade_max", "Umidade_min",
    "Pto_Orvalho_ins", "Pto_Orvalho_max", "Pto_Orvalho_min",
    "Pressão_int", "Pressão_max", "Pressão_min",
    "Vento_vel", "Vento_dir", "Vento_raj",
    "Radiação",
    "Chuva"
  )

  return(tabela)
}
