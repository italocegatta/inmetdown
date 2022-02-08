function (proxy = "") 
{
  tab_estacoes_m <- httr::GET("https://apitempo.inmet.gov.br/estacoes/M") %>% 
    httr::content(type = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() %>% dplyr::as_tibble()
  tab_estacoes_t <- httr::GET("https://apitempo.inmet.gov.br/estacoes/T") %>% 
    httr::content(type = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() %>% dplyr::as_tibble()
  dplyr::bind_rows(tab_estacoes_m, tab_estacoes_t) %>% dplyr::mutate(dplyr::across(c(VL_LATITUDE, 
    VL_LONGITUDE, VL_ALTITUDE), as.numeric), DT_INICIO_OPERACAO = as.Date(lubridate::ymd_hms(DT_INICIO_OPERACAO)), 
    CD_ESTACAO = stringr::str_trim(stringr::str_sub(CD_ESTACAO, 
      -5, -1))) %>% dplyr::select(CD_ESTACAO, DC_NOME, 
    VL_LATITUDE, VL_LONGITUDE, VL_ALTITUDE, DT_INICIO_OPERACAO) %>% 
    unique()
}
