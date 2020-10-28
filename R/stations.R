#' INMET Weather Stations
#'
#' @export
#'
inmet_station <- function() {

  httr::GET("https://mapas.inmet.gov.br/assets/js/estacao.js") %>%
    httr::content(type = "text", encoding = "UTF-8") %>%
     stringr::str_remove("var estacao = \n") %>%
    jsonlite::fromJSON() %>%
    '[['("features") %>%
    '[['("properties") %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(c(VL_LATITUDE, VL_LONGITUDE, VL_ALTITUDE), as.numeric),
      DT_INICIO_OPERACAO = as.Date(lubridate::ymd_hms(DT_INICIO_OPERACAO)),
      CD_ESTACAO_DESC = stringr::str_extract(CD_ESTACAO, "^.+(?= -)"),
      CD_ESTACAO = stringr::str_trim(stringr::str_sub(CD_ESTACAO, -5, -1))
    ) %>%
    dplyr::select(
      CD_ESTACAO,
      CD_ESTACAO_DESC,
      CD_WMO,
      CD_CATEGORIA = categoria,
      VL_LATITUDE,
      VL_LONGITUDE,
      VL_ALTITUDE,
      DT_INICIO_OPERACAO
    ) %>%
    unique()

}

# API antiga
# inmet_estacoes <- function(proxy = ".", estacoes.br = TRUE) {
#
#   sonabra_auto_raw <- "http://www.inmet.gov.br/sonabra/maps/pg_mapa.php" %>%
#     rvest::html_session(proxy) %>%
#     xml2::read_html() %>%
#     rvest::html_nodes(xpath = "/html/head/script[2]/text()") %>%
#     rvest::html_text() %>%
#     stringr::str_split("\\/\\/\\*", simplify = TRUE) %>%
#     '['(-1)
#
#   sonabra_auto_df <- dplyr::data_frame(sonabra_auto_raw = list(sonabra_auto_raw)) %>%
#     dplyr::mutate(
#       id = purrr::map(sonabra_auto_raw, ~stringr::str_match(.x, "\\*\\* ESTACÃO (.*?) \\*\\*")[,2]),
#       estado = purrr::map(sonabra_auto_raw, ~stringr::str_sub(gsub(".*label = '|';.*", "", .x), 1, 2)),
#       municipio = purrr::map(sonabra_auto_raw, ~stringr::str_extract(gsub(".*<b>Estação: |<br>.*", "", .x), ".*(?=-)")),
#       lat = purrr::map(sonabra_auto_raw, ~readr::parse_number(gsub(",", ".", gsub(".*Latitude: |º<br>.*", "", .x)))),
#       lon = purrr::map(sonabra_auto_raw, ~readr::parse_number(gsub(",", ".", gsub(".*Longitude: |º<br>.*", "", .x)))),
#       altitude = purrr::map(sonabra_auto_raw, ~readr::parse_number(gsub(",", ".", gsub(".*Altitude: | metros.*", "", .x)))),
#       inicio = purrr::map(sonabra_auto_raw, ~lubridate::dmy(gsub(".*Aberta em: |<br>.*", "", .x))),
#       url = purrr::map(sonabra_auto_raw, ~gsub(".*width=50><a href=| target=_new>.*",  "", .x))
#     ) %>%
#     tidyr::unnest(id, estado, municipio, lon, lat, altitude, inicio, url) %>%
#     dplyr::mutate(tipo = "Automática") %>%
#     dplyr::select(id, tipo, dplyr::everything()) %>%
#     {if (isTRUE(estacoes.br)) dplyr::filter(., !stringr::str_detect(id, "[UC]")) else .}
#
#   sonabra_conv_raw <- "http://www.inmet.gov.br/sim/sonabra/index.php" %>%
#     rvest::html_session(proxy) %>%
#     xml2::read_html() %>%
#     rvest::html_nodes(xpath = "/html/head/script[2]/text()") %>%
#     rvest::html_text() %>%
#     stringr::str_split("\\/\\/\\*", simplify = TRUE) %>%
#     '['(-1)
#
#   sonabra_conv_df <- dplyr::data_frame(sonabra_conv_raw = list(sonabra_conv_raw)) %>%
#     dplyr::mutate(
#       id = purrr::map(sonabra_conv_raw, ~gsub(".*OMM: |<br>.*", "", .x)),
#       estado = purrr::map(sonabra_conv_raw, ~stringr::str_sub(gsub(".*label = '|';.*", "", .x), 1, 2)),
#       municipio = purrr::map(sonabra_conv_raw, ~stringr::str_extract(gsub(".*<b>Estação: |<br>.*", "", .x), ".*(?=-)")),
#       lat = purrr::map(sonabra_conv_raw, ~readr::parse_number(gsub(",", ".", gsub(".*Latitude: |º<br>.*", "", .x)))),
#       lon = purrr::map(sonabra_conv_raw, ~readr::parse_number(gsub(",", ".", gsub(".*Longitude: |º<br>.*", "", .x)))),
#       altitude = purrr::map(sonabra_conv_raw, ~readr::parse_number(gsub(",", ".", gsub(".*Altitude: | metros.*", "", .x)))),
#       inicio = purrr::map(sonabra_conv_raw, ~lubridate::dmy(gsub(".*Aberta em: |<br>.*", "", .x))),
#       url = purrr::map(sonabra_conv_raw, ~gsub(".*<br><a href=|= target=_new>.*",  "", .x))
#     ) %>%
#     tidyr::unnest(id, estado, municipio, lon, lat, altitude, inicio, url) %>%
#     dplyr::mutate(tipo = "Convencional") %>%
#     dplyr::select(id, tipo, dplyr::everything())
#
#   dplyr::bind_rows(sonabra_auto_df, sonabra_conv_df)
# }
