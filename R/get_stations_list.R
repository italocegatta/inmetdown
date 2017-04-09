#' Get basic information about INMET's automatic weather station
#'
#' This function searches the stations registered in the INMET database.
#'
#' @export
#'
aws_stations <- function() {
  xml2::read_html("http://www.inmet.gov.br/sonabra/maps/pg_mapa.php") %>%
  rvest::html_nodes(xpath = "/html/head/script[2]/text()") %>%
  rvest::html_text() %>%
  dplyr::as_data_frame() %>%
  tidytext::unnest_tokens(x, value, token = stringr::str_split, pattern = "\\*", to_lower = FALSE) %>%
  '['(c(-1,-2), ) %>%
  dplyr::mutate(
    id = rep(1:(nrow(.) / 2),each = 2),
    key = rep(c("est", "text"), nrow(.) / 2)
  ) %>%
  tidyr::spread(key, x) %>%
  transmute(
    cod = gsub(".*-", "", gsub(".*<b>Estação:</b> |<br>.*", "", text)),
    state = stringr::str_sub(gsub(".*label = '|';.*", "", text), 1, 2),
    city = stringr::str_extract(gsub(".*<b>Estação:</b> |<br>.*", "", text), ".*(?=-)"),
    lat = as.numeric(gsub(".*Latitude: |º<br>.*", "", text)),
    lon = as.numeric(gsub(".*Longitude: |º<br>.*", "", text)),
    alt = readr::parse_number(gsub(".*Altitude: | metros.*", "", text)),
    start = lubridate::dmy(gsub(".*Aberta em: |<br>.*", "", text)),
    status =  ifelse(stringr::str_detect(gsub(".*imagem = 'mm_20_|.png.*", "", text), "amarelo"), "On", "Off"),
    url = gsub(".*width=50><a href=| target=_new>.*",  "", text)
  )
}
