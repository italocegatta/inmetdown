#' Get basic information about INMET's conventional weather station
#'
#' @export
#'
cws_station <- function(proxy = ".") {

  txt_split <- "http://www.inmet.gov.br/sim/sonabra/index.php" %>%
    rvest::html_session(proxy) %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = "/html/head/script[2]/text()") %>%
    rvest::html_text() %>%
    stringr::str_split("\\/\\/\\*", simplify = TRUE) %>%
    '['(-1)

  id <- gsub(".*OMM: |<br>.*", "", txt_split)
  estado <- stringr::str_sub(gsub(".*label = '|';.*", "", txt_split), 1, 2)
  municipio <- stringr::str_extract(gsub(".*<b>Estação: |<br>.*", "", txt_split), ".*(?=-)")
  lat <- as.numeric(gsub(".*Latitude: |º<br>.*", "", txt_split))
  lon <- as.numeric(gsub(".*Longitude: |º<br>.*", "", txt_split))
  alt <- readr::parse_number(gsub(".*Altitude: | metros.*", "", txt_split))
  inicio <- lubridate::dmy(gsub(".*Aberta em: |<br>.*", "", txt_split))
  url <- gsub(".*<br><a href=|= target=_new>.*",  "", txt_split)

  dplyr::data_frame(
    id, estado, municipio, lon, lat, alt, inicio, url
  )
}
