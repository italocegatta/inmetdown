#' Get basic information about INMET's automatic weather station
#'
#' @export
#'
aws_station <- function(proxy = ".", only.br = TRUE) {

  txt_split <- "http://www.inmet.gov.br/sonabra/maps/pg_mapa.php" %>%
    rvest::html_session(proxy) %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = "/html/head/script[2]/text()") %>%
    rvest::html_text() %>%
    stringr::str_split("\\/\\/\\*", simplify = TRUE) %>%
    '['(-1)

  id <- stringr::str_match(txt_split, "\\*\\* ESTACÃO (.*?) \\*\\*")[,2]
  estado <- stringr::str_sub(gsub(".*label = '|';.*", "", txt_split), 1, 2)
  municipio <- stringr::str_extract(gsub(".*<b>Estação: |<br>.*", "", txt_split), ".*(?=-)")
  lat <- gsub(".*Latitude: |º<br>.*", "", txt_split)
  lon <- gsub(".*Longitude: |º<br>.*", "", txt_split)
  alt <- readr::parse_number(gsub(".*Altitude: | metros.*", "", txt_split))
  inicio <- lubridate::dmy(gsub(".*Aberta em: |<br>.*", "", txt_split))
  url <- gsub(".*width=50><a href=| target=_new>.*",  "", txt_split)

  z <- dplyr::data_frame(
    id, estado, municipio, lon, lat, alt, inicio, url
  )

  z[z$id == "A923", "lon"] <- -54.381001 # erro no site

  z <- dplyr::mutate_at(z, dplyr::vars(lon, lat), as.numeric)

  if (isTRUE(only.br)) {
    z <- dplyr::filter(z, !stringr::str_detect(id, "[UC]"))
  }

  z
}
