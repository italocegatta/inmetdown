#' Get basic information about INMET's conventional weather station
#'
#' @export
#'
cws_station <- function() {

  df_tidy <- "http://www.inmet.gov.br/sim/sonabra/index.php" %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = "/html/head/script[2]/text()") %>%
    rvest::html_text() %>%
    dplyr::as_data_frame() %>%
    tidytext::unnest_tokens(
      x, value,
      token = stringr::str_split, pattern = "\\*",
      to_lower = FALSE
    ) %>%
    dplyr::mutate(
      id = rep(1:(nrow(.) / 2),each = 2),
      key = rep(c("est", "text"), nrow(.) / 2)
    ) %>%
    tidyr::spread(
      key, x
    ) %>%
    '['(-1,)

  id <- gsub(".*OMM:</b> |<br>.*", "", df_tidy$text)
  state <- stringr::str_sub(gsub(".*label = '|';.*", "", df_tidy$text), 1, 2)
  city <- stringr::str_extract(gsub(".*<b>Estação:</b> |<br>.*", "", df_tidy$text), ".*(?=-)")
  lat <- as.numeric(gsub(".*Latitude: |º<br>.*", "", df_tidy$text))
  lon <- as.numeric(gsub(".*Longitude: |º<br>.*", "", df_tidy$text))
  alt <- readr::parse_number(gsub(".*Altitude: | metros.*", "", df_tidy$text))
  start <- lubridate::dmy(gsub(".*Aberta em: |<br>.*", "", df_tidy$text))
  url <- gsub(".*<br><a href=|= target=_new>.*",  "", df_tidy$text)

  dplyr::data_frame(
    id, state, city, lat, lon, alt, start, url
  )
}
