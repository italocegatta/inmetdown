#' Get basic information about INMET's automatic weather station
#'
#' @export
#'
aws_stations <- function() {
  get_stations("aws")
}

#' Get basic information about INMET's conventional weather station
#'
#' @export
#'
cws_stations <- function() {
  get_stations("cws")
}

# Função generica para pesquisar estações
get_stations <- function(x) {

  if (x == "aws") {
    url <- "http://www.inmet.gov.br/sonabra/maps/pg_mapa.php"
  } else if(x == "cws") {
    url <- "http://www.inmet.gov.br/sim/sonabra/index.php"
  } else {
    message("Wrong argument. Use 'aws' or 'cws'.")
  }

  root <- xml2::read_html(url)

  nodes <- rvest::html_nodes(root, xpath = "/html/head/script[2]/text()")

  nodes_text <- rvest::html_text(nodes)

  df_messy <- tidytext::unnest_tokens(
    dplyr::as_data_frame(nodes_text),
    x, value,
    token = stringr::str_split, pattern = "\\*",
    to_lower = FALSE
  )

  df_key <- dplyr::mutate(
    df_messy,
    id = rep(1:(nrow(df_messy) / 2),each = 2),
    key = rep(c("est", "text"), nrow(df_messy) / 2)
  )

  df_tidy <- tidyr::spread(
    df_key,
    key, x
  )[-1, ]

  if (x == "aws") {
    id <- stringr::str_sub(df_tidy$est, 10, 13)
  } else {
    id <- gsub(".*OMM:</b> |<br>.*", "", df_tidy$text)
  }

  state <- stringr::str_sub(gsub(".*label = '|';.*", "", df_tidy$text), 1, 2)

  city <- stringr::str_extract(gsub(".*<b>Estação:</b> |<br>.*", "", df_tidy$text), ".*(?=-)")

  lat <- as.numeric(gsub(".*Latitude: |º<br>.*", "", df_tidy$text))

  lon <- as.numeric(gsub(".*Longitude: |º<br>.*", "", df_tidy$text))

  alt <- readr::parse_number(gsub(".*Altitude: | metros.*", "", df_tidy$text))

  start <- lubridate::dmy(gsub(".*Aberta em: |<br>.*", "", df_tidy$text))

  #status <- ifelse(stringr::str_detect(gsub(".*mm_20_|.png.*", "", df_tidy$text), "cinza"), "On", "Off")

  if (x == "aws") {
    url <- gsub(".*width=50><a href=| target=_new>.*",  "", df_tidy$text)
  } else {
    url <- gsub(".*<br><a href=|= target=_new>.*",  "", df_tidy$text)
  }

  z <- dplyr::data_frame(
    id, state, city, lat, lon, alt, start, url
  )

  return(z)
}

# Get URL's station
get_url <- function(x, id) {
  df <- get_stations(x = x)

  aux <- df$id %in% id

  if (!any(aux)) stop(sprintf("'%s' station not fund.", id))

  df[aux, "url"][[1]]
}
