# Get URL's station
get_url <- function(x, id) {
  df <- get_stations(x = x)

  aux <- df$id %in% id

  if (!any(aux)) stop(sprintf("'%s' station not fund.", id))

  df[aux, "url"][[1]]
}
