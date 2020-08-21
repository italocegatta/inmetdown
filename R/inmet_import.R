#' Download INMET data
#'
#' @export
#'
inmet_import <- function(station, start, end) {

  ..start <- check_date(start)
  ..end <- check_date(end)

  ..req <- stringr::str_glue(
    "https://apitempo.inmet.gov.br/estacao/{..start}/{..end}/{station}"
  )

  ..get <- polite_GET(..req)

  if (httr::status_code(..get) != 200) return(NULL)

  ..content <- httr::content(..get, type = "text", encoding = "UTF-8")

  ..table <- ..content %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    dplyr::select(
      CD_ESTACAO,
      DT_MEDICAO,
      HR_MEDICAO,
      CHUVA,
      dplyr::starts_with("TEM_"),
      dplyr::starts_with("TEMP_"),
      dplyr::starts_with("UMD_"),
      dplyr::starts_with("UMID_"),
      dplyr::starts_with("PTO_"),
      dplyr::starts_with("PRE_"),
      dplyr::starts_with("PRESS_"),
      dplyr::starts_with("RAD_"),
      dplyr::starts_with("INSO_"),
      dplyr::starts_with("NEBU_"),
      dplyr::starts_with("VEN_"),
      dplyr::starts_with("VENT_")
    ) %>%
    dplyr::mutate(
      dplyr::across(-c(CD_ESTACAO, DT_MEDICAO), as.numeric),
      DT_MEDICAO = as.Date(DT_MEDICAO),
      HR_MEDICAO = HR_MEDICAO / 100
    )

  return(..table)

}
