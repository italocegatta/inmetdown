
# nova api ----------------------------------------------------------------

library(magrittr)

source("R/util.R")

start = "2020-01-01"
end = "2020-01-01"
station = "83049"
station = "82376"
station = "A001"

library(inmetdown)

cad <- inmet_station()
out <- list()
i=1
i =603
625

for (i in seq_along(cad$CD_ESTACAO)[600:630]) {

  out[[i]] <- inmet_import(
    cad$CD_ESTACAO[[i]],
    "2020-08-20",
    "2020-08-20"
  )

  print(stringr::str_glue("{i}/{nrow(cad)} - {format( Sys.time(), '%H:%M')}"))

}

head(out)

dplyr::bind_rows(out) %>%
  dplyr::distinct(HR_MEDICAO) %>%
  View()

View(dplyr::bind_rows(out))

# teste geral -------------------------------------------------------------

library(inmetdown)

est <- inmet_estacoes()

inmet_download_sonabra_automatica("A108", Sys.Date() - 365, Sys.Date(), est)
inmet_download_sonabra_automatica("A108", Sys.Date() - 370, Sys.Date(), est)

inmet_download_sonabra_convencional("83235", Sys.Date() - 80, Sys.Date() - 5, est)

inmet_download_bdmep_convencional("82000", Sys.Date() - 100, Sys.Date() - 95, est)



aws_import("A108", Sys.Date() - 60, Sys.Date())

cws_import("82994", Sys.Date() - 30, Sys.Date())
cws_import("82915", Sys.Date() - 100, Sys.Date() - 95)

x <- inmetdown:::import_bdmep("82915", Sys.Date() - 100, Sys.Date() - 95, stations = cws_station(), proxy = ".")

#purrr::map(cws_station()$id %>% slice(head(n = 20), ~cws_import(.x, Sys.Date() - 100, Sys.Date() - 95, stations = cws_station()))

writexl::write_xlsx(x, "variaveis_inmet_convencial.xlsx")

# automatica --------------------------------------------------------------

library(magrittr)
load("R/sysdata.rda")
source("R/util.R")
source("R/aws_import.R")

id = "A567"
id = c("A108", "A140")
start = Sys.Date() - 68
end = Sys.Date() - 1
stations = inmetdown::aws_station()
proxy = "."

inmetdown::aws_import(id, start, end, stations = stations) %>%
  print(n =20)

# sonabra --------------------------------------------------------------

library(magrittr)
load("R/sysdata.rda")
source("R/util.R")

id = "83235"
id = c("82915", "82326")
inicio = Sys.Date() - 20
fim = Sys.Date()
estacoes = inmetdown::inmet_estacoes()
proxy = "."

inmetdown::inmet_download_sonabra_convencional(id, inicio, fim, estacoes = estacoes)


# bdmep --------------------------------------------------------------

library(magrittr)
load("R/sysdata.rda")
source("R/util.R")

id = "83577"
id = "82326"
# id = c("82915", "82326")
inicio = Sys.Date() - 100; start = inicio
fim = Sys.Date() - 95; end = fim
estacoes = inmetdown::inmet_estacoes()
proxy = httr::use_proxy("201.6.149.20", 8080)
token = "^.+instruções\n--------------------\n"
i = 1

inmetdown::inmet_download_bdmep( c("82915", "83577"), Sys.Date() - 100, Sys.Date() - 95) %>%
  print(n = Inf)

inmetdown:::import_bdmep(id, start, end, stations, proxy)

inmetdown::cws_import(id, start, end, stations = stations) %>% View()
