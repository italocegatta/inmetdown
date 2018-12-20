
# teste geral -------------------------------------------------------------

library(inmetdown)

est <- inmet_estacoes()

inmet_download_sonabra_automatica("A108", Sys.Date() - 365, Sys.Date(), est)
inmet_download_sonabra_automatica("A108", Sys.Date() - 370, Sys.Date(), est)

inmet_download_sonabra_convencional("82994", Sys.Date() - 80, Sys.Date() - 75, est)

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

id = "82336"
id = c("82915", "82326")
start = Sys.Date() - 10
end = Sys.Date() - 1
stations = inmetdown::cws_station()
proxy = "."

inmetdown::cws_import(id, start, end, stations = stations)


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
