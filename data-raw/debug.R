
# teste geral -------------------------------------------------------------

library(inmetdown)
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

id = "82915"
id = c("82915", "82326")
start = Sys.Date() - 100
end = Sys.Date() - 95
stations = inmetdown::cws_station()
proxy = "."
token = "^.+instruções\n--------------------\n"
i = 1

inmetdown:::import_bdmep(id, start, end, stations, proxy)

inmetdown::cws_import(id, start, end, stations = stations) %>% View()
