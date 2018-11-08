
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

id = "83064"
id = c("82915", "82326")
start = Sys.Date() - 190
end = Sys.Date() - 180
stations = inmetdown::cws_station()
proxy = "."
token = "^.+instruções\n--------------------\n"

inmetdown::cws_import(id, start, end, stations = stations) %>% View()
