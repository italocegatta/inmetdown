# debug BDMEP

library(magrittr)
load("R/sysdata.rda")
source("R/check_date.R")
source("R/aux_import_aws.R")
source("R/aux_import_cws.R")
i = 1
id = c("82915", "82326")
id = "82336"
start =  Sys.Date() - 100
end =  Sys.Date() - 85

inmetdown:::import_bdmep("82336", Sys.Date() - 100, Sys.Date() -100)

# debug SONABRA

library(magrittr)
load("R/sysdata.rda")
source("R/check_date.R")
source("R/cws_station.R")
source("R/aux_import_aws.R")
source("R/aux_import_cws.R")
# library(inmetdown)
i = 1
# id = c("82915", "82326")
id = "82691"
start =  as.Date("2018-01-01")
end = as.Date("2018-01-31")
stations <- inmetdown::cws_station()


inmetdown:::import_sonabra(id, start , end, NULL)
inmetdown:::import_sonabra(id, start , end, stations)
inmetdown:::import_sonabra("82336", start, end, stations)
inmetdown:::import_sonabra(c("82915", "82326"), start,end, stations)

cws_import("82336", Sys.Date() - 92, Sys.Date() - 88, stations)

cws_import("82336", as.Date("2017-12-25"), as.Date("2018-01-05"))
cws_import( c("82915", "82326"), as.Date("2017-12-25"), as.Date("2018-01-05"))


Sys.Date() - 90


# debug cws_import

start =  Sys.Date() - 58
end =  Sys.Date() - 30
inmetdown::cws_import("83398", start, end)



isTRUE(c(start, end))
isTRUE(TRUE)


inmetdown:::import_bdmep(id, dates$bdmep[1], dates$bdmep[2])

#library(inmetdown)
load("R/sysdata.rda")
source("R/import.R")
source("R/aws_station.R")
source("R/check_date.R")
library(magrittr)

#id = "A032"
# id = "A137"
id= c("A137", "A104", "A032")
start =  Sys.Date() - 35
end =  Sys.Date()

stations <- inmetdown::cws_station()

inmetdown::aws_import( c("A446"), Sys.Date() - 35, Sys.Date())



##############
library(magrittr)
source("R/check_date.R")
source("R/import.R")


i = 1
id = "82336"
id <- c("82915", "82326")
start =  Sys.Date() - 100
end =  Sys.Date() - 85



cws_import("82326", Sys.Date() - 20, Sys.Date() -1)

cws_import( c("82915", "82326"), Sys.Date() - 10, Sys.Date() )

cws_station() %>% View()

##############################

split_dates <- function(start, end) {

  seq <- seq.Date(start, end, '1 day')

  dummy <- (seq < (lubridate::today("GMT") - 90))

  list(
    bdmep = range(seq[dummy]),
    inmet = range(seq[!dummy])
  )
}


