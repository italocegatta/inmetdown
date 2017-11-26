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

inmetdown::aws_import( c("A446"), Sys.Date() - 35, Sys.Date())



##############
library(magrittr)
source("R/check_date.R")
source("R/import.R")


i = 1
id = "82336"
id <- c("82915", "82326")
start =  Sys.Date() - 100
end =  Sys.Date()



cws_import("82326", Sys.Date() - 20, Sys.Date() -1)

cws_import( c("82915", "82326"), Sys.Date() - 10, Sys.Date() )

cws_station()

##############################

split_dates <- function(start, end) {

  seq <- seq.Date(start, end, '1 day')

  dummy <- (seq < (lubridate::today("GMT") - 90))

  list(
    bdmep = range(seq[dummy]),
    inmet = range(seq[!dummy])
  )
}


