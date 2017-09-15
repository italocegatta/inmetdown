#library(inmetdown)
load("R/sysdata.rda")
source("R/import.R")
source("R/aws_station.R")
source("R/check_date.R")
library(magrittr)

#id = "A032"
id= c("A137", "A104", "A032")
start =  Sys.Date() - 1
end =  Sys.Date()
ins = FALSE

aws_import( c("A137", "A104", "A032"), Sys.Date() - 1, Sys.Date() )
aws_import( c("A032"), Sys.Date() - 1, Sys.Date() )



##############

i = 1
id = "82915"
start =  Sys.Date() - 10
end =  Sys.Date()


cws_import( c("82915"), Sys.Date() - 10, Sys.Date() )




