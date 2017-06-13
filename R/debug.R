# source("R/get_stations.R")
# source("R/check_date.R")
# source("R/aws_import.R")
# load("R/sysdata.rda")
#
#
# x = "aws"
# id = "A123"
# id = "A108"
# id = "82067"
# id = "82915"
# start =  Sys.Date()
# end =  Sys.Date()
#
#
# get_aws("A123",  Sys.Date(), Sys.Date(), TRUE)
# aws_import("A123",  Sys.Date(), Sys.Date(), TRUE)
#
# get_aws("A108",  Sys.Date(), Sys.Date(), TRUE)
# aws_import("A108",  Sys.Date(), Sys.Date(), TRUE)
#
# library(inmetdown)
# x <- aws_stations()
# aws_import(c("A108", "A123"),  Sys.Date(), Sys.Date(), TRUE)
