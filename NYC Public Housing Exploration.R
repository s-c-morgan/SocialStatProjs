library(jsonlite)
library(httr)
library(purrr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
#Getting JSON data from the NYDOT and binding it to a dataframe
url = "https://data.cityofnewyork.us/resource/evjd-dqpz.json"
traf_data<-(GET(url = url))
uftraf_data<-rbindlist(content(traf_data), fill = TRUE)