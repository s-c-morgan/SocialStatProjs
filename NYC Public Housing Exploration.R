library(jsonlite)
library(httr)
library(purrr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)
#Getting JSON data from the NYDOT and binding it to a dataframe
url = "https://data.cityofnewyork.us/resource/evjd-dqpz.json"
traf_data<-(GET(url = url))
uftraf_data<-rbindlist(content(traf_data), fill = TRUE)

housing_data<-uftraf_data

##Cleaning and Parsing
for (i in 1:nrow(housing_data)){
  housing_data$mean_stories[i]<-mean(as.numeric(str_split(gsub(',','-',housing_data$number_of_stories[i]),pattern = '-')[[1]]))
  for (j in 12:35){
    housing_data[i][[j]]<-gsub('\\$','',gsub('%','',gsub(',','',housing_data[i][[j]])))
  }
  housing_data$completion_year[i]<-as.numeric(str_split(housing_data$completion_date[i],pattern = '/')[[1]][3])
}


#Adjusting Development Costs for Inflation(Using BLS Urban CPI dataset)
bls_data<-read_xlsx('BLS.xlsx')
for (i in 1:nrow(bls_data)){
  bls_data$Mean_Index[i]<-mean(t(bls_data[i,2:13]),na.rm = TRUE)
}
inflation_adjustment<-function(price,priceyear,baseyear){
   return(price*((bls_data[bls_data$Year==baseyear,]$Mean_Index[1])/bls_data[bls_data$Year==priceyear,]$Mean_Index[1]))
}
for (i in 1:nrow(housing_data)){
  housing_data$Adjusted_Development_Cost[i]<-inflation_adjustment(as.numeric(housing_data$development_cost[i]),housing_data$completion_year[i],'2020')
  housing_data$Adjusted_Cost_Per_Room[i]<-inflation_adjustment(as.numeric(housing_data$per_rental_room[i]),housing_data$completion_year[i],'2020')
}
cost_dist<-ggplot(data = housing_data,aes(x=Adjusted_Cost_Per_Room))+
  geom_histogram(color="black", fill="lightblue",binwidth = 5000)
cost_dist

time_chart<-ggplot(data = housing_data,aes(x=completion_year,y=Adjusted_Cost_Per_Room))+
  geom_point()+
  scale_x_continuous(name="Speed of cars", limits=c(1930, 2020))
time_chart
  