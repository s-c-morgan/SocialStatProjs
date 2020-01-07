library(jsonlite)
library(httr)
library(purrr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
#Getting JSON data from the NYDOT and binding it to a dataframe
url = "https://data.cityofnewyork.us/resource/i4gi-tjb9.json"
traf_data<-(GET(url = url))
uftraf_data<-rbindlist(content(traf_data))
#Data cleaning and standardization
traf_data<-uftraf_data%>%
  filter(status == '0')
traf_data$speed <- as.numeric(traf_data$speed)
traf_data$travel_time <- as.numeric(traf_data$travel_time)
traf_data$min<-substr(traf_data$data_as_of, str_locate(traf_data$data_as_of,'T')[1]+1,str_locate(traf_data$data_as_of,'T')[1]+5)
for (i in 1:nrow(traf_data)){
  traf_data$borough[i] <- str_replace(traf_data$borough[i],'island', 'Island')}

avg_spd <- traf_data %>%
    group_by(borough)%>%
    summarise(avg_time = mean(speed,na.rm = TRUE))

time_count <- traf_data%>%
    group_by(min)%>%
    summarise(count = n())

sum_table <- traf_data%>%
    group_by(borough,min)%>%
    summarise(avg_time = mean(speed,na.rm = TRUE))

avg_spd_viz<- ggplot(as.data.frame(avg_spd), aes(x=borough, y=avg_time, fill = borough))+
  geom_bar(stat = 'identity', show.legend = FALSE)+theme_minimal()+
  geom_text(aes(label = round(avg_time, 2)),vjust=1.6, color="white", size=3.5)+
  labs(title = 'New York City Traffic Speed by Borough as of ', subtitle = 'Data Courtesy of NYCDOT', x = 'Borough', y = 'Average Speed on Major Roadways(in MPH)')
avg_spd_viz
spd_min_viz<- ggplot(as.data.frame(sum_table), aes(x=min, y=avg_time, color = borough))+
  geom_point()
spd_min_viz