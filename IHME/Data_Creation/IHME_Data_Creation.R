library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(openxlsx)

moving_avg = function(x)
{
  y = rollmean(x, 3, fill = NA)
  y[1] = y[2] - ((y[4]-y[2])/2)
  length.y = length(y)
  y[length.y] = y[length.y-1] + (y[length.y-1] - y[length.y-3])/2
  return(y)
}

#----------------------------START OF DATA CREATION: IHME MODEL------------------#

county_level_url_JHU_deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
data_county_level_JHU_deaths_NY = read.csv(county_level_url_JHU_deaths, na.strings = c("NA","")) %>%
  dplyr::rename(country = `Country_Region`, state = `Province_State`, county = `Admin2`) %>%
  select(-Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Population) %>%
  pivot_longer(-c(country, state, county), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(gsub("X", "0", date), "%m.%d.%y")) %>% filter(county != "Unassigned") %>% group_by(state,date) %>% summarise(deaths = sum(deaths)) %>% filter(state == "New York") %>%
  mutate(log_death_rate = log(deaths/19.45), population = 19.45) %>% filter(log_death_rate >= log(0.31)) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "New York") %>% ungroup() %>% distinct() %>% select(-state)

data_county_level_JHU_deaths_NY_nytimes = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% filter(state == "New York") %>%
select(-fips, -cases, -fips)  %>%
mutate(log_death_rate = log(deaths/19.45), population = 19.45) %>%
filter(log_death_rate >= log(0.31)) %>%
mutate(smooth = moving_avg(log_death_rate), group = "New York") %>%
ungroup() %>% distinct() %>% select(-state) %>% mutate(date = as.Date(date))

#county_level_url_JHU_cases = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
#data_county_level_JHU_cases = read.csv(county_level_url_JHU_cases, na.strings = c("NA","")) %>%
# dplyr::rename(country = `Country_Region`, state = `Province_State`, county = `Admin2`) %>%
#select(-Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key) %>%
#pivot_longer(-c(country, state, county), names_to = "date", values_to = "confirmed") %>%
#mutate(date = as.Date(gsub("X", "0", date), "%m.%d.%y")) %>% group_by(state,date) %>% summarise(cases = sum(confirmed)) %>% filter(state == "New York" | state == "Washington")


county_level_url_JHU_deaths = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
data_county_level_JHU_deaths_WA = read.csv(county_level_url_JHU_deaths, na.strings = c("NA","")) %>%
  dplyr::rename(country = `Country_Region`, state = `Province_State`, county = `Admin2`) %>%
  select(-Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Population) %>%
  pivot_longer(-c(country, state, county), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(gsub("X", "0", date), "%m.%d.%y")) %>% group_by(state,date) %>% summarise(deaths = sum(deaths)) %>% filter(state == "Washington") %>%
  mutate(log_death_rate = log(deaths/7.615), population = 7.615) %>% filter(log_death_rate >= log(0.31)) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "Washington") %>% ungroup() %>% select(-state)


deaths_cases_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

data_deaths_JHU_Italy = read.csv(deaths_cases_url, na.strings = c("NA","")) %>%
  dplyr::rename(country = `Country.Region`, province = `Province.State`) %>%
  select(-Lat, -Long) %>%
  pivot_longer(-c(country, province), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(gsub("X", "0", date), "%m.%d.%y")) %>%
  select(-province) %>%
  arrange(date) %>%
  group_by(date, country) %>%
  mutate(deaths = sum(deaths)) %>%
  distinct() %>%
  ungroup() %>%
  filter(country == "Italy") %>%
  mutate(log_death_rate = log(deaths/60.36), population = 60.36) %>% filter(log_death_rate >= log(0.31)) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "Italy") %>% select(-country)


data_deaths_JHU_France = read.csv(deaths_cases_url, na.strings = c("NA","")) %>%
  dplyr::rename(country = `Country.Region`, province = `Province.State`) %>%
  select(-Lat, -Long) %>%
  pivot_longer(-c(country, province), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(gsub("X", "0", date), "%m.%d.%y")) %>%
  select(-province) %>%
  arrange(date) %>%
  group_by(date, country) %>%
  mutate(deaths = sum(deaths)) %>%
  distinct() %>%
  ungroup() %>%
  filter(country == "France") %>%
  mutate(log_death_rate = log(deaths/65.24), population = 65.24) %>% filter(log_death_rate >= log(0.31)) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "France") %>% select(-country) %>% distinct()

data_deaths_JHU_India = read.csv(deaths_cases_url, na.strings = c("NA","")) %>%
  dplyr::rename(country = `Country.Region`, province = `Province.State`) %>%
  select(-Lat, -Long) %>%
  pivot_longer(-c(country, province), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(gsub("X", "0", date), "%m.%d.%y")) %>%
  select(-province) %>%
  arrange(date) %>%
  group_by(date, country) %>%
  mutate(deaths = sum(deaths)) %>%
  distinct() %>%
  ungroup() %>%
  filter(country == "India") %>%
  mutate(log_death_rate = log(deaths/1.35), population = 1.35) %>% filter(log_death_rate >= log(30)) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "India") %>% select(-country)


data_IHME = rbind(data_deaths_JHU_France,data_deaths_JHU_Italy, data_county_level_JHU_deaths_NY_nytimes, data_county_level_JHU_deaths_WA, data_deaths_JHU_India)

social_distance_covariate = function(data, group)
{
  if(group == "Italy")
  {
    data = data %>% filter(group == "Italy")
    social_distance = rep(NA, dim(data)[1])
    for(i in 1: length(social_distance))
    {
      if(data$date[i] <= as.Date("2020-03-04"))
        social_distance[i] = 1
      if(data$date[i] > as.Date("2020-03-04") & data$date[i] <= as.Date("2020-03-09"))
        social_distance[i] = 0.67
      if(data$date[i] > as.Date("2020-03-09") & data$date[i] <= as.Date("2020-03-11"))
        social_distance[i] = 0.33
      if(data$date[i] > as.Date("2020-03-11"))
        social_distance[i] = 0
    }
  }
  if(group == "France")
  {
    data = data %>% filter(group == "France")
    social_distance = rep(NA, dim(data)[1])
    for(i in 1: length(social_distance))
    {
      if(data$date[i] <= as.Date("2020-03-14"))
        social_distance[i] = i-1
      if(data$date[i] > as.Date("2020-03-14") & data$date[i] <= as.Date("2020-03-15"))
        social_distance[i] = social_distance[i-1] + 0.67
      if(data$date[i] > as.Date("2020-03-15") & data$date[i] <= as.Date("2020-03-18"))
        social_distance[i] = social_distance[i-1] + 0.33
      if(data$date[i] > as.Date("2020-03-18"))
        social_distance[i] = social_distance[i-1]
    }
  }
  
  if(group == "New York")
  {
    data = data %>% filter(group == "New York")
    social_distance = rep(0, dim(data)[1])
    for(i in 1: length(social_distance))
    {
      if(data$date[i] <= as.Date("2020-03-18"))
        social_distance[i] = 1
      if(data$date[i] > as.Date("2020-03-18") & data$date[i] <= as.Date("2020-03-20"))
        social_distance[i] = 0.67
    }
  }
  
  if(group == "India")
  {
    data = data %>% filter(group == "India")
    social_distance = rep(1, dim(data)[1])
  }
  
  return(social_distance)
}

data_IHME = data_IHME %>% mutate(social_distance = NA)
data_IHME$social_distance[data_IHME$group == "Italy"] = social_distance_covariate(data_IHME, "Italy")
data_IHME$social_distance[data_IHME$group == "France"] = social_distance_covariate(data_IHME, "France")
data_IHME$social_distance[data_IHME$group == "New York"] = social_distance_covariate(data_IHME, "New York")
data_IHME$social_distance[data_IHME$group == "India"] = social_distance_covariate(data_IHME, "India")
data_IHME$intercept = 1
View(data_IHME)

peak_date = data.frame(group=c("France","Italy","New York", "India"),
                       date=c(as.Date("2020-04-05"),as.Date("2020-03-27"),
                              as.Date("2020-04-07"), as.Date("2020-04-18")))

train_data = function(group,days_before){
  data_sub = data_IHME[data_IHME$group == group & data_IHME$date<=(peak_date$date[peak_date$group==group]-days_before),]
  data_sub$time = 1:(nrow(data_sub))
  data_sub$measurement_std = (nrow(data_sub):1)/100
  data_sub$intercept = rep(1,nrow(data_sub))
  return(data_sub)
}


#---Extracting data for Italy---#
## training data Italy
df7_Italy = train_data("Italy",7)
df14_Italy = train_data("Italy",14)
df0_Italy = train_data("Italy", 0)
df_final_projection_Italy = data_IHME[data_IHME$group == "Italy", ]
df_final_projection_Italy = df_final_projection_Italy %>% mutate(measurement_std = sort(seq(0.1, length.out = nrow(df_final_projection_Italy),by = 0.1), decreasing= TRUE))
df_final_projection_Italy = df_final_projection_Italy %>% mutate(time = seq(1, nrow(df_final_projection_Italy),1))
write.xlsx(df7_Italy,file="df7_Italy_v1.xlsx")
write.xlsx(df14_Italy,file="df14_Italy_v1.xlsx")
write.xlsx(df0_Italy,file="df0_Italy_v1.xlsx")
write.xlsx(df_final_projection_Italy,file="df_final_projection_Italy.xlsx")

#----------------------------END OF DATA CREATION: IHME MODEL------------------#
