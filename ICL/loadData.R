setwd("Covid-19-Model-Validation/ICL")

library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(openxlsx)
county_level_url = "data/us-states.csv"
library(dplyr)
library(tidyverse)
#-----Creating datset for ICL model-----#
#####  NY data from NY time, till April 15 
data_county_level = read.csv(county_level_url) %>%
  select(-fips,) %>%
  arrange(date) 

data_NY=data_county_level[which(data_county_level$state=="New York"),]
data_NY_death=aggregate(data_NY$deaths, by=list(Category=data_NY$date), FUN=sum) 
data_NY_case=aggregate(data_NY$cases, by=list(Category=data_NY$date), FUN=sum) 
df_NY=data.frame(date=data_NY_death$Category,death=data_NY_death$x,case=data_NY_case$x)
ifr_NY=df_NY$death[nrow(df_NY)-1]/df_NY$case[nrow(df_NY)-1]#0.05890672
df_NY$death=c(0,diff(df_NY$death))
df_NY$case=c(0,diff(df_NY$case))

data_merge_JHU_NY=df_NY
data_merge_JHU_NY$DateRep = as.Date(as.character(df_NY$date), format = "%Y-%m-%d")
data_merge_JHU_NY$DateRep=format(data_merge_JHU_NY$DateRep,format="%d/%m/%Y")

data_merge_JHU_NY$Cases=data_merge_JHU_NY$case
data_merge_JHU_NY$Countries.and.territories="NY"
data_merge_JHU_NY$Deaths=data_merge_JHU_NY$death
data_merge_JHU_NY=data_merge_JHU_NY[,c("Countries.and.territories","DateRep","Cases","Deaths")]
data_merge_JHU_NY=data_merge_JHU_NY[-nrow(data_merge_JHU_NY),]
save(data_merge_JHU_NY,file="data/data_merge_JHU_NY.RData")


#####  NY data from NY time, till May 1
county_level_url = "data/us-states_tillMay1.csv"

data_county_level = read.csv(county_level_url) %>%
  select(-fips,) %>%
  arrange(date) 

data_NY=data_county_level[which(data_county_level$state=="New York"),]
data_NY_death=aggregate(data_NY$deaths, by=list(Category=data_NY$date), FUN=sum) 
data_NY_case=aggregate(data_NY$cases, by=list(Category=data_NY$date), FUN=sum) 
df_NY=data.frame(date=data_NY_death$Category,death=data_NY_death$x,case=data_NY_case$x)
ifr_NY=df_NY$death[nrow(df_NY)-1]/df_NY$case[nrow(df_NY)-1]#0.05890672
df_NY$death=c(0,diff(df_NY$death))
df_NY$case=c(0,diff(df_NY$case))

data_merge_JHU_NY=df_NY
data_merge_JHU_NY$DateRep = as.Date(as.character(df_NY$date), format = "%Y-%m-%d")
data_merge_JHU_NY$DateRep=format(data_merge_JHU_NY$DateRep,format="%d/%m/%Y")

data_merge_JHU_NY$Cases=data_merge_JHU_NY$case
data_merge_JHU_NY$Countries.and.territories="NY"
data_merge_JHU_NY$Deaths=data_merge_JHU_NY$death
data_merge_JHU_NY=data_merge_JHU_NY[,c("Countries.and.territories","DateRep","Cases","Deaths")]
data_merge_JHU_NY=data_merge_JHU_NY[-nrow(data_merge_JHU_NY),]
save(data_merge_JHU_NY,file="data/data_merge_JHU_NY_tillMay1.RData")




