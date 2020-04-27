library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(openxlsx)

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

tmp = data_deaths_JHU_Italy
tmp = tmp %>% mutate(deaths = deaths - lag(deaths, 1))
IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-28"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-28") &
                      tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/data/IHME/Italy/df0_Italy_pred_CI_v1.xlsx")
colnames(IHME_predicted)[5] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all$Source[df_all$Source == "1"] = "Predicted"
df_all$Source[df_all$Source == "2"] = "Training"
df_all$Source[df_all$Source == "3"] = "Reported"

IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-21"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-21") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/data/IHME/Italy/df7_Italy_pred_CI_v1.xlsx")
colnames(IHME_predicted)[5] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all2 <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all2$Source[df_all2$Source == "1"] = "Predicted"
df_all2$Source[df_all2$Source == "2"] = "Training"
df_all2$Source[df_all2$Source == "3"] = "Reported"

IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-14"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-14") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/data/IHME/Italy/df14_Italy_pred_CI_v1.xlsx")
colnames(IHME_predicted)[5] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all3 <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all3$Source[df_all3$Source == "1"] = "Predicted"
df_all3$Source[df_all3$Source == "2"] = "Training"
df_all3$Source[df_all3$Source == "3"] = "Reported"

df_final = bind_rows(df_all,df_all2,df_all3,.id = "Training")
df_final$Training[df_final$Training == "1"] = "Till peak"
df_final$Training[df_final$Training == "2"] = "7 days before peak"
df_final$Training[df_final$Training == "3"] = "14 days before peak"
df_final = df_final[c(1,2,4,7,9,10)]
write.xlsx(df_final,"df_Italy_CI_tableau.xlsx")





#---------------Medium Article------#
#-----By Pro----#
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(openxlsx)

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

tmp = data_deaths_JHU_Italy
tmp = tmp %>% mutate(deaths = deaths - lag(deaths, 1))
IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-27"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-27") &
                      tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df0_Italy_pred_CI.xlsx")
colnames(IHME_predicted)[6] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all$Source[df_all$Source == "1"] = "Predicted"
df_all$Source[df_all$Source == "2"] = "Training"
df_all$Source[df_all$Source == "3"] = "Observed"

IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-20"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-20") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df7_Italy_pred_CI.xlsx")
colnames(IHME_predicted)[6] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all2 <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all2$Source[df_all2$Source == "1"] = "Predicted"
df_all2$Source[df_all2$Source == "2"] = "Training"
df_all2$Source[df_all2$Source == "3"] = "Observed"

IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-13"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-13") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df14_Italy_pred_CI.xlsx")
colnames(IHME_predicted)[6] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all3 <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all3$Source[df_all3$Source == "1"] = "Predicted"
df_all3$Source[df_all3$Source == "2"] = "Training"
df_all3$Source[df_all3$Source == "3"] = "Observed"


# h <- ggplot(data=df_all3,aes(x=date, y = deaths))
# plot1 <- h + 
#   geom_ribbon(aes(x=date,ymin=PI_low,ymax=PI_upper, group = Source),fill = "grey70") +
#   geom_line(aes(y=deaths,color=Source, group = Source)) + theme(legend.position = "none") + theme_bw() + 
#   labs(y="Deaths per day")
# plot1


df_final = bind_rows(df_all,df_all2,df_all3,.id = "Training")
df_final$Training[df_final$Training == "1"] = "Till peak"
df_final$Training[df_final$Training == "2"] = "7 days before peak"
df_final$Training[df_final$Training == "3"] = "14 days before peak"
df_final = df_final[c(1,2,4,8,9,10)]
df_final$PI_low[which(df_final$PI_low <= 0)] = 0
write.xlsx(df_final,"~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df_Italy_CI_tableau.xlsx")


#----NY----#

data_county_level_JHU_deaths_NY_nytimes = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% filter(state == "New York") %>%
  select(-fips, -cases, -fips)  %>%
  mutate(log_death_rate = log(deaths/19.45), population = 19.45) %>%
  filter(log_death_rate >= log(0.31)) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "New York") %>%
  ungroup() %>% distinct() %>% select(-state) %>% mutate(date = as.Date(date))


tmp = data_county_level_JHU_deaths_NY_nytimes
tmp = tmp %>% mutate(deaths = deaths - lag(deaths, 1))
IHME_training = tmp[tmp$date >= as.Date("2020-03-16") & tmp$date <= as.Date("2020-04-07"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-04-07") &
                      tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df0_NY_pred_CI.xlsx")
colnames(IHME_predicted)[6] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all$Source[df_all$Source == "1"] = "Predicted"
df_all$Source[df_all$Source == "2"] = "Training"
df_all$Source[df_all$Source == "3"] = "Observed"

IHME_training = tmp[tmp$date >= as.Date("2020-03-16") & tmp$date <= as.Date("2020-03-31"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-31") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df7_NY_pred_CI.xlsx")
colnames(IHME_predicted)[6] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all2 <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all2$Source[df_all2$Source == "1"] = "Predicted"
df_all2$Source[df_all2$Source == "2"] = "Training"
df_all2$Source[df_all2$Source == "3"] = "Observed"

IHME_training = tmp[tmp$date >= as.Date("2020-03-16") & tmp$date <= as.Date("2020-03-24"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-24") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df14_NY_pred_CI.xlsx")
colnames(IHME_predicted)[6] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all3 <- bind_rows(IHME_predicted,IHME_training,IHME_reported,.id = "Source")
df_all3$Source[df_all3$Source == "1"] = "Predicted"
df_all3$Source[df_all3$Source == "2"] = "Training"
df_all3$Source[df_all3$Source == "3"] = "Observed"


# h <- ggplot(data=df_all3,aes(x=date, y = deaths))
# plot1 <- h + 
#   geom_ribbon(aes(x=date,ymin=PI_low,ymax=PI_upper, group = Source),fill = "grey70") +
#   geom_line(aes(y=deaths,color=Source, group = Source)) + theme(legend.position = "none") + theme_bw() + 
#   labs(y="Deaths per day")
# plot1


df_final = bind_rows(df_all,df_all2,df_all3,.id = "Training")
df_final$Training[df_final$Training == "1"] = "Till peak"
df_final$Training[df_final$Training == "2"] = "7 days before peak"
df_final$Training[df_final$Training == "3"] = "14 days before peak"
df_final = df_final[c(1,2,4,8,9,10)]
df_final$PI_low[which(df_final$PI_low <= 0)] = 0
write.xlsx(df_final,"~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df_NY_CI_tableau.xlsx")




#---- Italy cumulative CI for tableu---#
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

tmp = data_deaths_JHU_Italy
#tmp = tmp %>% mutate(deaths = deaths - lag(deaths, 1))
IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-27"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-27") &
                      tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df0_Italy_pred_CI.xlsx")
colnames(IHME_predicted)[3] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all <- bind_rows(IHME_predicted[-1,],IHME_training,IHME_reported,.id = "Source")
df_all$Source[df_all$Source == "1"] = "Predicted"
df_all$Source[df_all$Source == "2"] = "Training"
df_all$Source[df_all$Source == "3"] = "Reported"

IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-20"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-20") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df7_Italy_pred_CI.xlsx")
colnames(IHME_predicted)[3] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all2 <- bind_rows(IHME_predicted[-1,],IHME_training,IHME_reported,.id = "Source")
df_all2$Source[df_all2$Source == "1"] = "Predicted"
df_all2$Source[df_all2$Source == "2"] = "Training"
df_all2$Source[df_all2$Source == "3"] = "Reported"

IHME_training = tmp[tmp$date >= as.Date("2020-02-28") & tmp$date <= as.Date("2020-03-13"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-13") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df14_Italy_pred_CI.xlsx")
colnames(IHME_predicted)[3] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all3 <- bind_rows(IHME_predicted[-1,],IHME_training,IHME_reported,.id = "Source")
df_all3$Source[df_all3$Source == "1"] = "Predicted"
df_all3$Source[df_all3$Source == "2"] = "Training"
df_all3$Source[df_all3$Source == "3"] = "Reported"

df_final = bind_rows(df_all,df_all2,df_all3,.id = "Training")
df_final$Training[df_final$Training == "1"] = "Till peak"
df_final$Training[df_final$Training == "2"] = "7 days before peak"
df_final$Training[df_final$Training == "3"] = "14 days before peak"
df_final = df_final[c(1,2,4,5,9,10)]
write.xlsx(df_final,"~/Dropbox/COVID_19/results/IHME/Italy/Plots_medium_article/df_Italy_cumulative_CI_tableau.xlsx")


#----NY cumulative CI----#

data_county_level_JHU_deaths_NY_nytimes = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% filter(state == "New York") %>%
  select(-fips, -cases, -fips)  %>%
  mutate(log_death_rate = log(deaths/19.45), population = 19.45) %>%
  filter(log_death_rate >= log(0.31)) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "New York") %>%
  ungroup() %>% distinct() %>% select(-state) %>% mutate(date = as.Date(date))


tmp = data_county_level_JHU_deaths_NY_nytimes
#tmp = tmp %>% mutate(deaths = deaths - lag(deaths, 1))
IHME_training = tmp[tmp$date >= as.Date("2020-03-16") & tmp$date <= as.Date("2020-04-07"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-04-07") &
                      tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df0_NY_pred_CI.xlsx")
colnames(IHME_predicted)[3] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all <- bind_rows(IHME_predicted[-1,],IHME_training,IHME_reported,.id = "Source")
df_all$Source[df_all$Source == "1"] = "Predicted"
df_all$Source[df_all$Source == "2"] = "Training"
df_all$Source[df_all$Source == "3"] = "Observed"

IHME_training = tmp[tmp$date >= as.Date("2020-03-16") & tmp$date <= as.Date("2020-03-31"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-31") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df7_NY_pred_CI.xlsx")
colnames(IHME_predicted)[3] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all2 <- bind_rows(IHME_predicted[-1,],IHME_training,IHME_reported,.id = "Source")
df_all2$Source[df_all2$Source == "1"] = "Predicted"
df_all2$Source[df_all2$Source == "2"] = "Training"
df_all2$Source[df_all2$Source == "3"] = "Observed"

IHME_training = tmp[tmp$date >= as.Date("2020-03-16") & tmp$date <= as.Date("2020-03-24"),1:2]
IHME_reported = tmp[tmp$date >= as.Date("2020-03-24") & tmp$date <= as.Date("2020-04-22"),1:2]
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df14_NY_pred_CI.xlsx")
colnames(IHME_predicted)[3] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all3 <- bind_rows(IHME_predicted[-1,],IHME_training,IHME_reported,.id = "Source")
df_all3$Source[df_all3$Source == "1"] = "Predicted"
df_all3$Source[df_all3$Source == "2"] = "Training"
df_all3$Source[df_all3$Source == "3"] = "Observed"


# h <- ggplot(data=df_all3,aes(x=date, y = deaths))
# plot1 <- h + 
#   geom_ribbon(aes(x=date,ymin=PI_low,ymax=PI_upper, group = Source),fill = "grey70") +
#   geom_line(aes(y=deaths,color=Source, group = Source)) + theme(legend.position = "none") + theme_bw() + 
#   labs(y="Deaths per day")
# plot1


df_final = bind_rows(df_all,df_all2,df_all3,.id = "Training")
df_final$Training[df_final$Training == "1"] = "Till peak"
df_final$Training[df_final$Training == "2"] = "7 days before peak"
df_final$Training[df_final$Training == "3"] = "14 days before peak"
df_final = df_final[c(1,2,4,5,9,10)]
df_final$PI_low[which(df_final$PI_low <= 0)] = 0
write.xlsx(df_final,"~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df_NY_cumulative_CI_tableau.xlsx")




#-----Maharashtra----#

data_maharashtra = read.xlsx("~/Dropbox/COVID_19/data/IHME/India/df_Maharashtra_datasettillApril22.xlsx")
tmp = data_maharashtra 
tmp$Date = convertToDate(tmp$Date)
#tmp = tmp %>% mutate(deaths = deaths - lag(deaths, 1))
IHME_training = tmp[tmp$date <= as.Date("2020-04-23"),]
IHME_training = IHME_training %>% mutate(deaths = cumulative_deaths-lag(cumulative_deaths,1))
IHME_predicted = read.xlsx("~/Dropbox/COVID_19/results/IHME/India/Plots_medium_article/df_MH_pred_CI.xlsx")
colnames(IHME_predicted)[3] <- "deaths"
IHME_predicted$date = as.Date(IHME_predicted$date)
df_all <- bind_rows(IHME_predicted[-1,],IHME_training,IHME_reported,.id = "Source")
df_all$Source[df_all$Source == "1"] = "Predicted"
df_all$Source[df_all$Source == "2"] = "Training"
df_all$Source[df_all$Source == "3"] = "Observed"
