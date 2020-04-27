#### State-level projection for Inda
data_India_kaggle = read.csv("~/Downloads/covid_19_india.csv")
data_India_kaggle = data_India_kaggle %>% group_by(State.UnionTerritory) %>% summarise(deaths = sum(deaths), Confirmed = sum(Confirmed))

data_India_API = read.csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv")
data_long <- gather(data_India_API, condition, measurement, AN:WB, factor_key=TRUE)
data_long  = data_long %>% group_by(condition,Status) %>% summarise(count = max(measurement))



data_India_API = read.csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv") %>%
  gather(State, measurement, TT:WB, factor_key=TRUE) %>% 
  filter(State!= "TT") %>%
  spread(Status, measurement)

moving_avg = function(x)
{
  y = rollmean(x, 3, fill = NA)
  y[1] = y[2] - ((y[4]-y[2])/2)
  length.y = length(y)
  y[length.y] = y[length.y-1] + (y[length.y-1] - y[length.y-3])/2
  return(y)
}

data_maharashtra = data_India_API %>%
    filter(State == "MH")  %>%
    mutate(Date = gsub("Apr","04", as.character(Date))) %>%
    mutate(Date = gsub("Mar","03", as.character(Date))) %>%
    mutate(Date = as.Date(Date, format = "%d-%m-%y")) %>%
    arrange_at("Date") %>%
    mutate(cumulative_deaths = cumsum(Deceased), population = 114.2, death_rate = cumulative_deaths/population, log_death_rate = log(death_rate)) %>%
    filter(death_rate >= 0.17) %>%
    mutate(smooth = moving_avg(log_death_rate), group = "Maharashtra", social_distance = 0)
data_maharashtra$time = 1:(nrow(data_maharashtra))
data_maharashtra$measurement_std = (nrow(data_maharashtra):1)/100
data_maharashtra$intercept = rep(1,nrow(data_maharashtra))
write.xlsx(data_maharashtra, "~/Dropbox/COVID_19/data/IHME/India/df_Maharashtra_datasettillApril22.xlsx")

data_GJ = data_India_API %>%
  filter(State == "GJ")  %>%
  mutate(Date = gsub("Apr","04", as.character(Date))) %>%
  mutate(Date = gsub("Mar","03", as.character(Date))) %>%
  mutate(Date = as.Date(Date, format = "%d-%m-%y")) %>%
  arrange_at("Date") %>%
  mutate(cumulative_deaths = cumsum(Deceased), population = 62.7, death_rate = cumulative_deaths/population, log_death_rate = log(death_rate)) %>%
  filter(death_rate >= 0.31) %>%
  mutate(smooth = moving_avg(log_death_rate), group = "Gujarat", social_distance = 0)
data_GJ$time = 1:(nrow(data_GJ))
data_GJ$measurement_std = (nrow(data_GJ):1)/100
data_GJ$intercept = rep(1,nrow(data_GJ))
write.xlsx(data_GJ, "~/Dropbox/COVID_19/data/IHME/India/df_Gujarat_datasettillApril22.xlsx")
