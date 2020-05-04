setwd("Covid-19-Model-Validation/ICL")

library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(optparse)

StanModel='base'
DEBUG=TRUE

countries_Eu <- c(
  "Denmark",
  "Italy",
  "Germany",
  "Spain",
  #"United_Kingdom",
  "France",
  "Norway",
  "Belgium",
  "Austria",
  "Sweden",
  "Switzerland",
  "Greece",
  "Portugal",
  "Netherlands"
)

## Reading all data
## since here we are not the data from ICL github, 
## we only need to use the name of the imput data
## for convenience of data formatting 
d=readRDS('data/COVID-19-up-to-date.rds')
d=d[which(d$Countries.and.territories %in% countries_Eu),]
names_d=names(d)

## load cumulative number of cases from JHU data
format_data_process<-function(data_URL){
  data_county_level_JHU = read.csv(data_URL, na.strings = c("NA"), header = T)
  colnames(data_county_level_JHU) = c(colnames(data_county_level_JHU)[1:4],
                                      substr(colnames(data_county_level_JHU)[5:ncol(data_county_level_JHU)],2,20))
  data_county_level_JHU = data_county_level_JHU[which(data_county_level_JHU$Country.Region %in% countries_Eu),]
  drops = c("Lat","Long","Province.State")
  data_county_level_JHU  = data_county_level_JHU[ , !(names(data_county_level_JHU) %in% drops)]
  
  name_country=number=c()
  for(i in 1:length(countries_Eu)){
    country_tmp=countries_Eu[i]
    df_infect_tmp=colSums(data_county_level_JHU[which(data_county_level_JHU$Country.Region==country_tmp),-1])
    #country name
    len_date = length(df_infect_tmp)
    name_country = c(name_country,rep(country_tmp,len_date))
    #case number
    number = c(number,df_infect_tmp)
    
  }
  
  date = names(data_county_level_JHU)[-1]
  date_report = rep(date,length(countries_Eu))
  
  df_format = data.frame(number=number, country=name_country,date=date_report)
  
  #### add more variables, for infection data
  df_format$Cases = df_format$number
  df_format$Countries.and.territories = df_format$country
  
  list_date = as.character(df_format$date)
  df_format  = df_format[ , !(names(df_format) %in% c("number","country","date"))]
  
  list_date2 = strsplit(list_date,split=".", fixed = TRUE)
  list_date3=matrix(unlist(list_date2),ncol=3,byrow = T)
  df_format$day = list_date3[,2]
  df_format$day[which(as.numeric(df_format$day)<10)]=paste0(0,df_format$day[which(as.numeric(df_format$day)<10)])
  df_format$year = list_date3[,3]
  df_format$year = paste0("20",df_format$year)
  df_format$month = list_date3[,1]
  df_format$month[which(as.numeric(df_format$month)<10)]=paste0(0,df_format$month[which(as.numeric(df_format$month)<10)])
  df_format$DateRep = paste0(df_format$day,"/",df_format$month,"/",df_format$year)
  df_format
}
county_level_url_JHU = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
df_case = format_data_process(county_level_url_JHU)

for(i in 1:length(countries_Eu)){
  country_tmp=countries_Eu[i]
  df_tmp=df_case[which(df_case$Countries.and.territories==country_tmp),]
  df_tmp$Cases=c(0,diff(df_tmp$Cases))
  df_case[which(df_case$Countries.and.territories==country_tmp),] = df_tmp
}

## load cumulative number of cases from JHU data
format_data<-function(data_URL){
  data_county_level_JHU = read.csv(data_URL, na.strings = c("NA"), header = T)
  colnames(data_county_level_JHU) = c(colnames(data_county_level_JHU)[1:4],substr(colnames(data_county_level_JHU)[5:ncol(data_county_level_JHU)],2,20))
  data_county_level_JHU = data_county_level_JHU[which(data_county_level_JHU$Country.Region %in% countries_Eu),]
  drops = c("Lat","Long","Province.State")
  data_county_level_JHU  = data_county_level_JHU[ , !(names(data_county_level_JHU) %in% drops)]
  # df_infect<-data.frame(DateRep="",Cases=0,country="")
  
  name_country=number=c()
  for(i in 1:length(countries_Eu)){
    country_tmp=countries_Eu[i]
    df_tmp=colSums(data_county_level_JHU[which(data_county_level_JHU$Country.Region==country_tmp),-1])
    #country name
    len_date = length(df_tmp)
    name_country = c(name_country,rep(country_tmp,len_date))
    #case number
    number = c(number,df_tmp)
  }
  
  date = names(data_county_level_JHU)[-1]
  date_report = rep(date,length(countries_Eu))
  
  df_format = data.frame(number=number, country=name_country,date=date_report)
  date = names(data_county_level_JHU)[-1]
  list_date = as.character(df_format$date)
  list_date2 = strsplit(list_date,split=".", fixed = TRUE)
  list_date3=matrix(unlist(list_date2),ncol=3,byrow = T)
  
  df_format$day = list_date3[,2]
  df_format$day[which(as.numeric(df_format$day)<10)]=paste0(0,df_format$day[which(as.numeric(df_format$day)<10)])
  df_format$year = list_date3[,3]
  df_format$year = paste0("20",df_format$year)
  df_format$month = list_date3[,1]
  df_format$month[which(as.numeric(df_format$month)<10)]=paste0(0,df_format$month[which(as.numeric(df_format$month)<10)])
  df_format$DateRep = paste0(df_format$day,"/",df_format$month,"/",df_format$year)
  
  df_format
}
county_level_url_JHU = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
df_death = format_data(county_level_url_JHU)
# error in German data: death number on 4.11.20 is smaller than 4.10.20
# number     country    date day year month    DateRep
# 282   2767 Germany 4.10.20  10 2020    04 10/04/2020
# 283   2736 Germany 4.11.20  11 2020    04 11/04/2020

# get increased death for reach date for each country
for(i in 1:length(countries_Eu)){
  country_tmp=countries_Eu[i]
  df_tmp=df_death[which(df_death$country==country_tmp),]
  df_tmp$number=c(0,diff(df_tmp$number))
  df_death[which(df_death$country==country_tmp),] = df_tmp
}

# change the increased death number on the 
# date of wrong death number of German data to be 0
df_death$number[which(df_death$number<0)]=0

# combine infection data and death data
df_case$Deaths = df_death$number

# for italy, only keep the data before till(included) peak of death 3-27-2020
# for italy, only keep the data after(included) the 1st day with deaths number >=10
d_italy=df_case[which(df_case$Countries.and.territories=="Italy" ),]
d_italy=d_italy[which(as.Date(d_italy$DateRep,format="%d/%m/%Y") < as.Date("2020-03-28",format="%Y-%m-%d")),]
d_italy=d_italy[which(! as.Date(d_italy$DateRep,format="%d/%m/%Y")<as.Date("2020-03-02")),]
# Cases Countries.and.territories day year month    DateRep Deaths
# 140   240                     Italy  29 2020    02 29/02/2020      8
# 141   566                     Italy  01 2020    03 01/03/2020      5
# 142   342                     Italy  02 2020    03 02/03/2020     18
# 143   466                     Italy  03 2020    03 03/03/2020     27
df_case=df_case[which(df_case$Countries.and.territories!="Italy"),]

df_case=rbind(df_case,d_italy)

# keep training data since 2-29(included),till 4-15(included)
df_case=df_case[which(! as.Date(df_case$DateRep,format="%d/%m/%Y")<as.Date("2020-02-29")),]
df_case=df_case[which(as.Date(df_case$DateRep,format="%d/%m/%Y")<as.Date("2020-04-16")),]
# > head(df_case)
# Cases Countries.and.territories day year month    DateRep Deaths
# 1     0                   Denmark  22 2020    01 22/01/2020      0
# 2     0                   Denmark  23 2020    01 23/01/2020      0
# 3     0                   Denmark  24 2020    01 24/01/2020      0

######## NY data
# load NY data 
load("data/data_merge_JHU_NY.RData")# data_merge_JHU_NY, from NY time

# keep NY data till peak
# NY peak date of death="4/07/2020"
last_date_training_NY="2020-04-07"
last_date_training_NY=as.Date(last_date_training_NY,format="%Y-%m-%d")
df_NY_training=data_merge_JHU_NY[which(! as.Date(data_merge_JHU_NY$DateRep, format = "%d/%m/%Y") > 
                                         last_date_training_NY), ]
df_NY_training=data_merge_JHU_NY[which(! as.Date(data_merge_JHU_NY$DateRep, format = "%d/%m/%Y") <
                                         as.Date("2020-03-18",format="%Y-%m-%d")), ]
df_case=rbind(df_case[,names(df_NY_training)],df_NY_training)


d=df_case

countries <- c(
  "Denmark",
  "Italy",
  "Germany",
  "Spain",
  #"United_Kingdom",
  "France",
  "Norway",
  "Belgium",
  "Austria",
  "Sweden",
  "Switzerland",
  "Greece",
  "Portugal",
  "Netherlands","NY")

##################
## get IFR and population from same file
#ifr.by.country = read.csv("data/popt_ifr.csv")
ifr.by.country = read.csv("data/pop_ifr_edited.csv")

######### added, Yi
ifr.by.country = ifr.by.country[which(ifr.by.country$country %in% countries),]
#########

ifr.by.country$country = as.character(ifr.by.country[,2])
ifr.by.country$country[ifr.by.country$country == "United Kingdom"] = "United_Kingdom"

serial.interval = read.csv("data/serial_interval.csv") ##?

N2 = 110 # increase if you need more forecast
serial.interval_X=1:N2
serial.interval_fit=array(0,dim=N2)
serial.interval_fit[1:100]=serial.interval$fit
serial.interval=data.frame(X=serial.interval_X,fit=serial.interval_fit)

#covariates = read.csv('data/interventions.csv', stringsAsFactors = FALSE)
covariates = read.csv('data/interventions_edited.csv', stringsAsFactors = FALSE)
####
covariates = covariates[which(covariates$Country %in% countries),]
#########

names_covariates = c('Schools + Universities','Self-isolating if ill', 'Public events', 'Lockdown', 'Social distancing encouraged')
covariates <- covariates %>%
  filter((Type %in% names_covariates))
covariates <- covariates[,c(1,2,4)]
covariates <- spread(covariates, Type, Date.effective)
names(covariates) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
covariates <- covariates[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
covariates$schools_universities <- as.Date(covariates$schools_universities, format = "%d.%m.%Y")
covariates$lockdown <- as.Date(covariates$lockdown, format = "%d.%m.%Y")
covariates$public_events <- as.Date(covariates$public_events, format = "%d.%m.%Y")
covariates$self_isolating_if_ill <- as.Date(covariates$self_isolating_if_ill, format = "%d.%m.%Y")
covariates$social_distancing_encouraged <- as.Date(covariates$social_distancing_encouraged, format = "%d.%m.%Y")

## using covariates as dates we want
covariates$schools_universities[covariates$schools_universities > covariates$lockdown] <- covariates$lockdown[covariates$schools_universities > covariates$lockdown]
covariates$public_events[covariates$public_events > covariates$lockdown] <- covariates$lockdown[covariates$public_events > covariates$lockdown]
covariates$social_distancing_encouraged[covariates$social_distancing_encouraged > covariates$lockdown] <- covariates$lockdown[covariates$social_distancing_encouraged > covariates$lockdown]
covariates$self_isolating_if_ill[covariates$self_isolating_if_ill > covariates$lockdown] <- covariates$lockdown[covariates$self_isolating_if_ill > covariates$lockdown]

forecast = 0

dates = list()
reported_cases = list()
# stan_data = list(M=length(countries),N=NULL,covariate1=NULL,covariate2=NULL,covariate3=NULL,covariate4=NULL,covariate5=NULL,covariate6=NULL,deaths=NULL,f=NULL,
#                  N0=6,cases=NULL,SI=serial.interval$fit[1:N2],
#                  EpidemicStart = NULL, pop = NULL) # N0 = 6 to make it consistent with Rayleigh
# 

stan_data = list(M=length(countries),N=NULL,covariate1=NULL,covariate2=NULL,
                 covariate3=NULL,covariate4=NULL,covariate5=NULL,covariate6=NULL,
                 deaths=NULL,f=NULL, N0=6,cases=NULL,SI=serial.interval$fit[1:N2],
                 EpidemicStart = NULL, pop = NULL) # N0 = 6 to make it consistent with Rayleigh
######### 

deaths_by_country = list()

# various distributions required for modeling
mean1 = 5.1; cv1 = 0.86; # infection to onset
mean2 = 18.8; cv2 = 0.45 # onset to death
x1 = rgammaAlt(1e7,mean1,cv1) # infection-to-onset distribution
x2 = rgammaAlt(1e7,mean2,cv2) # onset-to-death distribution

ecdf.saved = ecdf(x1+x2)
date_model=list();  count=1

for(Country in countries) {
  IFR=ifr.by.country$ifr[ifr.by.country$country == Country]
  
  covariates1 <- covariates[covariates$Country == Country, c(2,3,4,5,6)]
  
  d1_pop = ifr.by.country[ifr.by.country$country==Country,]
  d1=d[d$Countries.and.territories==Country,]
  
  ##### added by Yi
  names_extract=names_d[c(1,5,6,7)]
  d1 = d[d$Countries.and.territories==Country,names_extract]
  
  ##### 
  
  d1$date = as.Date(d1$DateRep,format='%d/%m/%Y')
  # d1$t = decimal_date(d1$date) # edited by Yi
  # d1=d1[order(d1$t),] # edited by Yi
  
  date_min <- dmy('31/12/2019') 
  if (as.Date(d1$DateRep[1], format='%d/%m/%Y') > as.Date(date_min, format='%d/%m/%Y')){
    print(paste(Country,'In padding'))
    pad_days <- as.Date(d1$DateRep[1], format='%d/%m/%Y') - date_min
    pad_dates <- date_min + days(1:pad_days[[1]]-1)
    # padded_data <- data.frame("Countries.and.territories" = rep(Country, pad_days),
    #                           "DateRep" = format(pad_dates, '%d/%m/%Y'),
    #                           "t" = decimal_date(as.Date(pad_dates,format='%d/%m/%Y')),
    #                           "date" = as.Date(pad_dates,format='%d/%m/%Y'),
    #                           "Cases" = as.integer(rep(0, pad_days)),
    #                           "Deaths" = as.integer(rep(0, pad_days)),
    #                           stringsAsFactors=F)
    # changed by Yi
    padded_data <- data.frame("DateRep" = format(pad_dates, '%d/%m/%Y'),
                              "Cases" = as.integer(rep(0, pad_days)),
                              "Deaths" = as.integer(rep(0, pad_days)),
                              "Countries.and.territories" = rep(Country, pad_days),
                              
                              "date" = as.Date(pad_dates,format='%d/%m/%Y'))
    # d1 <- bind_rows(padded_data, d1)#edited by Yi
    d1 <- rbind(padded_data, d1)#edited by Yi
  }
  
  index = which(d1$Cases>0)[1]
  index1 = which(cumsum(d1$Deaths)>=10)[1] # also 5
  index2 = index1-30
  
  print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d",index,index2))
  d1=d1[index2:nrow(d1),]
  
  #### added by Yi
  date_model[[count]]=d1$date
  count=count+1
  #### 
  
  ## stan_data$EpidemicStart = c(stan_data$EpidemicStart,index1+1-index2) ##by Yi
  ## added by Yi
  stan_data$EpidemicStart = c(stan_data$EpidemicStart,index2)
  #####
  stan_data$pop = c(stan_data$pop, d1_pop$popt)
  
  for (ii in 1:ncol(covariates1)) {
    covariate = names(covariates1)[ii]
    d1[covariate] <- (as.Date(d1$DateRep, format='%d/%m/%Y') >= as.Date(covariates1[1,covariate]))*1  # should this be > or >=?
  }
  
  dates[[Country]] = d1$date
  # hazard estimation
  N = length(d1$Cases)
  print(sprintf("%s has %d days of data",Country,N))
  forecast = N2 - N
  if(forecast < 0) {
    print(sprintf("%s: %d", Country, N))
    print("ERROR!!!! increasing N2")
    N2 = N
    forecast = N2 - N
  }
  
  # IFR is the overall probability of dying given infection
  convolution = function(u) (IFR * ecdf.saved(u))
  
  f = rep(0,N2) # f is the probability of dying on day i given infection
  f[1] = (convolution(1.5) - convolution(0))
  for(i in 2:N2) {
    f[i] = (convolution(i+.5) - convolution(i-.5)) 
  }
  
  reported_cases[[Country]] = as.vector(as.numeric(d1$Cases))
  deaths=c(as.vector(as.numeric(d1$Deaths)),rep(-1,forecast))
  cases=c(as.vector(as.numeric(d1$Cases)),rep(-1,forecast))
  deaths_by_country[[Country]] = as.vector(as.numeric(d1$Deaths))
  covariates2 <- as.data.frame(d1[, colnames(covariates1)])
  # x=1:(N+forecast)
  covariates2[N:(N+forecast),] <- covariates2[N,]
  
  ## append data
  stan_data$N = c(stan_data$N,N)
  # stan_data$x = cbind(stan_data$x,x)
  stan_data$covariate1 = cbind(stan_data$covariate1,covariates2[,1])
  stan_data$covariate2 = cbind(stan_data$covariate2,covariates2[,2])
  stan_data$covariate3 = cbind(stan_data$covariate3,covariates2[,3])
  stan_data$covariate4 = cbind(stan_data$covariate4,covariates2[,4])
  stan_data$covariate5 = cbind(stan_data$covariate5,covariates2[,4])
  stan_data$covariate6 = cbind(stan_data$covariate6,covariates2[,5])
  stan_data$f = cbind(stan_data$f,f)
  stan_data$deaths = cbind(stan_data$deaths,deaths)
  stan_data$cases = cbind(stan_data$cases,cases)
  
  stan_data$N2=N2
  stan_data$x=1:N2
  if(length(stan_data$N) == 1) {
    stan_data$N = as.array(stan_data$N)
  }
}

# create the `any intervention` covariate
stan_data$covariate4 = 1*as.data.frame((stan_data$covariate1+
                                          stan_data$covariate2+
                                          stan_data$covariate3+
                                          stan_data$covariate5+
                                          stan_data$covariate6) >= 1)
DEBUG=TRUE


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',StanModel,'.stan')) ##model

##########################################################################################
###################################################################,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#######################
##########################################################################################
######## added by Yi
DEBUG=TRUE
########
########added by Yi
#stan_data$pop = c(6.036e7, 6.699e7,4.694e7) 
#  Ns = c(Italy=6.036e7,France=6.699e7,Spain=4.694e7) # population size
########
set.seed(222)


# note: it takes about 5 hours to run 1000 iterations on macbook air.  
fit = sampling(m,data=stan_data,iter=1000,warmup=500,chains=4,thin=4,
               control = list(adapt_delta = 0.95, max_treedepth = 10))
niter_notburning=1000-500


out = rstan::extract(fit)
prediction = out$prediction


out_cvd19_model=list(reported_cases=reported_cases,prediction=prediction,
                     EpidemicStart=stan_data$EpidemicStart,
                     date_latest=d$DateRep[length(d$DateRep)],
                     date_model = date_model,fit=fit,out=out,
                     deaths_by_country=deaths_by_country,
                     stan_data=stan_data,df_death=df_death)

save(out_cvd19_model,file="output/out_cvd19_model_italy_ny_0week_constraint_4chains.RData")
# 
N2=110+40
M=out_cvd19_model$stan_data$M
out = rstan::extract(out_cvd19_model$fit)


kappa_all = out$kappa
y_all=out$y
phi_all=out$phi
tau_all=out$tau
alpha_hier_all=out$alpha_hier
mu_all=out$mu
ifr_noise_all=out$ifr_noise
covariate1=out_cvd19_model$stan_data$covariate1
covariate2=out_cvd19_model$stan_data$covariate2
covariate3=out_cvd19_model$stan_data$covariate3
covariate4=out_cvd19_model$stan_data$covariate4
covariate5=out_cvd19_model$stan_data$covariate5
covariate6=out_cvd19_model$stan_data$covariate6

#the covariate of the last day of training dates
covariate1_added=covariate1[nrow(covariate1),]
for(i in c(1:40)){
  covariate1=rbind(covariate1,covariate1_added)
}

covariate2_added=covariate2[nrow(covariate2),]
for(i in c(1:40)){
  covariate2=rbind(covariate2,covariate2_added)
}

covariate3_added=covariate3[nrow(covariate3),]
for(i in c(1:40)){
  covariate3=rbind(covariate3,covariate3_added)
}

covariate4_added=covariate4[nrow(covariate4),]
for(i in c(1:40)){
  covariate4=rbind(covariate4,covariate4_added)
}

covariate5_added=covariate5[nrow(covariate5),]
for(i in c(1:40)){
  covariate5=rbind(covariate5,covariate5_added)
}

covariate6_added=covariate6[nrow(covariate6),]
for(i in c(1:40)){
  covariate6=rbind(covariate6,covariate6_added)
}


EpidemicStart=out_cvd19_model$EpidemicStart
pop=out_cvd19_model$stan_data$pop
SI=out_cvd19_model$stan_data$SI
SI=c(SI,rep(0,40))

# f= out_cvd19_model$stan_data$f
mat_f=NULL
for(Country in countries) {
  IFR=ifr.by.country$ifr[ifr.by.country$country == Country]
  
  # IFR is the overall probability of dying given infection
  convolution = function(u) (IFR * ecdf.saved(u))
  
  f = rep(0,N2) # f is the probability of dying on day i given infection
  f[1] = (convolution(1.5) - convolution(0))
  for(i in 2:N2) {
    f[i] = (convolution(i+.5) - convolution(i-.5)) 
  }
  mat_f = cbind(mat_f,f)
}

f=mat_f


E_deaths_all=array(dim=c(niter_notburning,110+40,14))
for(iter in 1:niter_notburning){
  kappa = kappa_all[iter]
  y=y_all[iter,]
  phi=phi_all[iter]
  tau=tau_all[iter]
  alpha_hier=alpha_hier_all[iter,]
  mu=mu_all[iter,]
  ifr_noise=ifr_noise_all[iter,]
  
  
  N0 = 6
  
  cumm_sum = E_deaths = Rt = Rt_adj =prediction= array(0,dim=c(N2,M))
  alpha = array(dim=6)
  
  for(i in 1:6){
    alpha[i] = alpha_hier[i] - ( log(1.05) / 6.0 );
  }
  
  
  for (m in 1:M){
    for (i in 2:N0){
      cumm_sum[i,m] = cumm_sum[i-1,m] + y[m];
    }
    prediction[1:N0,m] = rep(y[m],N0); # learn the number of cases in the first N0 days
    
    Rt[,m] = mu[m] * exp( covariate1[,m] * (-alpha[1]) + covariate2[,m] * (-alpha[2]) +
                            covariate3[,m] * (-alpha[3]) + covariate4[,m] * (-alpha[4]) + covariate5[,m] * (-alpha[5]) +
                            covariate6[,m] * (-alpha[6]) );
    Rt_adj[1:N0,m] = Rt[1:N0,m];
    for (i in (N0+1):N2) {
      convolution=0;
      for(j in 1:(i-1)){
        convolution = convolution + prediction[j, m] * SI[i-j];
      }
      cumm_sum[i,m] = cumm_sum[i-1,m] + prediction[i-1,m];
      Rt_adj[i,m] = ((pop[m]-cumm_sum[i,m]) / pop[m]) * Rt[i,m];
      prediction[i, m] = Rt_adj[i,m] * convolution;
    }
    
    E_deaths[1, m]= 1e-15 * prediction[1,m];
    for (i in 2:N2){
      for(j in 1:(i-1)){
        E_deaths[i,m] = E_deaths[i,m] + prediction[j,m] * f[i-j,m] * ifr_noise[m];
      }
    }
  }
  E_deaths_all[iter,,]=E_deaths
}

# #### plot
JHU_data=TRUE
library( resample )


out_cvd19_model=list(reported_cases=reported_cases,prediction=prediction,
                     EpidemicStart=stan_data$EpidemicStart,
                     date_latest=d$DateRep[length(d$DateRep)],
                     date_model = date_model,fit=fit,out=out,
                     deaths_by_country=deaths_by_country,
                     stan_data=stan_data,df_death=df_death,
                     E_deaths_all=E_deaths_all)
save(out_cvd19_model,file="output/out_cvd19_model_italy_ny_0week_constraint_4chains.RData")


######################################################################## 
######################################################################## 
######################################################################## 
############################## plot Italy
library(resample)
library(timeSeries)

data_sourse_='jhu'
dir_figure="fig/ita_ny_0week_constraint_4chains/"
dir.create(dir_figure)

out_model = out_cvd19_model;dir_fig=dir_figure;data_sourse=data_sourse_


countries=c(
  "Denmark",
  "Italy",
  "Germany",
  "Spain",
  #"United_Kingdom",
  "France",
  "Norway",
  "Belgium",
  "Austria",
  "Sweden",
  "Switzerland",
  "Greece",
  "Portugal",
  "Netherlands","NY"
)


prediction = out_model$prediction
reported_cases = out_model$reported_cases
out = out_model$out
reported_deaths = (out_model$df_death)$number

# dates that are available for validation
date_vaildation=out_model$date_model[[1]]

#for(i in 1:length(countries)){


prediction = out_model$prediction
reported_cases = out_model$reported_cases
out = out_model$out
reported_deaths = (out_model$df_death)$number

# dates that are available for validation
date_vaildation=out_model$date_model[[1]]


for(i in 2){#italy
  country = countries[i]# i=2, Italy
  
  # dates for plot - part of fitting + projection
  table_dates=seq.Date(from=as.Date("2020-02-29"),to=as.Date("2020-05-15"),"day")
  
  ## dates that are used for training(added dates before 2-29-2020
  ## in the modeling procedure) 
  date_training=out_model$date_model[[i]]
  #for plot
  date_training_plot=date_training[which(!as.Date(date_training,format="%Y-%m-%d")< as.Date("2020-02-29",format="%Y-%m-%d"))]
  
  #estimated death, in the dates of the model fitting, including dates not used for plots
  date_training_projection=c(date_training,table_dates[which(! table_dates %in% date_training )] )
  
  # estimated death number, only keep the dates included in plot
  estimated.deaths = colMeans(out_model$E_deaths_all[,,i])
  estimated.deaths=estimated.deaths[which(! as.Date(date_training_projection,format="%Y-%m-%d") < as.Date("2020-02-29",format="%Y-%m-%d"))]
  #variance of estimated death
  var_est_deaths=colVars(out_model$E_deaths_all[,,i])
  var_est_deaths=var_est_deaths[which(! as.Date(date_training_projection,format="%Y-%m-%d") < as.Date("2020-02-29",format="%Y-%m-%d"))]
  
  # get true death rate,in the full reported data, since the beginning date "02-29-2020"
  # till the most recent available date
  df_death=out_model$df_death
  df_death_country=df_death[which(df_death$country=="Italy"),]
  df_death_country=df_death_country[which(!as.Date(df_death_country$DateRep,format="%d/%m/%Y")<as.Date("2020-02-29",format="%Y-%m-%d")),]
  df_death_country=df_death_country[which(!as.Date(df_death_country$DateRep,format="%d/%m/%Y")>as.Date("2020-07-15",format="%Y-%m-%d")),]
  reported_deaths_country=df_death_country$number
  
  
  ## construct table
  table_est_death=estimated.deaths
  table_true_death=reported_deaths_country
  table_true_death=c(table_true_death,rep("NA",length(table_est_death)-length(reported_deaths_country)))
  
  
  table_l_est_death <- colQuantiles(out_model$E_deaths_all[,,i], prob=.025)
  table_h_est_death=colQuantiles(out_model$E_deaths_all[,,i], prob=1-.025)
  
  table_h_est_death=table_h_est_death[which(! as.Date(date_training_projection,format="%Y-%m-%d") < 
                                              as.Date("2020-02-29",format="%Y-%m-%d"))]
  table_l_est_death=table_l_est_death[which(! as.Date(date_training_projection,format="%Y-%m-%d") < 
                                              as.Date("2020-02-29",format="%Y-%m-%d"))]
  
  table_l_est_death[which(table_l_est_death<0)]=0
  
  # table_l_est_death=table_est_death+1.96*sqrt(var_est_deaths)
  # table_l_est_death[which(table_l_est_death<0)]=0
  
  # table_l_est_death=table_est_death-1.96*sqrt(var_est_deaths)
  # table_h_est_death=table_est_death+1.96*sqrt(var_est_deaths)
  # 
  
  table_l_est_death[which(table_l_est_death<0)]=0
  
  table_phase=c(rep("Training",length(date_training_plot)),rep("Validation",length(table_l_est_death)-length(date_training_plot)))
  
  table=data.frame(date=table_dates,observed=table_true_death,estmate=table_est_death,
                   estmate.l=table_l_est_death,estmate.h=table_h_est_death,
                   table_phase=table_phase)
  
  write.csv(table,file=paste0(dir_fig,"italy_0week_constraint_4chains.csv"))
}

ylim_=range(as.numeric(c(table_est_death,
                         table_l_est_death,table_h_est_death)))
par(mar=c(1,1,1,1))
pdf(paste0(dir_fig,"italy0week_constrain_4chains.pdf"))
plot(table_true_death[(table_true_death)!="NA"],ylim=ylim_,xlim=c(0,length(table_est_death)))
lines(table_est_death,col="red")
lines(table_l_est_death,col="red")
lines(table_h_est_death,col="red")
dev.off()



############################################### plot NY

library(resample)
library(timeSeries)


prediction = out_model$prediction
reported_cases = out_model$reported_cases
out = out_model$out

for(i in 14){#NY
  #date_training=out_model$date_model[[16]]
  country = countries[i]# i=2, Italy
  
  # dates for plot - part of fitting + projection
  table_dates=seq.Date(from=as.Date("2020-03-01"),to=as.Date("2020-07-15"),"day")
  
  ## dates that are used for training(added dates before 2-29-2020
  ## in the modeling procedure) 
  date_training=out_model$date_model[[i]]
  #for plot
  date_training_plot=date_training[which(!as.Date(date_training,format="%Y-%m-%d")< 
                                           as.Date("2020-03-01",format="%Y-%m-%d"))]
  
  #estimated death, in the dates of the model fitting, including dates not used for plots
  date_training_projection=c(date_training,table_dates[which(! table_dates %in% date_training )] )
  
  # estimated death number, only keep the dates included in plot
  estimated.deaths = colMeans(out_model$E_deaths_all[,,i])
  estimated.deaths=estimated.deaths[which(! as.Date(date_training_projection,format="%Y-%m-%d") < 
                                            as.Date("2020-03-01",format="%Y-%m-%d"))]
  
  load("data/data_merge_JHU_NY.RData")# data_merge_JHU_NY
  df_death_country=data_merge_JHU_NY[which(data_merge_JHU_NY$Countries.and.territories=="NY"),]
  df_death_country=df_death_country[which(!as.Date(df_death_country$DateRep,format="%d/%m/%Y")<
                                            as.Date("2020-02-29",format="%Y-%m-%d")),]
  df_death_country=df_death_country[which(!as.Date(df_death_country$DateRep,format="%d/%m/%Y")>
                                            as.Date("2020-07-15",format="%Y-%m-%d")),]
  reported_deaths_country=df_death_country$Deaths
  
  library(timeSeries)
  ## construct table
  library(resample)
  table_est_death=estimated.deaths
  table_true_death=reported_deaths_country
  table_true_death=c(table_true_death,rep("NA",length(table_est_death)-length(reported_deaths_country)))
  table_l_est_death <- colQuantiles(out_model$E_deaths_all[,,i], prob=.025)
  table_h_est_death=colQuantiles(out_model$E_deaths_all[,,i], prob=1-.025)
  
  table_h_est_death=table_h_est_death[which(! as.Date(date_training_projection,format="%Y-%m-%d") < 
                                              as.Date("2020-03-01",format="%Y-%m-%d"))]
  table_l_est_death=table_l_est_death[which(! as.Date(date_training_projection,format="%Y-%m-%d") < 
                                              as.Date("2020-03-01",format="%Y-%m-%d"))]
  
  # table_l_est_death=table_est_death+1.96*sqrt(var_est_deaths)
  table_l_est_death[which(table_l_est_death<0)]=0
  
  table_phase=c(rep("Training",length(date_training_plot)),
                rep("Validation",length(table_l_est_death)-length(date_training_plot)))
  
  table=data.frame(date=table_dates,observed=table_true_death,estmate=table_est_death,
                   estmate.l=table_l_est_death,estmate.h=table_h_est_death,
                   table_phase=table_phase)
  
  write.csv(table,file=paste0(dir_fig,"NY_0week_constraint_4chains.csv"))
  
  ylim_=range(as.numeric(c(table_est_death,
                           table_l_est_death,table_h_est_death,table_true_death[(table_true_death)!="NA"])))
  par(mar=c(1,1,1,1))
  pdf(paste0(dir_fig,country,"_",data_sourse,"_0week_NY_constraint_4chains.pdf"))
  plot(table_true_death[(table_true_death)!="NA"],ylim=ylim_,xlim=c(0,length(table_est_death)))
  lines(table_est_death,col="red")
  lines(table_l_est_death,col="red")
  lines(table_h_est_death,col="red")
  dev.off()
  
}



# 

