# library(ggplot2)
# library(matrixStats)
# library(scales)
# library(ggpubr)

# source("utils/geom-stepribbon.r")
# i = 1 # 1st country, F
# countries=c("Italy","France","Spain")

# N <- length(dates[[i]])
# predicted_cases <- colMeans(prediction[,,i])

### use data in github, cvd19_model
#out_cvd19_model=run_covid19model(JHU_data="FALSE",name_save="/Users/yiwang/Dropbox/cvd/cvd_N/model/output/all_out_githubData.RData")
#JHU_data=TRUE
#source("/Users/yiwang/Dropbox/cvd/cvd_N/model/code/April_20/base_april20.R")
#setwd("/Users/yiwang/Dropbox/cvd/cvd_N/model/covid19model-master")
out_cvd19_model=list(reported_cases=reported_cases,prediction=prediction,
                     EpidemicStart=stan_data$EpidemicStart,
                     date_latest=d$DateRep[length(d$DateRep)],
                     date_model = date_model,fit=fit,out=out,
                     deaths_by_country=deaths_by_country,
                     stan_data=stan_data,df_case=df_case,
                     E_deaths_all=E_deaths_all)

save(out_cvd19_model,file="/Users/yiwang/Dropbox/cvd/cvd_N/model/output/April_21/out_cvd19_model_NY_1weekbeforepeak.RData")


### plot, jhu data
data_sourse_='jhu'
dir_figure="/Users/yiwang/Dropbox/cvd/cvd_N/model/fig/April_24_NY_1weekbeforepeak/"
dir.create(dir_figure)
load("/Users/yiwang/Dropbox/cvd/cvd_N/model/output/April_21/out_cvd19_model_jhuData_NY_1weekbeforepeak.RData")

out_model = out_cvd19_model;dir_fig=dir_figure;data_sourse=data_sourse_


countries_sub=countries <- c(
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
  "Netherlands",
  "NY"
)

#get_plot<-function(out_model,data_sourse,dir_fig,name_data_sourse,countries){
  
  prediction = out_model$prediction
  reported_cases = out_model$reported_cases
  out = out_model$out
  #reported_deaths = (out_model$df_case)$Deaths
  
  # dates that are available for validation
  # date_vaildation=out_model$date_model[[14]]
  
  #for(i in 1:length(countries)){
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
    #variance of estimated death
    var_est_deaths=colVars(out_model$E_deaths_all[,,i])
    var_est_deaths=var_est_deaths[which(! as.Date(date_training_projection,format="%Y-%m-%d") < 
                                          as.Date("2020-03-01",format="%Y-%m-%d"))]

    # get true death rate,in the full reported data, since the beginning date "02-29-2020"
    # till the most recent available date
    
    load("/Users/yiwang/Dropbox/cvd/cvd_N/model/data/data_merge_JHU_NY.RData")# data_merge_JHU_NY
    df_death_country=data_merge_JHU_NY[which(data_merge_JHU_NY$Countries.and.territories=="NY"),]
    df_death_country=df_death_country[which(!as.Date(df_death_country$DateRep,format="%d/%m/%Y")<
                                              as.Date("2020-02-29",format="%Y-%m-%d")),]
    df_death_country=df_death_country[which(!as.Date(df_death_country$DateRep,format="%d/%m/%Y")>
                                              as.Date("2020-07-15",format="%Y-%m-%d")),]
    reported_deaths_country=df_death_country$Deaths
    
    
    ## construct table
    library(resample)
    table_est_death=estimated.deaths
    table_true_death=reported_deaths_country
    table_true_death=c(table_true_death,rep("NA",length(table_est_death)-length(reported_deaths_country)))
    table_l_est_death=table_est_death-1.96*sqrt(var_est_deaths)
    table_h_est_death=table_est_death+1.96*sqrt(var_est_deaths)
    table_l_est_death[which(table_l_est_death<0)]=0
    
    table_phase=c(rep("Training",length(date_training_plot)),
                  rep("Validation",length(table_l_est_death)-length(date_training_plot)))
      
    table=data.frame(date=table_dates,observed=table_true_death,estmate=table_est_death,
                     estmate.l=table_l_est_death,estmate.h=table_h_est_death,
                     table_phase=table_phase)
    
    write.csv(table,file=paste0(dir_fig,"NY_1week_beforepeak_long_projection.csv"))

    ylim_=range(as.numeric(c(table_est_death,
               table_l_est_death,table_h_est_death)))
    par(mar=c(1,1,1,1))
    pdf(paste0(dir_fig,country,"_",data_sourse,"_death_long_pro_1week_NY.pdf"))
    plot(table_true_death[(table_true_death)!="NA"],ylim=ylim_,xlim=c(0,length(table_est_death)))
    lines(table_est_death,col="red")
    lines(table_l_est_death,col="red")
    lines(table_h_est_death,col="red")
    dev.off()

  }
 

