# load packages
library(anytime)
library(rjson)

result <- fromJSON(file = "https://api.covid19india.org/states_daily.json")
#result$states_daily[[1]]
#result$states_daily[[2]]
#result$states_daily[[3]]
# These three are for the same date
# 1: Confirmed 
# 2: Recovered
# 3: Decreased
# mh: Maharastra
# gj: Gujrat

# 40 needs to be adjusted if it is 
# used to read current date
state_dat<-function(state){
  if (state == "mh"){
    df<-sapply(c(1:40), function(i){
      c(result$states_daily[[i*3]]$date,
        result$states_daily[[i*3]]$mh)
    })
  }else{
    if (state == "gj"){
      df<-sapply(c(1:40), function(i){
        c(result$states_daily[[i*3]]$date,
          result$states_daily[[i*3]]$gj)
      })
    }else{
      return("please use new func")
    }
  }
  
  df<-t(df)
  df<-data.frame("date"=df[,1],"deaths" = 
                   df[,2])
  rownames(df)<-c()
  # change from 14-Mar-20 to 2020-03-14
  df$date<-paste(df$date,"20",sep = "")
  df$date<-anytime::anydate(df$date)
  # convert deaths to int
  df$deaths<-as.numeric(as.character(df$deaths))
  return(df)
}

# Maharastra: mh
# Gujrat: gj
mh<-state_dat("mh")
gj<-state_dat("gj")
