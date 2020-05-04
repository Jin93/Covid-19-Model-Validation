setwd("Covid-19-Model-Validation")
#### Data Format Adjustment for Tableau Input
DataFormAdjust <-function(df,name){
  # remove the first column
  df<-df[,-1]
  # change date format
  df$date<-format(as.Date(df$date),"%m/%d/%y")
  # get the data framework 
  # with training or observed data
  dfnew<-df[,-3]
  dfnew<-dfnew[-which(is.na(dfnew$observed)),]
  # get the data framework 
  # with predicted data
  dfpred<-df[,-2]
  dfpred<-dfpred[-which(dfpred$table_phase == 
                          "Training"),]
  dfpred$table_phase<-"Predicted"
  
  # match the colnames
  colnames(dfnew)<-c("date","deaths",
                     "PI_low","PI_upper","Source")
  colnames(dfpred)<-c("date","deaths",
                      "PI_low","PI_upper","Source")
  # combine predicted and observed data
  dffinal<-rbind(dfpred,dfnew)
  rownames(dffinal)<-c()
  dffinal$Training<-name
  dffinal$Source[which(dffinal$Source == 
                         "Validation")]<-"Observed"
  # remove the confidence interval for 
  # observed and training
  dffinal$PI_upper[which(dffinal$Source != 
                           "Predicted")]<-NA
  dffinal$PI_low[which(dffinal$Source != 
                         "Predicted")]<-NA
  dffinal<-dffinal[c(6,5,1,2,4,3)]
  match_index<-max(which(dffinal$Source == 
                           "Training"))
  
  match_df<-dffinal[match_index,]
  match_df$Source<-"Observed"
  dffinal<-rbind(dffinal[c(1:match_index),],
                 match_df,dffinal[c((1+match_index)
                                    :nrow(dffinal)),])
  
  return(dffinal)
}

# read results from ICL models for italy
it0<-read.csv("italy_0week_beforepeak_ICL.csv")
it1<-read.csv("italy_1week_beforepeak_ICL.csv")
it2<-read.csv("italy_2week_beforepeak_ICL.csv")

# change the data format
it0final<-DataFormAdjust(it0,"Till peak")
it1final<-DataFormAdjust(it1,"7 days before peak")
it2final<-DataFormAdjust(it2,"14 days before peak")

# write into xlsx file
library(xlsx)
write.xlsx(itfinal,"df_Italy_ICL.xlsx")

# combine data framework
itfinal<-rbind(it0final,it1final,it2final)

# read results from ICL models for NewYork
ny0<-read.csv("ICL_NY_0week_beforepeak_long_projection.csv")
ny1<-read.csv("ICL_NY_1week_beforepeak_long_projection.csv")
ny2<-read.csv("ICL_NY_2week_beforepeak_long_projection.csv")

# change the data format
ny0final<-DataFormAdjust(ny0,"Till peak")
ny1final<-DataFormAdjust(ny1,"7 days before peak")
ny2final<-DataFormAdjust(ny2,"14 days before peak")

# combine data framework
nyfinal<-rbind(ny0final,ny1final,ny2final)

library(xlsx)
write.xlsx(nyfinal,"df_NY_ICL.xlsx")
