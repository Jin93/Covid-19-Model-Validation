setwd("Covid-19-Model-Validation/ICL")

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
it0<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_0week_constraint_4chains/italy_0week_constraint_4chains.csv")
it1<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_1week_constraint_4chains/italy_1week_constraint_4chains.csv")
it2<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_2week_constraint_4chains/italy_2week_constraint_4chains.csv")

# change the data format
it0final<-DataFormAdjust(it0,"Till peak")
it1final<-DataFormAdjust(it1,"7 days before peak")
it2final<-DataFormAdjust(it2,"14 days before peak")

# write into xlsx file
library(xlsx)
write.xlsx(itfinal,"/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/df_Italy_ICL.xlsx")

# combine data framework
itfinal<-rbind(it0final,it1final,it2final)

# read results from ICL models for NewYork
ny0<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_0week_constraint_4chains/NY_0week_constraint_4chains.csv")
ny1<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_1week_constraint_4chains/NY_1week_constraint_4chains.csv")
ny2<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_2week_constraint_4chains/NY_2week_constraint_4chains.csv")

# change the data format
ny0final<-DataFormAdjust(ny0,"Till peak")
ny1final<-DataFormAdjust(ny1,"7 days before peak")
ny2final<-DataFormAdjust(ny2,"14 days before peak")

# combine data framework
nyfinal<-rbind(ny0final,ny1final,ny2final)

library(xlsx)
write.xlsx(nyfinal,"/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/df_NY_ICL.xlsx")




############################### NY, till May1 observed

# read results from ICL models for NewYork
ny0<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_0week_constraint_4chains/NY_0week_constraint_4chains.csv")
ny1<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_1week_constraint_4chains/NY_1week_constraint_4chains.csv")
ny2<-read.csv("/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/ita_ny_2week_constraint_4chains/NY_2week_constraint_4chains.csv")

# change the data format
ny0final<-DataFormAdjust(ny0,"Till peak")
ny1final<-DataFormAdjust(ny1,"7 days before peak")
ny2final<-DataFormAdjust(ny2,"14 days before peak")

# combine data framework
nyfinal<-rbind(ny0final,ny1final,ny2final)

library(xlsx)
write.xlsx(nyfinal,"/Users/yiwang/Dropbox/cvd/cvd_N/code_final_format/fig/df_NY_ICL_v2.xlsx")

