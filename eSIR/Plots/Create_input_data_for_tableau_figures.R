library(ggExtra)
library(ggplot2)
library(data.table)
library(gapminder)
library(tidyverse)
library(grid)
library(gridExtra) 
dat.tables = list()
setwd(paste0('~/Dropbox/Covid-19/eSIR-result/',region.name))
region.name = 'Italy' # 'New York'
lag.val = 7
lags = c(21,14,7)
for (i in 1:length(lags)){
  dat.tables[[i]] = read.csv(paste0(region.name,'-eSIR-',lag-lag.val,'.csv'))
}

validation.indx = c('14 days before peak','7 days before peak','Till peak')
out = list()
for (l in 1:length(lags)){
  lag = lags[l]
  dat.table = dat.tables[[l]]
  dat.table$date = as.Date(dat.table$date,format='%m/%d/%y')
  dat.table = dat.table[1:(which(dat.table$date=='2020-05-15')),]
  proj.peak.t = which.max(dat.table$observed)
  ### smoothing
  # prediction curve
  smooth.est = predict(loess(dat.table$estimate ~ as.numeric(dat.table$date), span = .4))
  # upper bound
  smooth.ub = predict(loess(dat.table$estimate.ub ~ as.numeric(dat.table$date), span = .4))
  smooth.ub = pmax(0,smooth.ub)
  # lower bound
  smooth.lb = predict(loess(dat.table$estimate.lb ~ as.numeric(dat.table$date), span = .4))
  smooth.lb = pmax(0,smooth.lb)
  
  dat.table$estimate = smooth.est
  dat.table$estimate.lb = smooth.lb
  dat.table$estimate.ub = smooth.ub
  # training
  tr.table = dat.table[1:(sum(dat.table$phase == 'Training')),]
  # 1-week validation (for parameter tuning)
  va.table = dat.table[(sum(dat.table$phase == 'Training')):(proj.peak.t - lags[l] + lag.val),]
  # holdout
  ho.table = dat.table[(proj.peak.t - lags[l] + lag.val):(sum(!is.na(dat.table$observed))),]
  pred.table = dat.table[(proj.peak.t - lags[l] + lag.val):(nrow(dat.table)),]
  
  begin <- chron(dates. = '02/25/2020') # daily cases: starting from the next day

  out.training = data.frame(Source = 'Training',
                            date = tr.table$date,
                            cases = tr.table$observed,
                            PI_upper = NA, PI_low = NA)
  out.validation = data.frame(Source = 'Validation',
                            date = va.table$date,
                            cases = va.table$observed,
                            PI_upper = NA, PI_low = NA)
  out.observed = data.frame(Source = 'Observed',
                            date = ho.table$date,
                            cases = ho.table$observed,
                            PI_upper = NA, PI_low = NA)
  out.predicted = data.frame(Source = 'Predicted',
                             date = pred.table$date,
                             cases = pred.table$estimate,
                             PI_upper = pred.table$estimate.ub, 
                             PI_low = pred.table$estimate.lb)
  out = rbind(out.training,out.validation,out.observed,out.predicted)
  out$cases = round(out$cases,0)
  out$PI_upper = round(out$PI_upper,0)
  out$PI_low = round(out$PI_low,0)
  out = cbind(validation.indx[l],out)
  colnames(out)[1] = 'Training'
  out[[l]] = out
}
Out = rbind(out[[1]],out[[2]],out[[3]])
