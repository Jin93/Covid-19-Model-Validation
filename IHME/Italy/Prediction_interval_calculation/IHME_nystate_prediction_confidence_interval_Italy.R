# set path to Covid-19-Model-Validation folder
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(ggpubr)
library(openxlsx)
#-----Jacobian ------#
g_prime = function(params, t, s)
{
  p = exp(params[4])
  beta_0 = params[2]
  beta_1 = params[3]
  alpha = exp(params[1])
  d_alpha = p*dnorm(alpha*(t - beta_0 - beta_1*s)) * alpha *(t - beta_0 - beta_1*s)
  d_beta_0 = -p*dnorm(alpha*(t -  beta_0 - beta_1*s)) * alpha
  d_beta_1 = -p*dnorm(alpha*(t -  beta_0 - beta_1*s)) * alpha*s
  d_p = p*pnorm(alpha*(t-beta_0 - beta_1*s)) 
  
  return(c(d_alpha, d_beta_0, d_beta_1, d_p)/d_p)
}


#----Function to calculate variance using delta method------#
delta_var = function(t, s, hessian_inv_matrix, params)
{
  d = (g_prime(params, t, s) %*% hessian_inv_matrix %*% g_prime(params, t, s)) 
  return(d)
}



params = read.csv("IHME/CurveFit/parameters/df14_Italy_param_v1.csv", header = F)
params = as.numeric(params[1,])
df14_Italy_pred = xlsx::read.xlsx2("IHME/CurveFit/IHME_output/df14_Italy_pred.xlsx", sheetIndex = 1)
df14_Italy_pred$death_pred = as.numeric(df14_Italy_pred$death_pred)
df14_Italy_pred$time = as.numeric(df14_Italy_pred$time)
df14_Italy_train = read.xlsx("IHME/CurveFit/data/df14_Italy_v1.xlsx")
df14_Italy_train$date = convertToDate(df14_Italy_train$date)
measurement_error = 0.01

J =matrix(NA, nrow(df14_Italy_train), 4)
for(t in 1:nrow(df14_Italy_train))
{
  J[t, ] = g_prime(params = params, t, df14_Italy_train$social_distance[t])
}
Sigma = diag(df14_Italy_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))
hessian_inv_matrix =  (hessian_inv_matrix + t(hessian_inv_matrix))/2

var1 = rep(NA, nrow(df14_Italy_pred))
for(i in 1: length(var1))
{
  var1[i] = delta_var(as.numeric(df14_Italy_pred$time[i]), 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var1  + measurement_error^2

mu = log(exp(params[4])* pnorm(exp(params[1])*(df14_Italy_pred$time - params[2])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_cumulative_death = 60.36*sqrt(var_log_normal)
se_daily_death =   60.36*sqrt(var_log_normal + lag(var_log_normal,1))

df14_Italy_cum_pred = df14_Italy_pred %>% mutate(SE = sqrt(var)) %>%
  mutate(daily_deaths_pred = death_pred - lag(death_pred)) %>%
  mutate(PI_upper = 60.36*exp(log(death_pred/60.36) + 1.96*SE), PI_low = 60.36*exp(log(death_pred/60.36)  - 1.96*SE))

plot(df14_Italy_cum_pred$death_pred, type = "l", ylim = c(0, max(df14_Italy_cum_pred$PI_upper, na.rm = T)), ylab = "Cumulative predicted deaths", xlab = "time index")
lines(df14_Italy_cum_pred$PI_upper, type = "b")
lines(df14_Italy_cum_pred$PI_low, type = "b")

openxlsx::write.xlsx(df14_Italy_cum_pred, "IHME/Italy/Prediction_interval_calculation/df14_Italy_pred_CI.xlsx")

#----7days----#
params = read.csv("IHME/CurveFit/parameters/df7_Italy_param_v1.csv", header = F)
params = as.numeric(params[1,])
df7_Italy_pred = xlsx::read.xlsx2("IHME/CurveFit/IHME_output/df7_Italy_pred.xlsx", sheetIndex = 1)
df7_Italy_pred$death_pred = as.numeric(df7_Italy_pred$death_pred)
df7_Italy_pred$time = as.numeric(df7_Italy_pred$time)
df7_Italy_train = read.xlsx("IHME/CurveFit/data/df7_Italy_v1.xlsx")
df7_Italy_train$date = convertToDate(df7_Italy_train$date)

measurement_error = 0.01

J =matrix(NA, nrow(df7_Italy_train), 4)
for(t in 1:nrow(df7_Italy_train))
{
  J[t, ] = g_prime(params = params, t, df7_Italy_train$social_distance[t])
}
Sigma = diag(df7_Italy_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))
hessian_inv_matrix =  (hessian_inv_matrix + t(hessian_inv_matrix))/2

var1 = rep(NA, nrow(df7_Italy_pred))
for(i in 1: length(var1))
{
  var1[i] = delta_var(df7_Italy_pred$time[i], 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var1  + measurement_error^2

mu = log(exp(params[4])* pnorm(exp(params[1])*(df14_Italy_pred$time - params[2])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_cumulative_death = 60.36*sqrt(var_log_normal)
se_daily_death =   60.36*sqrt(var_log_normal + lag(var_log_normal,1))

df7_Italy_cum_pred = df7_Italy_pred %>% mutate(SE = sqrt(var)) %>%
  mutate(daily_deaths_pred = death_pred - lag(death_pred)) %>%
  mutate(PI_upper = 60.36*exp(log(death_pred/60.36) + 1.96*SE), PI_low = 60.36*exp(log(death_pred/60.36)  - 1.96*SE))

plot(df7_Italy_cum_pred$death_pred, type = "l", ylim = c(0, max(df7_Italy_cum_pred$PI_upper, na.rm = T)))
lines(df7_Italy_cum_pred$PI_upper, type = "b")
lines(df7_Italy_cum_pred$PI_low, type = "b")
openxlsx::write.xlsx(df7_Italy_cum_pred, "IHME/Italy/Prediction_interval_calculation/df7_Italy_pred_CI.xlsx")



#----0days----#

params = read.csv("IHME/CurveFit/parameters/df0_Italy_param_v1.csv", header = F)
params = as.numeric(params[1,])
df0_Italy_pred = xlsx::read.xlsx2("IHME/CurveFit/IHME_output/df0_Italy_pred.xlsx", sheetIndex = 1)
df0_Italy_pred$death_pred = as.numeric(df0_Italy_pred$death_pred)
df0_Italy_pred$time = as.numeric(df0_Italy_pred$time)
df0_Italy_train = read.xlsx("IHME/CurveFit/data/df0_Italy_v1.xlsx")
df0_Italy_train$date = convertToDate(df0_Italy_train$date)

measurement_error = 0.01

J =matrix(NA, nrow(df0_Italy_train), 4)
for(t in 1:nrow(df0_Italy_train))
{
  J[t, ] = g_prime(params = params, t, df0_Italy_train$social_distance[t])
}
Sigma = diag(df0_Italy_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))
hessian_inv_matrix =  (hessian_inv_matrix + t(hessian_inv_matrix))/2
var1 = rep(NA, nrow(df0_Italy_pred))
for(i in 1: length(var1))
{
  var1[i] = delta_var(df0_Italy_pred$time[i], 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var1  + measurement_error^2
mu = log(exp(params[4])* pnorm(exp(params[1])*(df0_Italy_pred$time - params[1])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_daily_death =   60.36*sqrt(var_log_normal + lag(var_log_normal,1))

df0_Italy_cum_pred = df0_Italy_pred %>% mutate(SE = sqrt(var)) %>%
  mutate(daily_deaths_pred = death_pred - lag(death_pred)) %>%
  mutate(PI_upper = 60.36*exp(log(death_pred/60.36) + 1.96*SE), PI_low = 60.36*exp(log(death_pred/60.36)  - 1.96*SE))

plot(df0_Italy_cum_pred$death_pred, type = "l", ylim = c(0, max(df0_Italy_cum_pred$PI_upper, na.rm = T)))
lines(df0_Italy_cum_pred$PI_upper, type = "b")
lines(df0_Italy_cum_pred$PI_low, type = "b")
openxlsx::write.xlsx(df0_Italy_cum_pred, "IHME/Italy/Prediction_interval_calculation/df0_Italy_pred_CI.xlsx")







