# Prediction interval based on mode based uncertainity
decre_fun = function(x)
{
  return(1/(0.1+x^2))
}

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

delta_var = function(t, s, hessian_inv_matrix, params)
{
  d = (g_prime(params, t, s) %*% hessian_inv_matrix %*% g_prime(params, t, s)) 
  return(d)
}

#--------------------New York--------------#

params = read.csv("~/Dropbox/COVID_19/data/IHME/Newyork/df14_NY_param_v1.csv", header = F)
params = as.numeric(params[1,])
#hessian_inv_matrix = read.csv("~/Dropbox/COVID_19/data/IHME/NY/df14_NY_hessinv_v1.csv", header = F)
#hessian_inv_matrix = as.matrix(hessian_inv_matrix[c(1,2,4), c(1,2,4)])
#hessian_inv_matrix = solve(solve(hessian_inv_matrix) + diag(nrow(hessian_inv_matrix)))
df14_NY_pred = read.xlsx("~/Dropbox/COVID_19/results/Medium_article/IHME/df14_NY_pred.xlsx")
df14_NY_train = read.xlsx("~/Dropbox/COVID_19/data/IHME/Newyork/df14_NY_v1.xlsx")
#measurement_error = sapply(df14_NY_pred$time, FUN = decre_fun)
measurement_error = 0.01^2

J =matrix(NA, nrow(df14_NY_train), 4)
for(t in 1:nrow(df14_NY_train))
{
  J[t, ] = g_prime(params = params, t, df14_NY_train$social_distance[t])
}
Sigma = diag(df14_NY_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))
hessian_inv_matrix =  (hessian_inv_matrix + t(hessian_inv_matrix))/2

var1 = rep(NA, nrow(df14_NY_pred))
for(i in 1: length(var1))
{
  var1[i] = delta_var(df14_NY_pred$time[i], 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var1  + measurement_error

mu = log(exp(params[4])* pnorm(exp(params[1])*(df14_NY_pred$time - params[2])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_cumulative_death = 19.45*sqrt(var_log_normal)
se_daily_death =   19.45*sqrt(var_log_normal + lag(var_log_normal,1))

#df14_NY_pred_daily = df14_NY_pred %>% mutate(daily_deaths_pred = death_pred - lag(death_pred, 1), SE = se_daily_death) %>%
#mutate(PI_upper = daily_deaths_pred + 1.96*SE, PI_low = daily_deaths_pred - 1.96*SE, PI_upper_daily = PI_upper - lag(PI_upper, 1), PI_low_daily = PI_low - lag(PI_low, 1))

df14_NY_cum_pred = df14_NY_pred %>% mutate(SE = sqrt(var)) %>%
  mutate(daily_deaths_pred = death_pred - lag(death_pred)) %>%
  mutate(PI_upper = 19.45*exp(log(death_pred/19.45) + 1.96*SE), PI_low = 19.45*exp(log(death_pred/19.45)  - 1.96*SE))

plot(df14_NY_cum_pred$death_pred, type = "l", ylim = c(0, max(df14_NY_cum_pred$PI_upper, na.rm = T)), ylab = "Cumulative predicted deaths", xlab = "time index")
lines(df14_NY_cum_pred$PI_upper, type = "b")
lines(df14_NY_cum_pred$PI_low, type = "b")


# plot(df14_NY_pred_daily$daily_deaths_pred, type = "l", ylim = c(0, max(df14_NY_pred_daily$PI_upper, na.rm = T)), ylab = "dailypredicted deaths", xlab = "time index")
# lines(df14_NY_pred_daily$PI_upper, type = "b")
# lines(df14_NY_pred_daily$PI_low, type = "b")




#write.xlsx(df14_NY_pred, "~/Dropbox/COVID_19/data/IHME/NY/df14_NY_pred_CI_v1.xlsx")
write.xlsx(df14_NY_cum_pred, "~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df14_NY_pred_CI.xlsx")

#----7days----#
params = read.csv("~/Dropbox/COVID_19/data/IHME/Newyork/df7_NY_param_v1.csv", header = F)
params = as.numeric(params[1,])
#hessian_inv_matrix = read.csv("~/Dropbox/COVID_19/data/IHME/NY/df14_NY_hessinv_v1.csv", header = F)
#hessian_inv_matrix = as.matrix(hessian_inv_matrix[c(1,2,4), c(1,2,4)])
#hessian_inv_matrix = solve(solve(hessian_inv_matrix) + diag(nrow(hessian_inv_matrix)))
df7_NY_pred = read.xlsx("~/Dropbox/COVID_19/results/Medium_article/IHME/df7_NY_pred.xlsx")
df7_NY_train = read.xlsx("~/Dropbox/COVID_19/data/IHME/Newyork/df7_NY_v1.xlsx")
#measurement_error = sapply(df14_NY_pred$time, FUN = decre_fun)
measurement_error = 0.01

J =matrix(NA, nrow(df7_NY_train), 4)
for(t in 1:nrow(df7_NY_train))
{
  J[t, ] = g_prime(params = params, t, df7_NY_train$social_distance[t])
}
Sigma = diag(df7_NY_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))
hessian_inv_matrix =  (hessian_inv_matrix + t(hessian_inv_matrix))/2

var1 = rep(NA, nrow(df7_NY_pred))
for(i in 1: length(var1))
{
  var1[i] = delta_var(df7_NY_pred$time[i], 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var1  + measurement_error^2

mu = log(exp(params[4])* pnorm(exp(params[1])*(df14_NY_pred$time - params[2])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_cumulative_death = 19.45*sqrt(var_log_normal)
se_daily_death =   19.45*sqrt(var_log_normal + lag(var_log_normal,1))

df7_NY_cum_pred = df7_NY_pred %>% mutate(SE = sqrt(var)) %>%
  mutate(daily_deaths_pred = death_pred - lag(death_pred)) %>%
  mutate(PI_upper = 19.45*exp(log(death_pred/19.45) + 1.96*SE), PI_low = 19.45*exp(log(death_pred/19.45)  - 1.96*SE))

plot(df7_NY_cum_pred$death_pred, type = "l", ylim = c(0, max(df7_NY_cum_pred$PI_upper, na.rm = T)))
lines(df7_NY_cum_pred$PI_upper, type = "b")
lines(df7_NY_cum_pred$PI_low, type = "b")
write.xlsx(df7_NY_cum_pred, "~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df7_NY_pred_CI.xlsx")



#----0days----#

params = read.csv("~/Dropbox/COVID_19/data/IHME/Newyork/df0_NY_param_v1.csv", header = F)
params = as.numeric(params[1,])
#hessian_inv_matrix = read.csv("~/Dropbox/COVID_19/data/IHME/NY/df14_NY_hessinv_v1.csv", header = F)
#hessian_inv_matrix = as.matrix(hessian_inv_matrix[c(1,2,4), c(1,2,4)])
#hessian_inv_matrix = solve(solve(hessian_inv_matrix) + diag(nrow(hessian_inv_matrix)))
df0_NY_pred = read.xlsx("~/Dropbox/COVID_19/results/Medium_article/IHME/df0_NY_pred.xlsx")
df0_NY_train = read.xlsx("~/Dropbox/COVID_19/data/IHME/Newyork/df0_NY_v1.xlsx")
#measurement_error = sapply(df14_NY_pred$time, FUN = decre_fun)
measurement_error = 0.01

J =matrix(NA, nrow(df0_NY_train), 4)
for(t in 1:nrow(df0_NY_train))
{
  J[t, ] = g_prime(params = params, t, df0_NY_train$social_distance[t])
}
Sigma = diag(df0_NY_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))
hessian_inv_matrix =  (hessian_inv_matrix + t(hessian_inv_matrix))/2
var1 = rep(NA, nrow(df0_NY_pred))
for(i in 1: length(var1))
{
  var1[i] = delta_var(df0_NY_pred$time[i], 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var1  + measurement_error^2
mu = log(exp(params[4])* pnorm(exp(params[1])*(df0_NY_pred$time - params[1])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_daily_death =   19.45*sqrt(var_log_normal + lag(var_log_normal,1))

df0_NY_cum_pred = df0_NY_pred %>% mutate(SE = sqrt(var)) %>%
  mutate(daily_deaths_pred = death_pred - lag(death_pred)) %>%
  mutate(PI_upper = 19.45*exp(log(death_pred/19.45) + 1.96*SE), PI_low = 19.45*exp(log(death_pred/19.45)  - 1.96*SE))

plot(df0_NY_cum_pred$death_pred, type = "l", ylim = c(0, max(df0_NY_cum_pred$PI_upper, na.rm = T)))
lines(df0_NY_cum_pred$PI_upper, type = "b")
lines(df0_NY_cum_pred$PI_low, type = "b")
write.xlsx(df0_NY_cum_pred, "~/Dropbox/COVID_19/results/IHME/NY/Plots_medium_article/df0_NY_pred_CI.xlsx")
