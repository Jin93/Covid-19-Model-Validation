# Prediction interval based on mode based uncertainity
decre_fun = function(x)
{
  return(1/(0.1+x^2))
}

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



#------------------Maharashtra------#

params = read.csv("~/Dropbox/COVID_19/data/IHME/India/df_MH_proj_param.csv", header = F)
params = as.numeric(params[1,])
#hessian_inv_matrix = read.csv("~/Dropbox/COVID_19/data/IHME/Italy/df14_Italy_hessinv_v1.csv", header = F)
#hessian_inv_matrix = as.matrix(hessian_inv_matrix[c(1,2,4), c(1,2,4)])
#hessian_inv_matrix = solve(solve(hessian_inv_matrix) + diag(nrow(hessian_inv_matrix)))
df_MH_pred = read.xlsx("~/Dropbox/COVID_19/data/IHME/India/df_MH_proj.xlsx")
df_MH_train = read.xlsx("~/Dropbox/COVID_19/data/IHME/India/df_Maharashtra_datasettillApril22.xlsx")
df_MH_train$Date = convertToDate(df_MH_train$Date)
#measurement_error = sapply(df14_Italy_pred$time, FUN = decre_fun)
measurement_error = 0.01

J =matrix(NA, nrow(df_MH_train), 4)
for(t in 1:nrow(df_MH_train))
{
  J[t, ] = g_prime(params = params, t, df_MH_train$social_distance[t])
}
Sigma = diag(df_MH_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))

var = rep(NA, nrow(df_MH_pred))
for(i in 1: length(var))
{
  var[i] = delta_var(df_MH_pred$time[i], 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var  + measurement_error^2
mu = log(exp(params[4])* pnorm(exp(params[1])*(df_MH_pred$time - params[1])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_daily_death =   114.2*sqrt(var_log_normal + lag(var_log_normal,1))

df_MH_pred = df_MH_pred %>% mutate(daily_deaths_pred = death_pred - lag(death_pred, 1), SE = se_daily_death) %>%
  mutate(PI_upper = daily_deaths_pred + 1.96*SE, PI_low = daily_deaths_pred - 1.96*SE )

plot(df_MH_pred$daily_deaths_pred, type = "l", ylim = c(0, max(df_MH_pred$PI_upper, na.rm = T)))
lines(df_MH_pred$PI_upper, type = "b")
lines(df_MH_pred$PI_low, type = "b")
write.xlsx(df_MH_pred, "~/Dropbox/COVID_19/data/IHME/India/df_MH_pred_CI.xlsx")

df_MH_train_temp = df_MH_train %>% select(Date, group, cumulative_deaths) %>%
  mutate(daily_deaths = cumulative_deaths - lag(cumulative_deaths,1), SE = NA, PI_upper = NA, PI_low = NA)
df_MH_pred_temp = df_MH_pred %>% select(date, death_pred, daily_deaths_pred, SE, PI_upper, PI_low) %>%
  mutate(group = "Maharashtra", Date = as.Date(date)) %>% select(-date) %>%
  rename(cumulative_deaths = death_pred, daily_deaths = daily_deaths_pred) %>%
  select(Date, group,  cumulative_deaths, daily_deaths, SE, PI_upper, PI_low)
df_MH_pred_beg_CI = bind_rows(df_MH_train_temp, df_MH_pred_temp, .id = "Source")
df_MH_pred_beg_CI$daily_deaths[19] = 14
df_MH_pred_beg_CI$Source[df_MH_pred_beg_CI$Source == 1] = "Training"
df_MH_pred_beg_CI$Source[df_MH_pred_beg_CI$Source == 2] = "Projected"
df_MH_pred_beg_CI$Source[19] = "Training"

h <- ggplot(data=df_MH_pred_beg_CI,aes(x=Date, y = daily_deaths))
plot1 <- h +
  geom_ribbon(aes(x=Date,ymin=0,ymax=PI_upper),fill = "grey70") +
  geom_line(aes(y=daily_deaths,color=Source, group = Source)) + theme(legend.position = "none") + theme_bw() +
  labs(y="Deaths per day")
plot1






#-------------Maharashtra Cumulative PI--------#
params = read.csv("~/Dropbox/COVID_19/data/IHME/India/df_MH_proj_param.csv", header = F)
params = as.numeric(params[1,])
#hessian_inv_matrix = read.csv("~/Dropbox/COVID_19/data/IHME/NY/df14_NY_hessinv_v1.csv", header = F)
#hessian_inv_matrix = as.matrix(hessian_inv_matrix[c(1,2,4), c(1,2,4)])
#hessian_inv_matrix = solve(solve(hessian_inv_matrix) + diag(nrow(hessian_inv_matrix)))
df_MH_pred = read.xlsx("~/Dropbox/COVID_19/data/IHME/India/df_MH_proj.xlsx")
df_MH_train = read.xlsx("~/Dropbox/COVID_19/data/IHME/India/df_Maharashtra_datasettillApril22.xlsx")
#measurement_error = sapply(df14_NY_pred$time, FUN = decre_fun)
measurement_error = 0.01

J =matrix(NA, nrow(df_MH_train), 4)
for(t in 1:nrow(df_MH_train))
{
  J[t, ] = g_prime(params = params, t, df_MH_train$social_distance[t])
}
Sigma = diag(df_MH_train$measurement_std^2)
hessian_inv_matrix = solve((t(J) %*% solve(Sigma) %*% J)  + diag(4))
hessian_inv_matrix =  (hessian_inv_matrix + t(hessian_inv_matrix))/2
var1 = rep(NA, nrow(df_MH_pred))
for(i in 1: length(var1))
{
  var1[i] = delta_var(df_MH_pred$time[i], 0, hessian_inv_matrix = hessian_inv_matrix, params = params)
}

var = var1  + measurement_error^2
mu = log(exp(params[4])* pnorm(exp(params[1])*(df0_NY_pred$time - params[1])))
var_log_normal = (exp(var) - 1) * (exp(2*mu + var))
se_daily_death =   114.2*sqrt(var_log_normal + lag(var_log_normal,1))

df_MH_cum_pred = df_MH_pred %>% mutate(SE = sqrt(var)) %>%
  mutate(daily_deaths_pred = death_pred - lag(death_pred)) %>%
  mutate(PI_upper = 114.2*exp(log(death_pred/114.2) + 1.96*SE), PI_low = 114.2*exp(log(death_pred/114.2)  - 1.96*SE))

plot(df_MH_cum_pred$death_pred, type = "l", ylim = c(0, max(df_MH_cum_pred$PI_upper, na.rm = T)))
lines(df_MH_cum_pred$PI_upper, type = "b")
lines(df_MH_cum_pred$PI_low, type = "b")
write.xlsx(df_MH_cum_pred, "~/Dropbox/COVID_19/results/IHME/India/Plots_medium_article/df_MH_pred_CI.xlsx")

