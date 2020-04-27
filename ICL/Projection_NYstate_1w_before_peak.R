library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(optparse)
  parser <- OptionParser()
  parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                       help="Perform a debug run of the model")
  parser <- add_option(parser, c("-F", "--full"), action="store_true",
                       help="Perform a full run of the model")
  cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)
  
  # Default run parameters for the model
  if(is.null(cmdoptions$options$debug)) {
    DEBUG = Sys.getenv("DEBUG") == "TRUE"
  } else {
    DEBUG = cmdoptions$options$debug
  }
  
  if(is.null(cmdoptions$options$full)) {
    FULL = Sys.getenv("FULL") == "TRUE"
  } else {
    FULL = cmdoptions$options$full
  }
  
  if(DEBUG && FULL) {
    stop("Setting both debug and full run modes at once is invalid")
  }
  
  if(length(cmdoptions$args) == 0) {
    StanModel = 'base'
  } else {
    StanModel = cmdoptions$args[1]
  }
  
  print(sprintf("Running %s",StanModel))
  if(DEBUG) {
    print("Running in DEBUG mode")
  } else if (FULL) {
    print("Running in FULL mode")
  }
  

  
  
###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################
  
#N2=out_cvd19_model$stan_data$N2
N2=220
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

covariate1_added=covariate1[nrow(covariate1),]
for(i in c(1:120)){
  covariate1=rbind(covariate1,covariate1_added)
}

covariate2_added=covariate2[nrow(covariate2),]
for(i in c(1:120)){
  covariate2=rbind(covariate2,covariate2_added)
}

covariate3_added=covariate3[nrow(covariate3),]
for(i in c(1:120)){
  covariate3=rbind(covariate3,covariate3_added)
}

covariate4_added=covariate4[nrow(covariate4),]
for(i in c(1:120)){
  covariate4=rbind(covariate4,covariate4_added)
}

covariate5_added=covariate5[nrow(covariate5),]
for(i in c(1:120)){
  covariate5=rbind(covariate5,covariate5_added)
}

covariate6_added=covariate6[nrow(covariate6),]
for(i in c(1:120)){
  covariate6=rbind(covariate6,covariate6_added)
}



EpidemicStart=out_cvd19_model$EpidemicStart
pop=out_cvd19_model$stan_data$pop
SI=out_cvd19_model$stan_data$SI
SI=c(SI,rep(0,120))
f= out_cvd19_model$stan_data$f
for(k in 1:120){
  f=rbind(f,rep(0,14))
}
  
#transformed_parameters <- function(N2,M,mu,alpha_hier,kappa,y,phi,tau,ifr_noise,
#                                   covariate1,covariate2,covariate3,covariate4,
#                                   covariate5,covariate6,EpidemicStart,
#                                   pop,SI){

E_deaths_all=array(dim=c(100,220,14))
for(iter in 1:100){
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

