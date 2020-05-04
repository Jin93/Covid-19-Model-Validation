# set path to Covid-19-Model-Validation folder
#install_github("lilywang1988/eSIR")
library(devtools)
library(eSIR)
library(rjags)
library(ggplot2)
library(gridExtra)
library(chron)
library(gtools)
source('eSIR/R_functions/eSIR_modified_functions.R')

region.name = c('Italy') # name of the country/region, used for extracting Hopkins Covid-19 data
Ns = c(Italy=6.036e7)
# date of the initical lockdown. 
# We assume that the transmission rate decays as an exponential function after the initial lockdown
# assume that the lockdown takes effect starting from the next day
change_time=c(Italy=c("03/04/2020"))

initial.death = 7 # the n.death on the first day has to be greater than or equal to this value
lag.val=7 # number of days in the validation data set used for parameter tuning
lag=14 # c(21,14,7) number of days before peak, i.e. the last day in the training data set
# note: lag-nag.val is the number of days before peak for the last day in the validation data set

# ------------------------ Data Creation ------------------------

### death
county_level_url_JHU = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
data_county_level_JHU = read.csv(county_level_url_JHU, na.strings = c("NA"), header = T)
colnames(data_county_level_JHU) = c(colnames(data_county_level_JHU)[1:4],substr(colnames(data_county_level_JHU)[5:ncol(data_county_level_JHU)],2,20))
region.rows = which(data_county_level_JHU[,'Country.Region']==region.name)
F.region = data_county_level_JHU[data_county_level_JHU[,'Country.Region']==region.name,]
F.region = F.region[5:length(F.region)]
F.region = F.region[(min(which(F.region>=initial.death))):(length(F.region))]
#start date of the training data
start.date = names(F.region)[1]

county_level_url_JHU = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
data_county_level_JHU = read.csv(county_level_url_JHU, na.strings = c("NA"), header = T)
colnames(data_county_level_JHU) = c(colnames(data_county_level_JHU)[1:4],substr(colnames(data_county_level_JHU)[5:ncol(data_county_level_JHU)],2,20))
region.rows = which(data_county_level_JHU[,'Country.Region']==region.name)
I.region = data_county_level_JHU[data_county_level_JHU[,'Country.Region']==region.name,]
I.region = I.region[5:length(I.region)]
I.region = colSums(I.region)
I.region = I.region[(which(names(I.region)==start.date)):(length(I.region))]

### recovery
county_level_url_JHU = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
data_county_level_JHU = read.csv(county_level_url_JHU, na.strings = c("NA"), header = T)
colnames(data_county_level_JHU) = c(colnames(data_county_level_JHU)[1:4],substr(colnames(data_county_level_JHU)[5:ncol(data_county_level_JHU)],2,20))
region.rows = which(data_county_level_JHU[,'Country.Region']==region.name)
R.region = data_county_level_JHU[data_county_level_JHU[,'Country.Region']==region.name,]
R.region = R.region[5:length(R.region)]
R.region = colSums(R.region)
R.region = R.region[(which(names(R.region)==start.date)):(length(R.region))]

# combine I, F and R
dat = cbind(I=as.numeric(I.region),F=as.numeric(F.region),R=as.numeric(R.region))
colnames(dat) = c('I','F','R')
rownames(dat) = names(I.region)
current.date = names(I.region)[length(I.region)]

daily = rbind(NA,pmax(t(sapply(1:(nrow(dat)-1),function(x){dat[x+1,]-dat[x,]})),0))
rownames(daily) = rownames(dat)
peak_t = as.numeric(which.max(daily[,'I']))
pd.tem = as.Date(rownames(daily)[1],format="%m.%d.%Y") + peak_t-1
peak.date.I = gsub(" 0", "", format(pd.tem, " %m. %d.%y"))
peak.date.I = sub(' ','',peak.date.I)

##### change the format of start.date
raw.start.date=c(strsplit(rownames(dat)[1],'\\.')[[1]])
raw.start.date[1] = paste0(c('0',raw.start.date[1]),collapse = '')
if (nchar(raw.start.date[2])==1) raw.start.date[2] = paste0(c('0',raw.start.date[2]),collapse = '')
raw.start.date[3] = paste0(c('20',raw.start.date[3]),collapse = '')
start.date = paste0(raw.start.date,collapse = '/')

change_t = numeric()
change_t[1] = as.numeric(as.Date(paste0(strsplit(change_time,'/')[[1]][c(3,1,2)],collapse = '/'), format="%Y/%m/%d")-as.Date(paste0(strsplit(start.date,'/')[[1]][c(3,1,2)],collapse = '/'), format="%Y/%m/%d")) + 1
if (length(change_time)>1)
{
  for (tt in 2:length(change_time)){
    change_t[tt] = as.numeric(as.Date(paste0(strsplit(change_time,'/')[[tt]][c(3,1,2)],collapse = '/'), format="%Y/%m/%d")-as.Date(paste0(strsplit(start.date,'/')[[1]][c(3,1,2)],collapse = '/'), format="%Y/%m/%d")) + 1
  }
}

# the number of days between the last day of training and the current date  
l.test = nrow(dat)-which(rownames(dat) == peak.date.I) + lag

td.tem = as.Date(current.date,format="%m.%d.%Y") -l.test
# the last day in the training data:
training.date = gsub(" 0", "", format(td.tem, " %m. %d.%y"))
training.date = sub(' ','',training.date)
# training data:
dat.train = dat[1:which(rownames(dat) == training.date),]
dat.validation = dat[(which(rownames(dat) == training.date)+1):nrow(dat),]
time.train = 1:nrow(dat.train)
time.validation = (nrow(dat.train)+1):nrow(dat) # do not need to use nrow(dat.train)+1: want to connect the two parts in line

N=Ns[region.name] # total population
# construct training data for fit.eSIR
NI_complete <- dat.train[,'I'] # number of cases
RI_complete <- dat.train[,'F'] + dat.train[,'R'] # number of removed (death + recovered)
R <- RI_complete/N
Y <- NI_complete/N - R
# the proportion of death in removed
death_in_R = as.numeric(dat.train[length(R),'F']/RI_complete[length(R)])

# ------------------------ Plot the Observed data, lockdown dates, etc. ------------------------

# daily cases, death and recovered
daily.train = rbind(NA,pmax(t(sapply(1:(nrow(dat.train)-1),function(x){dat.train[x+1,]-dat.train[x,]})),0))
daily.validation = rbind(pmax(dat[nrow(daily.train),]-dat[nrow(daily.train)-1,],0),pmax(dat[nrow(daily.train)+1,]-dat[nrow(daily.train),],0),pmax(t(sapply(1:(nrow(dat.validation)-1),function(x){dat.validation[x+1,]-dat.validation[x,]})),0))
plot(time.train,daily.train[,'I'],type='l',lty=1,xlim=c(0,nrow(dat)),xlab='time',
     ylab='N.cases, N.death, N.recovered',main='JHU Data',ylim=c(0,max(daily,na.rm=T)))
points(c(max(time.train),time.validation),daily.validation[,'I'],type='l',lty='dashed')
points(time.train,daily.train[,'F'],type='l',lty=1,xlim=c(0,nrow(dat)),col='red')
points(c(max(time.train),time.validation),daily.validation[,'F'],type='l',lty='dashed',col='red')
points(time.train,daily.train[,'R'],type='l',lty=1,xlim=c(0,nrow(dat)),col='blue')
points(c(max(time.train),time.validation),daily.validation[,'R'],type='l',lty='dashed',col='blue')
# lockdown date
for (tt in 1:length(change_t)){
  abline(v=change_t[tt],lty='twodash')
  text(change_t[tt]*0.9,daily[nrow(dat.train),'I']*0.5*tt, labels=paste0('Lockdown ',tt,': \n',rownames(dat)[change_t[tt]]), cex=0.7,col="black")
}
abline(v=which.max(daily[,'I']),lty='dashed')
abline(v=which.max(daily[,'F']+1),col='red',lty='dashed')
tem = which.max(daily[,'I'])
pd.tem = as.Date(rownames(daily)[1],format="%m.%d.%Y") + tem-1
peak.date.I = gsub(" 0", "", format(pd.tem, " %m. %d.%y"))
peak.date.I = sub(' ','',peak.date.I)
text(tem+1,daily[tem,'I']*runif(1,0.6,0.8), labels=peak.date.I, cex=0.7,col="black")
tem = which.max(daily[,'F'])
pd.tem = as.Date(rownames(daily)[1],format="%m.%d.%Y") + tem-1
peak.date = gsub(" 0", "", format(pd.tem, " %m. %d.%y"))
peak.date = sub(' ','',peak.date)

text(tem+runif(1,1,3),daily[tem,'I']*runif(1,0.6,0.8), labels=peak.date, cex=0.7,col="red")

legend('topleft', legend=c("Confirmed Cases Training","Confirmed Cases Validation",
                           "Death Training","Death Validation",
                           "Recovered Training","Recovered Validation"),
       col=c(rep('black',2),rep('red',2),rep('blue',2)), lty=c(1,2,1,2,1,2), cex=0.5)


# ------------------------ Fit eSIR model ------------------------
T_fin0 = 200 # the # of days considered in eSIR
# model fitting and parameter tuning using the 1-week data after the last day of training data
ress0=list()
rmse = numeric() # use rmse to select parameters
# test of eSIR runs without error:
candidates = expand.grid(pi.min=0.1,lambda0=0.05) 
# grid search - need to change the search range based on training data
#candidates = expand.grid(pi.min=seq(0.1,0.3,by=0.02),lambda0=seq(0.035,0.07,by=0.005)) 
for (l in 1:nrow(candidates)){
  lambda0 = candidates[l,'lambda0']
  pi.min = candidates[l,'pi.min']
  res.exp <- fit.eSIR(Y,R,begin_str=start.date,death_in_R = death_in_R,change_time=change_time,
                      T_fin=T_fin0,exponential=TRUE,dic=T,lambda0=lambda0,add_death = F,
                      casename=paste0(region.name,"_exp"),save_files = F,save_mcmc=F,
                      save_plot_data = F,M=5e3,nburnin = 2e3, pi.min = pi.min)
  ress0[[l]] = res.exp
  val.p = daily[(nrow(dat.train)+1):(nrow(dat.train)+lag.val)]
  temp = res.exp$data_comp[,'mean']
  tempR=res.exp$data_comp_R[,'mean']
  daily.pred.I = c(temp[2]-temp[1],sapply(1:(length(temp)-1),function(x){temp[x+1]-temp[x]}))*N
  daily.pred.R = c(tempR[2]-tempR[1],sapply(1:(length(tempR)-1),function(x){tempR[x+1]-tempR[x]}))*N
  daily.pred.Itotal = daily.pred.I + daily.pred.R
  pred.p = daily.pred.Itotal[(nrow(dat.train)+1):(nrow(dat.train)+lag.val)]
  rmse[l] = sqrt(sum((pred.p-val.p)^2))
  print(l)
}
l = which.min(rmse)
lambda0 = candidates[l,'lambda0']
pi.min = candidates[l,'pi.min']
res.exp <- ress0[[l]]
decay.period = res.exp$decay.period # the duration of transmission rate decay

# ------------------------ Create plots ------------------------

T_fin = T_fin0 - 1
ress = list()
ress[[1]] = res.exp
begin <- chron(dates. = start.date) + 1 # daily cases: starting from the next day
chron_ls <- chron(begin:(begin + T_fin - 1))
end <- chron(begin:(begin + T_fin))[T_fin]

temp = ress[[1]]$data_comp[1:(T_fin+1),'mean']
tempR=ress[[1]]$data_comp_R[1:(T_fin+1),'mean']
daily.pred.I = c(sapply(1:(length(temp)-1),function(x){temp[x+1]-temp[x]}))*N
daily.pred.R = c(sapply(1:(length(tempR)-1),function(x){tempR[x+1]-tempR[x]}))*N
daily.pred.Itotal = daily.pred.I + daily.pred.R

# upper bound:
temp = ress[[1]]$data_comp[1:(T_fin+1),'upper']
tempR=ress[[1]]$data_comp_R[1:(T_fin+1),'upper']
daily.upper.I = c(sapply(1:(length(temp)-1),function(x){temp[x+1]-temp[x]}))*N # tempR[2]-tempR[1]
daily.upper.R = c(sapply(1:(length(tempR)-1),function(x){tempR[x+1]-tempR[x]}))*N # tempR[2]-tempR[1]
daily.upper.Itotal = daily.upper.I + daily.upper.R
daily.upper.Itotal = pmax(0,daily.upper.Itotal)
# lower bound:
temp = ress[[1]]$data_comp[1:(T_fin+1),'lower']
tempR=ress[[1]]$data_comp_R[1:(T_fin+1),'lower']
daily.lower.I = c(sapply(1:(length(temp)-1),function(x){temp[x+1]-temp[x]}))*N # tempR[2]-tempR[1]
daily.lower.R = c(sapply(1:(length(tempR)-1),function(x){tempR[x+1]-tempR[x]}))*N # tempR[2]-tempR[1]
daily.lower.Itotal = daily.lower.I + daily.lower.R
daily.lower.Itotal = pmax(0,daily.lower.Itotal)

dat.table=data.frame(t = 1:T_fin,
                     x = c((daily[-1,'I']),rep(NA,T_fin+1-nrow(dat))),
                     y1 = daily.pred.Itotal, # the unsmoothed prediction
                     y1.upper = daily.upper.Itotal,
                     y1.lower = daily.lower.Itotal,
                     phase=c(rep('Training',nrow(dat.train)-1),rep('Validation',nrow(dat.validation)),rep('Future',T_fin+1-nrow(dat))))
tr.table = dat.table[1:(nrow(daily.train)-1),]
va.table = dat.table[(nrow(daily.train)-1):(nrow(daily.train)-1+lag.val),]
ho.table = dat.table[(nrow(daily.train)-1+lag.val):(nrow(dat)-1),]
proj.peak.t = which.max(daily.pred.Itotal)

colors <- c("Training" = "black", "Validation" = "cyan4", "Holdout" = "blue", "Projection" = "maroon")

plot1 <- ggplot() + 
  labs(title = substitute(paste(pi,"(t) = exp(-",lambda[0],"t) >= ",v2, ",  ",
                                lambda[0], " = ", v1),
                          list(v1 = as.numeric(lambda0), v2 = pi.min)),
       subtitle = substitute(paste(R[0]," = ",v1,",  R.min = ",v2,"  Decay period: ",v3," days"),
                             list(v1 = as.numeric(formatC(as.numeric(ress[[1]]$out_table['R0_p_mean']),digits=2,format='f')), 
                                  v2 = formatC(as.numeric(ress[[1]]$out_table['R0_p_mean']*pi.min),digits=2,format='f'),
                                  v3 = decay.period))) + 
  geom_line(data = dat.table, 
            aes(x = t, y = y1, color = 'Projection'), lty='solid') + 
  geom_line(data = tr.table, 
            aes(x=t, y=x, color = 'Training')) + 
  geom_line(data = va.table, 
            aes(x=t, y=x, color = 'Validation')) +
  geom_line(data = ho.table, 
            aes(x=t, y=x, color = 'Holdout')) +
  ## lockdown
  geom_vline(xintercept = change_t+0.2, 
             color = "black", lty = 'twodash', show.legend = F) + 
  annotate(geom = "text", 
           label = paste0('Intervention \n ',as.character(chron(chron_ls[change_t], format = "m/d/y"))), 
           x = change_t - 3, y = max(dat.table[,'y1']) * seq(1/5,1/5*length(change_time),by=1/5), color = "black") + 
  # projected peak
  geom_vline(xintercept = proj.peak.t, 
             color = "maroon", show.legend = F) + 
  annotate(geom = "text", 
           label = paste0('SIR Peak \n ',as.character(chron(chron_ls[proj.peak.t], format = "m/d/y"))), 
           x = proj.peak.t + 10, y = max(dat.table[,'y1']) * 1/2, color = "maroon") +
  # true peak
  geom_vline(xintercept = peak_t, 
             color = "blue", show.legend = F) +
  annotate(geom = "text", 
           label = paste0('True Peak \n ',as.character(chron(chron_ls[as.numeric(peak_t)], format = "m/d/y"))), 
           x = as.numeric(peak_t), y = max(dat.table[,'x'],na.rm=T) * 3/4, color = "blue") + 
  theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 11), axis.title = element_text(size = 13)) + 
  scale_x_continuous(labels = as.character(chron_ls)[seq(1, T_fin, 20)], breaks = seq(1, T_fin, 20))  +
  labs(x = "Date",
       y = "Confirmed Cases Per Day",
       color = "") +
  scale_color_manual(values = colors)
plot1

# ------------------------ Write output .csv file ------------------------

output = data.frame(time = 1:T_fin,
                    date = chron_ls,
                    observed = c((daily[-1,'I']),rep(NA,T_fin+1-nrow(dat))),
                    estimate = daily.pred.Itotal, # unsmoothed prediction
                    estimate.lb = daily.lower.Itotal, # unsmoothed lower bound
                    estimate.ub = daily.upper.Itotal, # unsmoothed uppwer bound
                    phase=c(rep('Training',nrow(dat.train)-1),rep('Validation',lag.val),rep('Holdout',l.test-lag.val),rep('Projection',T_fin+1-nrow(dat))))
write.csv(output,file=paste0('eSIR/Italy/',region.name,'-eSIR-',lag-lag.val,'days_before_peak.csv'))

###### please refer to Create_input_for_tablau_plots.R for the code that
###### generates data used for making the eSIR plots in the article.
