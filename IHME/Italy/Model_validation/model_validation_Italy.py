# --- python code for model fitting
# --- run python3 under the Github folder CurveFit!
# --- install modules xspline, xlrd, openpyxl 
# --- (and possibly several others if they are not on your laptop). Example:
# --- pip3 install xspline 

import sys
import pandas
import numpy
import scipy
import pdb
import sandbox
import csv
import numpy as np
import pandas as pd
sandbox.path()
import curvefit
from curvefit.core.model import CurveModel
from curvefit.core.functions import ln_gaussian_cdf


num_params   = 3

# link function used for beta
def identity_fun(x) :
    return x
#
# link function used for alpha, p

def exp_fun(x) :
    return numpy.exp(x)
#
# inverse of function used for alpha, p

def log_fun(x) :
    return numpy.log(x)
    

# data_frame

num_fe = 4


### Reading the data extracted after implementing IHME_data_creation.R

from os.path import expanduser as ospath

#---Change the path where the extracted data set is located---#
df7_Italy = pd.read_excel(ospath('data/df7_Italy_v1.xlsx'))
df14_Italy = pd.read_excel(ospath('data/df14_Italy_v1.xlsx'))
df0_Italy = pd.read_excel(ospath('data/df0_Italy_v1.xlsx'))

#### model run for 7 days before peak ------ Italy
# We create social distance covariate as below:
# social distance covariate is equivalent to count of days (starting from 0 and increasing with an increment of one) when training data starts to the day before the first intervention. For days with first intervention active, we take the social distance covarite as 0.67 and for days when two interventions are active, the social distance covariate is measured as 0.33. When three or four inrterventions are active, the social distance covariate is taken as 0. For more details, see the data creation file.
model = curvefit.core.model.CurveModel(
  df=df7_Italy,
  col_t='time',
  col_obs='smooth',
  col_group='group',
  col_covs=[['intercept'], ['intercept', 'social_distance'], ['intercept']],
  param_names=['alpha', 'beta', 'p'],
  link_fun=[ exp_fun, identity_fun, exp_fun ],
  var_link_fun=num_fe * [ identity_fun ],
  fun = ln_gaussian_cdf,
  col_obs_se   = 'measurement_std'
)


# --- initialization
fe_init   = np.array([0.5, 0.5, 0.5, 0.5])
re_init   = numpy.zeros( num_fe )
fe_bounds = [[0, 0]] * num_fe
re_bounds = [ [0.0, 0.0] ] * num_fe
#We chose standard normal prior on the fixed effects
fe_gprior=[[0, 1], [0, 1], [0, 1], [0,1]]
model.fit_params(fe_init = fe_init, fe_bounds = fe_bounds, fe_gprior = fe_gprior, options={
  'ftol': 1e-10,
  'maxiter': 1000
})

#--exporting the parameter estimates that will be required for variance calculation using delta method--#
re_estimate = model.result.x[num_fe:]
with open(ospath('parameters/df7_Italy_param_v1.csv'),"w+") as my_csv:
    csvWriter = csv.writer(my_csv,delimiter=',')
    csvWriter.writerows([re_estimate])

import datetime

dt = datetime.datetime(2020, 3, 20)
end = datetime.datetime(2020, 7, 15)
step = datetime.timedelta(days=1)

result = []

while dt < end:
    result.append(dt.strftime('%Y-%m-%d'))
    dt += step
    
date_pred = result
n_pred = len(date_pred)
n_train = len(df7_Italy) + 1
df_pred_time = pd.DataFrame()
df_pred_time['time']  =  numpy.arange(start=n_train, stop=n_pred+n_train, step=1)
death_rate_predict = np.exp(model.predict(
    t=df_pred_time.time,
    group_name="Italy"
))

data_dict_pred = {
'date' : date_pred,
'death_pred'  : death_rate_predict*60.36,
'time' : df_pred_time.time
}
df_pred = pandas.DataFrame(data_dict_pred)

df_pred.to_excel(ospath('IHME_output/df7_Italy_pred.xlsx'))


#### model run for 14 days before peak ------ Italy
model = curvefit.core.model.CurveModel(
  df=df14_Italy,
  col_t='time',
  col_obs='smooth',
  col_group='group',
  col_covs=[['intercept'], ['intercept', 'social_distance'], ['intercept']],
  param_names=['alpha', 'beta', 'p'],
  link_fun=[ exp_fun, identity_fun, exp_fun ],
  var_link_fun=num_fe * [ identity_fun ],
  fun = ln_gaussian_cdf,
  col_obs_se   = 'measurement_std'
)

# --- initialization
fe_init   = np.array([0.5, 0.5, 0.5, 0.5])
re_init   = numpy.zeros( num_fe )
fe_bounds = [[0, 0]] * num_fe
re_bounds = [ [0.0, 0.0] ] * num_fe
#We chose standard normal prior on the fixed effects
fe_gprior=[[0, 1], [0, 1], [0, 1], [0,1]]
model.fit_params(fe_init = fe_init, fe_bounds = fe_bounds, fe_gprior = fe_gprior, options={
  'ftol': 1e-10,
  'maxiter': 1000
})

re_estimate = model.result.x[num_fe:]
with open(ospath('parameters/df14_Italy_param_v1.csv'),"w+") as my_csv:
    csvWriter = csv.writer(my_csv,delimiter=',')
    csvWriter.writerows([re_estimate])
    

import datetime

dt = datetime.datetime(2020, 3, 13)
end = datetime.datetime(2020, 7, 15)
step = datetime.timedelta(days=1)

result = []

while dt < end:
    result.append(dt.strftime('%Y-%m-%d'))
    dt += step
    
date_pred = result
n_pred = len(date_pred)
n_train = len(df14_Italy)+1
df_pred_time = pd.DataFrame()
df_pred_time['time']  =  numpy.arange(start=n_train, stop=n_pred+n_train, step=1)
death_rate_predict = np.exp(model.predict(
    t=df_pred_time.time,
    group_name="Italy"
))

data_dict_pred = {
'date' : date_pred,
'death_pred'  : death_rate_predict*60.36,
'time' : df_pred_time.time
}
df_pred = pandas.DataFrame(data_dict_pred)

df_pred.to_excel(ospath('IHME_output/df14_Italy_pred.xlsx'))




#### model run for 0 days before peak ------ Italy
model = curvefit.core.model.CurveModel(
  df=df0_Italy,
  col_t='time',
  col_obs='smooth',
  col_group='group',
  col_covs=[['intercept'], ['intercept', 'social_distance'], ['intercept']],
  param_names=['alpha', 'beta', 'p'],
  link_fun=[ exp_fun, identity_fun, exp_fun ],
  var_link_fun=num_fe * [ identity_fun ],
  fun = ln_gaussian_cdf,
  col_obs_se   = 'measurement_std'
)


# --- initialization
fe_init   = np.array([0.5, 0.5, 0.5, 0.5])
re_init   = numpy.zeros( num_fe )
fe_bounds = [[0, 0]] * num_fe
re_bounds = [ [0.0, 0.0] ] * num_fe
#We chose standard normal prior on the fixed effects
fe_gprior=[[0, 1], [0, 1], [0, 1], [0,1]]
model.fit_params(fe_init = fe_init, fe_bounds = fe_bounds, fe_gprior = fe_gprior, options={
  'ftol': 1e-10,
  'maxiter': 1000
})

re_estimate = model.result.x[num_fe:]
with open(ospath('parameters/df0_Italy_param_v1.csv'),"w+") as my_csv:
    csvWriter = csv.writer(my_csv,delimiter=',')
    csvWriter.writerows([re_estimate])
    

import datetime

dt = datetime.datetime(2020, 3, 27)
end = datetime.datetime(2020, 7, 15)
step = datetime.timedelta(days=1)

result = []

while dt < end:
    result.append(dt.strftime('%Y-%m-%d'))
    dt += step
    

date_pred = result
n_pred = len(date_pred)
n_train = len(df0_Italy) + 1
df_pred_time = pd.DataFrame()
df_pred_time['time']  =  numpy.arange(start=n_train, stop=n_pred+n_train, step=1)
death_rate_predict = np.exp(model.predict(
    t=df_pred_time.time,
    group_name="Italy"
))

data_dict_pred = {
'date' : date_pred,
'death_pred'  : death_rate_predict*60.36,
'time' : df_pred_time.time
}
df_pred = pandas.DataFrame(data_dict_pred)

df_pred.to_excel(ospath('IHME_output/df0_Italy_pred.xlsx'))
