# --- python code for model fitting
# --- run under the Github folder CurveFit

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
#from retrieve_JHU_data import data_country
num_params   = 3
#
# model for the mean of the data
def generalized_error_function(t, params) :
    alpha = params[0]
    beta  = params[1]
    p     = params[2]
    return 0.5 * p * ( 1.0 + scipy.special.erf( alpha * ( t - beta ) ) )
#
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


### after data creation in R
from os.path import expanduser as ospath
df_proj_MH = pd.read_excel(ospath('~/Dropbox/COVID_19/data/IHME/India/df_Maharashtra_datasettillApril22.xlsx'))


#### model run for 7 days before peak ------ France
model = curvefit.core.model.CurveModel(
  df=df_proj_MH,
  col_t='time',
  col_obs='smooth',
  col_group='group',
  col_covs=[['intercept'], ['intercept', 'social_distance'], ['intercept']],
  param_names=['alpha', 'beta', 'p'],
  link_fun=[ exp_fun, identity_fun, exp_fun ],
  var_link_fun=num_fe * [ identity_fun ],
  #link_fun=[lambda x: x, lambda x: x, lambda x: x],
  fun = ln_gaussian_cdf,
  col_obs_se   = 'measurement_std'
)

# --- Gaussian prior
#model.fit_params(fe_gprior=[[0, 1.], [0, 1e-3], [5, 10.]])

# --- initialization
fe_init   = np.array([0.5, 0.5, 0.5, 0.5])
re_init   = numpy.zeros( num_fe )
fe_bounds = [[0, 0]] * num_fe
re_bounds = [ [0.0, 0.0] ] * num_fe
#Insensitive to the following priors in this example... Choose any one
fe_gprior=[[0, 1], [0, 1], [0, 1], [0,1]]
#fe_gprior=[[0, 1.], [0, 1e-3], [5, 10.], [0,1]]
model.fit_params(fe_init = fe_init, fe_bounds = fe_bounds, fe_gprior = fe_gprior, options={
  'ftol': 1e-10,
  'maxiter': 1000
})

re_estimate = model.result.x[num_fe:]
with open(ospath('~/Dropbox/COVID_19/data/IHME/India/df_MH_proj_param.csv'),"w+") as my_csv:
    csvWriter = csv.writer(my_csv,delimiter=',')
    csvWriter.writerows([re_estimate])
#fe_estimate = model.result.x[:num_fe]

import datetime

dt = datetime.datetime(2020, 4, 23)
end = datetime.datetime(2020, 7, 15)
step = datetime.timedelta(days=1)

result = []

while dt < end:
    result.append(dt.strftime('%Y-%m-%d'))
    dt += step
    
date_pred = result
n_pred = len(date_pred)
n_train = len(df_proj_MH) + 1
df_pred_time = pd.DataFrame()
df_pred_time['time']  =  numpy.arange(start=n_train, stop=n_pred+n_train, step=1)
death_rate_predict = np.exp(model.predict(
    t=df_pred_time.time,
    group_name="Maharashtra"
))

data_dict_pred = {
'date' : date_pred,
'death_pred'  : death_rate_predict*114.2,
'time' : df_pred_time.time
}
df_pred = pandas.DataFrame(data_dict_pred)

df_pred.to_excel(ospath('~/Dropbox/COVID_19/data/IHME/India/df_MH_proj.xlsx'))







### after data creation in R
from os.path import expanduser as ospath
df_proj_GJ = pd.read_excel(ospath('~/Dropbox/COVID_19/data/IHME/India/df_Gujarat_datasettillApril22.xlsx'))


#### model run for 7 days before peak ------ France
model = curvefit.core.model.CurveModel(
  df=df_proj_GJ,
  col_t='time',
  col_obs='smooth',
  col_group='group',
  col_covs=[['intercept'], ['intercept', 'social_distance'], ['intercept']],
  param_names=['alpha', 'beta', 'p'],
  link_fun=[ exp_fun, identity_fun, exp_fun ],
  var_link_fun=num_fe * [ identity_fun ],
  #link_fun=[lambda x: x, lambda x: x, lambda x: x],
  fun = ln_gaussian_cdf,
  col_obs_se   = 'measurement_std'
)

# --- Gaussian prior
#model.fit_params(fe_gprior=[[0, 1.], [0, 1e-3], [5, 10.]])

# --- initialization
fe_init   = np.array([0.5, 0.5, 0.5, 0.5])
re_init   = numpy.zeros( num_fe )
fe_bounds = [[0, 0]] * num_fe
re_bounds = [ [0.0, 0.0] ] * num_fe
#Insensitive to the following priors in this example... Choose any one
fe_gprior=[[0, 1], [0, 1], [0, 1], [0,1]]
#fe_gprior=[[0, 1.], [0, 1e-3], [5, 10.], [0,1]]
model.fit_params(fe_init = fe_init, fe_bounds = fe_bounds, fe_gprior = fe_gprior, options={
  'ftol': 1e-10,
  'maxiter': 1000
})

re_estimate = model.result.x[num_fe:]
with open(ospath('~/Dropbox/COVID_19/data/IHME/India/df_GJ_proj_param.csv'),"w+") as my_csv:
    csvWriter = csv.writer(my_csv,delimiter=',')
    csvWriter.writerows([re_estimate])
#fe_estimate = model.result.x[:num_fe]

import datetime

dt = datetime.datetime(2020, 4, 23)
end = datetime.datetime(2020, 7, 15)
step = datetime.timedelta(days=1)

result = []

while dt < end:
    result.append(dt.strftime('%Y-%m-%d'))
    dt += step
    
date_pred = result
n_pred = len(date_pred)
n_train = len(df_proj_GJ) + 1
df_pred_time = pd.DataFrame()
df_pred_time['time']  =  numpy.arange(start=n_train, stop=n_pred+n_train, step=1)
death_rate_predict = np.exp(model.predict(
    t=df_pred_time.time,
    group_name="Gujarat"
))

data_dict_pred = {
'date' : date_pred,
'death_pred'  : death_rate_predict*62.7,
'time' : df_pred_time.time
}
df_pred = pandas.DataFrame(data_dict_pred)

df_pred.to_excel(ospath('~/Dropbox/COVID_19/data/IHME/India/df_GJ_proj.xlsx'))
