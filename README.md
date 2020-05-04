# Covid-19-Model-Validation
Transparency, Reproducibility, and Validation of COVID-19 Projection models

## Description
Source code for validation and projection of the following Covid-19 models:

#### `IHME` (Institute of Health Metrics at the University of Washington) 

* Homepage: https://covid19.healthdata.org/
* Code: https://github.com/ihmeuw-msca/CurveFit
* References:
   1. COVID, I. and Murray, C.J., 2020. Forecasting the impact of the first wave of the COVID-19 pandemic on hospital demand and deaths for the USA and European Economic Area countries. medRxiv.

#### `ICL` (Imperial College London)

* Homepage: https://www.imperial.ac.uk/about/covid-19/
* Code: https://github.com/ImperialCollegeLondon/covid19model

#### `eSIR` (University of Michigan)

* R package `eSIR`: https://github.com/lilywang1988/eSIR


## Contents

### IHME
#### Validation on Italy
    1. Create training data   IHME/Italy/Data_creation/IHME_data_creation.R
    2. IHME model fitting   IHME/Italy/Model_validation/model_validation_Italy.py
    3. Calcuate confidence interval for the predicted cumulative death   IHME/Italy/Prediction_interval_calculation/IHME_nystate_prediction_confidence_interval_Italy.R  

#### Validation on New York State
    1. Create training data   IHME/NYstate/Data_creation/IHME_data_creation.R
    2. IHME model fitting   IHME/NYstate/Model_validation/model_validation_nystate.py
    3. Calcuate confidence interval for the predicted cumulative death   IHME/NYstate/Prediction_interval_calculation/IHME_nystate_prediction_confidence_interval_NY.R 

#### Plots
    Generate input for Tableau to create the IHME figures in the article   IHME/Plots/Create_input_data_for_plots.R  



### ICL



#### eSIR
#### Validation on Italy
    eSIR/Italy/Validation_Italy.R

#### Validation on New York State
    eSIR/NYstate/Validation_NYstate.R

#### Plots
    Generate input for Tableau to create the eSIR figures in the article 
    -- Italy: eSIR/Plots/Create_input_data_for_tableau_figures_Italy.R
    -- New York State: eSIR/Plots/Create_input_data_for_tableau_figures_NYstate.R





## Contributors
* Neha Agarwala, Department of Mathematics and Statistics, University of Maryland, Baltimore County
* Jin Jin, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Prosenjit Kundu, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Yi Wang, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Ruzhang Zhao, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* [Nilanjan Chatterjee](https://nilanjanchatterjee.org/), Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
 

