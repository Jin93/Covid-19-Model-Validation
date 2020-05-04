# Covid-19-Model-Validation
Transparency, Reproducibility, and Validation of COVID-19 Projection models

## Description
Source code for validation and projection of the following Covid-19 models:

#### `IHME` (Institute of Health Metrics at the University of Washington) 

* Homepage: https://covid19.healthdata.org/
* Code: https://github.com/ihmeuw-msca/CurveFit

#### `ICL` (Imperial College London)

* Homepage: https://www.imperial.ac.uk/about/covid-19/
* Code: https://github.com/ImperialCollegeLondon/covid19model

#### `eSIR` (University of Michigan)

* R package `eSIR`: https://github.com/lilywang1988/eSIR


## Contents

#### IHME
##### Validation on Italy
   1. Create training data  `/IHME/Italy/Data_creation/IHME_data_creation.R`
   2. IHME model fitting  `/IHME/Italy/Model_validation/model_validation_Italy.py` 
   3. Calcuate confidence interval for the predicted cumulative death  `/IHME/Italy/Prediction_interval_calculation/IHME_nystate_prediction_confidence_interval_Italy.R`   

##### Validation on New York State
   1. Create training data  `/IHME/NYstate/Data_creation/IHME_data_creation.R`
   2. IHME model fitting  `/IHME/NYstate/Model_validation/model_validation_nystate.py` 
   3. Calcuate confidence interval for the predicted cumulative death  `/IHME/NYstate/Prediction_interval_calculation/IHME_nystate_prediction_confidence_interval_NY.R`   

> `/IHME/Plots/`
      
      Create_input_data_for_tableau_figures.R   code for generating the input for Tableau to create the IHME plots in Figure 1.
#### ICL


#### eSIR

      1. /eSIR/Italy/Validation_Italy.R   code for downloading data, eSIR model fitting and generating the input for Tableau for Italy.
      
      2. /eSIR/New York State/Validation_NYstate.R   code for analysis on New York state.
      
      3. /eSIR/Plots/Create_input_data_for_tableau_figures.R   code for generating the input for Tableau to create Figure 2.
      
      4. /eSIR/R_functions/eSIR_modified_functions.R   modified eSIR function used in our analysis.






## Acknowledgements
* Neha Agarwala, Department of Mathematics and Statistics, University of Maryland, Baltimore County
* Jin Jin, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Prosenjit Kundu, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Yi Wang, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Ruzhang Zhao, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* [Nilanjan Chatterjee](https://nilanjanchatterjee.org/), Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
 

