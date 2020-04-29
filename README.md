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
> `/IHME/Italy/`

      1. IHME_data_creation_Italy.R   download data for Italy.
      
      2. IHME_model_validation_Italy.py   python code for IHME model fittiing on Italy data.
      
      3. IHME_prediction_confidence_interval_Italy.R   calcuate the confidence interval for the predicted cumulative death in Italy.

> `/IHME/New York State/`

       IHME_data_creation_NYstate.R   download data for New York state.
      
       IHME_model_validation_NYstate.py   python code for IHME model fitting on New York data.
      
       IHME_prediction_confidence_interval_NYstate.R   calcuate the confidence interval for the predicted cumulative death in New York state.

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
 

