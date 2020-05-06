# Covid-19-Model-Validation




## Description
Source code for our Medium article:

[Transparency, Reproducibility, and Validity of COVID-19 Projection Models](https://medium.com/@nilanjan10c/transparency-reproducibility-and-validity-of-covid-19-projection-models-78592e029f28)



## Contents

### IHME

####   Validation on Italy
    1. Create training data   IHME/Italy/Data_creation/IHME_data_creation.R
    2. IHME model fitting   IHME/Italy/Model_validation/model_validation_Italy.py
    3. Calcuate confidence interval for the predicted cumulative death   IHME/Italy/Prediction_interval_calculation/IHME_nystate_prediction_confidence_interval_Italy.R  

####   Validation on New York State
    1. Create training data   IHME/NYstate/Data_creation/IHME_data_creation.R
    2. IHME model fitting   IHME/NYstate/Model_validation/model_validation_nystate.py
    3. Calcuate confidence interval for the predicted cumulative death   IHME/NYstate/Prediction_interval_calculation/IHME_nystate_prediction_confidence_interval_NY.R 

####   Plots
    Generate input for Tableau to create the IHME figures in the article   IHME/Plots/Create_input_data_for_plots.R  



### ICL

Detailed documentation for ICL is provided at ICL/README.md.



### eSIR

####   Validation on Italy
    eSIR/Italy/Validation_Italy.R

####   Validation on New York State
    eSIR/NYstate/Validation_NYstate.R

####   Plots
    Generate input for Tableau to create the eSIR figures in the article 
       -- Italy: eSIR/Plots/Create_input_data_for_tableau_figures_Italy.R
       -- New York State: eSIR/Plots/Create_input_data_for_tableau_figures_NYstate.R


<br/>

## Additional Information

#### `IHME` (Institute of Health Metrics at the University of Washington) 

* Homepage: https://covid19.healthdata.org/
* Code: https://github.com/ihmeuw-msca/CurveFit
* References:

    [COVID, I. and Murray, C.J., 2020. Forecasting the impact of the first wave of the COVID-19 pandemic on hospital demand and deaths for the USA and European Economic Area countries. medRxiv.](http://www.healthdata.org/sites/default/files/files/Projects/COVID/RA_COVID-forecasting-USA-EEA_042120.pdf)
* Accession date: April 17, 2020


#### `ICL` (Imperial College London)

* Homepage: https://www.imperial.ac.uk/about/covid-19/
* Code: https://github.com/ImperialCollegeLondon/covid19model
* References:

    [Flaxman, S., Mishra, S., Gandy, A., Unwin, H., Coupland, H., Mellan, T., Zhu, H., Berah, T., Eaton, J., Perez Guzman, P. and Schmit, N., 2020. Report 13: Estimating the number of infections and the impact of non-pharmaceutical interventions on COVID-19 in 11 European countries.](https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-03-30-COVID19-Report-13.pdf)
    
    [Flaxman, S., Mishra, S., Gandy, A., Unwin, H.J.T., Coupland, H., Mellan, T.A., Zhu, H., Berah, T., Eaton, J.W., Guzman, P.N. and Schmit, N., 2020. Estimating the number of infections and the impact of non-pharmaceutical interventions on COVID-19 in European countries: technical description update. arXiv preprint arXiv:2004.11342.](https://arxiv.org/abs/2004.11342)



#### `eSIR` (University of Michigan)

* R package `eSIR`: https://github.com/lilywang1988/eSIR
* References:

    [Song, P.X., Wang, L., Zhou, Y., He, J., Zhu, B., Wang, F., Tang, L. and Eisenberg, M., 2020. An epidemiological forecast model and software assessing interventions on COVID-19 epidemic in China. medRxiv.](https://www.medrxiv.org/content/10.1101/2020.02.29.20029421v1)
    
    [Ray, D., Salvatore, M., Bhattacharyya, R., Wang, L., Mohammed, S., Purkayastha, S., Halder, A., Rix, A., Barker, D., Kleinsasser, M. and Zhou, Y., 2020. Predictions, role of interventions and effects of a historic national lockdown in India's response to the COVID-19 pandemic: data science call to arms. medRxiv.](https://www.medrxiv.org/content/10.1101/2020.04.15.20067256v1)
* Accession date: April 20, 2020

<br/>

## Data Sources
* Country-level data for Italy and India:   [JHU CSSE COVID-19 GitHub](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data) (Accession date: May 2nd, 2020)

* State-level data for New York:   [New York Times data](https://github.com/nytimes/covid-19-data) (Accession date: May 2nd, 2020)



<br/>

## Contributors
* Neha Agarwala, Department of Mathematics and Statistics, University of Maryland, Baltimore County
* Jin Jin, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Prosenjit Kundu, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Yi Wang, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Ruzhang Zhao, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* [Nilanjan Chatterjee](https://nilanjanchatterjee.org/), Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
 

