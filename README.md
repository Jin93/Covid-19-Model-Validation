# Covid-19-Model-Validation
Transparency, Reproducibility, and Validation of COVID-19 Projection models.


## Description
Source code for the following article on validation of ICL, IHME and eSIR models for Covid-19 death/case projection.

link



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

In ICL model, training data for Italy is from 03/02/20 to 03/13/20 (2 weeks before peak), 03/20/20 (1 week before peak) or 03/27/20 (till peak). Training data for New York is from 03/18/20 to 03/24/20 (2 weeks before peak), 03/31/20 (1 week before peak) or 04/07/20 (till peak). The starting dates of the training data of Italy and New York were both selected as the first date when the daily increase in deaths exceeded 9. Training data for the other 12 Europe countries were from 02/29/20 to 05/01/20.

> `/ICL/code/`

      1. Italy_NY_till_peak.R  code for training ICL model, making projection for New York and Italy, and generating plots for ICL model validation, training data includes the daily case and deaths number before peak date of New York and Italy, and other 12 Europe countries till May 1st. Note that here in ICL model, self-isolation, public events banning and social distance encouraged are all corresponding to the "stay at home" intervention in the IHME model in this article
      
      2. Italy_NY_1week.R  code for training ICL model, making projection for New York and Italy, and generating plots for ICL model validation, training data includes the daily case and deaths number 1 week prior to peak date of New York and Italy, and other 12 Europe countries till May 1st.
      
      3. Italy_NY_2week.R  code for training ICL model, making projection for New York and Italy, and generating plots for ICL model validation, training data includes the daily case and deaths number 2 weeks prior to peak date of New York and Italy, and other 12 Europe countries till May 1st.
      
      4. DataFormAdjust.R: format the output csv file to for tableau input
      
      5. loadData.R load the data for the 13 Europe counties and New York, for the dates until April 15
      
      6. loadData_NY_May1.R load the data for New York for dates till May 1st
      

> `/ICL/data/`
	
      1.interventions_edited dates for intervention, downloaded from ICL github website, with New York's information added
      
      2.pop_ifr_edited population for each country/region, downloaded from ICL github website, with New York's information added
      
      3.serial_interval.csv the assumed distribution of time lag between infection and death, downloaded from ICL github website
      
      4. us-states.csv case and death number of regions in US, input for loadData.R dowloaded from https://github.com/nytimes/covid-19-data/blob/master/us-states.csv on April 27
      
      5. us-states_tillMay1.csv case and death number of regions in US, input for loadData_NY_May1.R, downloaded from https://github.com/nytimes/covid-19-data/blob/master/us-states.csv on May 3
      
      6. data_merge_JHU_NY.RData case and death number of New York until April 15, output of loadData.R
      
      7. data_merge_JHU_NY_tillMay1.RData case and death number of New York until May 1, output of loadData.R
            
> `/ICL/model/`

      stan-models model for ICL model fitting, downloaded from ICL github website, version 2



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

<br/>

## Data Sources
* Country-level data for Italy and India:   [JHU CSSE COVID-19 GitHub](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data)

* State-level data for New York:   [New York Times data](https://github.com/nytimes/covid-19-data)



<br/>

## Contributors
* Neha Agarwala, Department of Mathematics and Statistics, University of Maryland, Baltimore County
* Jin Jin, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Prosenjit Kundu, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Yi Wang, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* Ruzhang Zhao, Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
* [Nilanjan Chatterjee](https://nilanjanchatterjee.org/), Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
 

