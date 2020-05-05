## Notes for ICL model validation

The code was tested in R 3.6.3.

### Training Data Specification

* Training data for Italy is from 03/02/20 to 03/13/20 (2 weeks before peak), 03/20/20 (1 week before peak) or 03/27/20 (till peak). 


* Training data for New York is from 03/18/20 to 03/24/20 (2 weeks before peak), 03/31/20 (1 week before peak) or 04/07/20 (till peak). 


* The starting dates of the training data of Italy and New York were both selected as the first date when the daily increase in deaths exceeded 9. 


* Training data for the other 12 Europe countries were from 02/29/20 to 05/01/20.


### Contents

> `code/`

     1. Italy_NY_till_peak.R 
     
     code for training ICL model, making projection for New York and Italy, and generating plots for ICL model validation.
      
   Note: 
     * Training data includes the daily case and deaths number before peak date of New York and Italy, 
     and other 12 Europe countries till May 1st. 
     * For ICL model, self-isolation, public events banning and social distance encouraged 
     are all corresponding to the "stay at home" intervention in the IHME model in this article
      
     2. Italy_NY_1week.R 
     
     code for training ICL model, making projection for New York and Italy, and generating plots for ICL model validation.
      
   Note:
     Training data includes the daily case and deaths number 1 week prior to peak date of 
     New York and Italy, and other 12 Europe countries till May 1st.
      
     3. Italy_NY_2week.R
     code for training ICL model, making projection for New York and Italy, and generating plots for ICL model validation.
   Note:
     Training data includes the daily case and deaths number 2 weeks prior to peak date of 
     New York and Italy, and other 12 Europe countries till May 1st.
      
     4. DataFormAdjust.R 
     
     format the output csv file to for tableau input
      
     5. loadData.R
     
     load the data for the 13 Europe counties and New York, for the dates until April 15
      
     6. loadData_NY_May1.R load the data for New York for dates till May 1st
      

> `data/`
	
     1.interventions_edited dates for intervention, downloaded from ICL github website, with New York's information added
      
     2.pop_ifr_edited population for each country/region, downloaded from ICL github website, with New York's information added
      
     3.serial_interval.csv the assumed distribution of time lag between infection and death, downloaded from ICL github website
      
     4. us-states.csv case and death number of regions in US, input for loadData.R dowloaded from https://github.com/nytimes/covid-19-data/blob/master/us-states.csv on April 27
      
     5. us-states_tillMay1.csv case and death number of regions in US, input for loadData_NY_May1.R, downloaded from https://github.com/nytimes/covid-19-data/blob/master/us-states.csv on May 3
      
     6. data_merge_JHU_NY.RData case and death number of New York until April 15, output of loadData.R
      
     7. data_merge_JHU_NY_tillMay1.RData case and death number of New York until May 1, output of loadData.R
            
> `model/`

     stan-models model for ICL model fitting, downloaded from ICL github website, version 2
