
for(Species in c(2)){     # Select a species: 19 - GSS, 9 - wolffish, 2 - haddock
  
  tyr <- 2018                    # set last year of data input

#Note: haddock resulting in index x4 for some reason...
load_tyr_data <- TRUE        # needs to be run each year to load this year's index and catch data to local tables 
create_tyr_data <- TRUE        # needs to be run each year to create this year's CSV files
create_previous_years<-TRUE    # needs to be run the first time a model is created to form earlier years


source('../general_report/01-setup.R')
if(load_tyr_data){source('../general_report/02-length-based_indices.R')}
if(create_tyr_data){
  source('../general_report/05-catch_at_age.R')
  source('../general_report/06-create_SAM_input.R')
}
#source('../general_report/07-special_mod_SAM_input.R')
#source('../general_report/08-run_SAM_model.R')  

}