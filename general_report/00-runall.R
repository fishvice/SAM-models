
tyr <- 2018                     # set last year of data input
Species <- 9                    # Select a species
load_tyr_data <- FALSE          # needs to be run once to load this year's index and catch 
                                # data to local tables but not again if no updates

source('../general_report/01-setup.R')
if(load_tyr_data){source('../general_report/02-length-based_indices.R')}
source('../general_report/05-catch_at_age.R')
source('../general_report/06-create_SAM_input.R')
source('../general_report/07-special_mod_SAM_input.R')
