########################################################################################
## This script runs the Synthea application at the ***Windows*** command line (via R).
## Synthea presently only works for state-level and below geographies. Thus, to 
## generate a US population, Synthea is executed at the state level, weighted by 
## relative population in each state (as of ACS 2019: 
## https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html). 
## 
## In order to achieve a Medicare-eligible cohort, option '-a 65-100' is specified.
## Currently, only uniform age distributions are supported. The simulated data is 
## trimmed to reflect the current US national age distribution (for 65 and older) 
## in 00b_agedist_synthea.R. 
##
## See mysynthea.properties for other parameters. 
## 
## Due to specifics of Windows command line, this script likely needs to be adapted 
## for other OS. 
#######################################################################################


## libraries
library(tictoc)
library(data.table)

## directories
data_dir <- "U:/Synthea_privacy_justice/data/"

## functions
source("U:/Synthea_privacy_justice/code-synthea_privacy_justice/functions.R")



# Arguments and set up ----------------------------------------------------------------

args <- fread("U:/Synthea_privacy_justice/data/run_synthea_args.csv")

setwd("U:/Synthea_privacy_justice/synthea")

shell("gradlew.bat build check test")

datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")

dir.create(paste0("U:/Synthea_privacy_justice/data/synthea_raw_", datetime))

states <- args$state







# Run ------------------------------------------------------------------------------------

# Test synthea command 
synthea_test(state = "New^ York", pop = 1, datetime = datetime)
# dt <- fread("U:/Synthea_privacy_justice/data/synthea_test_2022_10_31_14_03_55/California/patients_California.csv")
# dt$age <- as.numeric(difftime(as.Date("2022-10-31"), as.Date(dt$BIRTHDATE, units = "days"))/365.2422)
# dt$alive <- ifelse(is.na(dt$DEATHDATE), 1, 0)
# dt$alive <- factor(dt$alive, levels = c(0,1), labels = c("alive", "dead"))
# table1(~ age | alive, data = dt)



tic("Run synthea at the state level")
lapply(states, synthea_state, datetime)
toc()




# Checks ----------------------------------------------------------------------------------

# Table 1 of cohort 
myfiles <- list.files(paste0(data_dir, "synthea_raw_", datetime), pattern = ".csv", 
                      recursive = TRUE, full.names = TRUE)
mypatientfiles <- grep(pattern = "patients", myfiles, value = TRUE)
patients <- rbindlist(lapply(myfiles[grep(pattern = "patients", myfiles)], fread))
