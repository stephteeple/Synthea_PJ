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




# Arguments and set up ----------------------------------------------------------------

args <- fread("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/run_synthea_args2.csv")

setwd("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/synthea")

shell("gradlew.bat build check test")

datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
dir.create(paste0("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/synthea_raw_", datetime))

states <- args$state







# Synthea function ---------------------------------------------------------------------

synthea_state <- function(s) {
  
  # state and pop
  message(s)
  state_string <- gsub(pattern = "\\^ ", replacement = "_", s) # remove '^' for file names
  pop <- round(as.numeric(args[args$state == s, 4]))
 
  # synthea call
  # shQuote() gives the messed-up quotes escaping that 
  # has to happen for two-word states on Windows cmd
  syn <- shQuote(paste0('run_synthea.bat -p ', pop, ' -a 65-100 "', s, 
                        '" -c C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/mysynthea.properties')) 
  shell(paste0(syn), intern = TRUE)
 
  # organize output files
  dir.create(paste0("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/synthea_raw_", datetime, "/", state_string))
  # remove uneeded files here. 
  out_files <- list.files("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  for(f in out_files) file.rename(f, gsub('[.]csv',paste0('_', state_string, '.csv'), f))
  renamed_files <- list.files("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  file.copy(renamed_files,  
            to = paste0("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/synthea_raw_", datetime, "/", state_string),
            recursive = TRUE, copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)
  file.remove(renamed_files)
  
}




# Run ------------------------------------------------------------------------------------

tic("Run synthea at the state level")
lapply(states, synthea_state)
toc()

