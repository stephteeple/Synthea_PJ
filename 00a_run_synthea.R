
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







# Run ------------------------------------------------------------------------------------

# Test synthea command 
dt <- synthea(state = "California", pop = 100, datetime = datetime)

tic("Run synthea at the state level")
lapply(states, synthea_state)
toc()

