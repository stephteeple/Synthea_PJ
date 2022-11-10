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
library(maps)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

## directories
data_dir <- "U:/Synthea_privacy_justice/data/"

## functions
source("U:/Synthea_privacy_justice/code-synthea_privacy_justice/functions.R")



# Arguments and set up ----------------------------------------------------------------

args <- fread("U:/Synthea_privacy_justice/data/run_synthea_args.csv")

setwd("U:/Synthea_privacy_justice/synthea")

shell("gradlew.bat build check test")

datetime <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
# datetime <- "2022_11_04_10_15_54"

dir.create(paste0("U:/Synthea_privacy_justice/data/synthea_raw_", datetime))

states <- args$state







# Run ------------------------------------------------------------------------------------

# # Test synthea command
# synthea_test(state = "New^ Mexico", pop = 1, datetime = datetime)
# dt <- fread("U:/Synthea_privacy_justice/data/synthea_test_2022_10_31_14_03_55/California/patients_California.csv")
# dt$age <- as.numeric(difftime(as.Date("2022-10-31"), as.Date(dt$BIRTHDATE, units = "days"))/365.2422)
# dt$alive <- ifelse(is.na(dt$DEATHDATE), 1, 0)
# dt$alive <- factor(dt$alive, levels = c(0,1), labels = c("alive", "dead"))
# table1(~ age | alive, data = dt)



tic("Run synthea at the state level")
lapply(states, synthea_state, datetime)
toc()




# Checks ----------------------------------------------------------------------------------

# Data 
myfiles <- list.files(paste0(data_dir, "synthea_raw_", datetime), pattern = ".csv", 
                      recursive = TRUE, full.names = TRUE)
mypatientfiles <- grep(pattern = "patients", myfiles, value = TRUE)
mycolclasses <- c("numeric", rep("character", 2), rep("numeric", 3), rep("character", 14),
                  rep("numeric", 7))
patients <- rbindlist(lapply(myfiles[grep(pattern = "patients", myfiles)], 
                        function(x) fread(x, colClasses = mycolclasses)), fill = TRUE)


### By state 
# More here: https://remiller1450.github.io/s230s19/Intro_maps.html
state_map <- map_data("state")

# Number of patients 
myd <- patients %>% 
  group_by(STATE) %>%
  summarise(pat_count = n()) %>%
  select(STATE, pat_count) %>%
  rename("region" = "STATE")
state_map$region <- str_to_title(state_map$region) # harmonize capitalization
myd <- inner_join(x = state_map, y = myd, by = "region")

gg <- ggplot () + 
    geom_polygon(data = myd, aes(x = long, y = lat, group = group, fill = pat_count), 
                 color = "white") + 
    ggtitle("Number of patients by state")
gg
ggsave(gg, filename = paste0("U:/Synthea_privacy_justice/figures/synthea_cohort_check_state_patient_count_map_", datetime, ".png"))


# Average age of patients 

# Calculate age
patients$BIRTHDATE <- ymd(patients$BIRTHDATE)
patients$age <- floor(as.numeric((as.Date(ymd_hms(datetime)) - patients$BIRTHDATE)/365.25))
myd <- patients %>%
  group_by(STATE) %>%
  summarise(pat_mean_age = mean(age)) %>%
  select(STATE, pat_mean_age) %>%
  rename("region" = "STATE")
state_map$region <- str_to_title(state_map$region) # harmonize capitalization
myd <- inner_join(x = state_map, y = myd, by = "region")

gg <- ggplot () + 
  geom_polygon(data = myd, aes(x = long, y = lat, group = group, fill = pat_mean_age), 
               color = "white") + 
  ggtitle("Mean age of patients by state")
gg
ggsave(gg, filename = paste0("U:/Synthea_privacy_justice/figures/synthea_cohort_check_state_patient_mean_age_map_", datetime, ".png"))



# ### Table 1 of cohort 
# myfiles <- list.files(paste0(data_dir, "synthea_raw_", datetime), pattern = ".csv", 
#                       recursive = TRUE, full.names = TRUE)
# mypatientfiles <- grep(pattern = "patients", myfiles, value = TRUE)
# patients <- rbindlist(lapply(myfiles[grep(pattern = "patients", myfiles)], fread))



# ### Remove uneeded files to free up space 
# get_remove_files <- function(state) {
# 
#     state_string <- gsub(pattern = "\\^ ", replacement = "_", state) # Remove '^' from two-word states
#     print(state_string)
#     all_files <- list.files(paste0("U:/Synthea_privacy_justice/data/synthea_raw_2022_11_04_10_15_54/",
#                                    state_string, "/"), pattern = ".csv", full.names = TRUE)
#     # bad_files <- grep("allergies|claims|claims_transactions|immunizations|organizations|payer_transitions|payers|providers|supplies", 
#                       # all_files, value = TRUE)
#     bad_files <- grep("claims|claims_transactions", all_files, value = TRUE)
# 
# }
# my_remove_files <- lapply(args$states, get_remove_files)
# my_remove_files <- unlist(my_remove_files[1:25])
# file.remove(my_remove_files)
    
   