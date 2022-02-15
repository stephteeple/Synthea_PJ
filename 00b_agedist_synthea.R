######################################################################################
## In order to achieve a Medicare-eligible cohort, command-line option '-a 65-100'
## is specified. Currently, only uniform age distributions are supported. In this 
## script,  simulated data is trimmed to reflect the current US national age 
## distribution (for 65 and older) using 
## https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html

## Output is the prepped, but not yet cleaned, Synthea cohort. 
#####################################################################################

## libraries 
library(data.table)
library(lubridate)
library(dplyr)

# functions

# arguments
datetime <- "2021_11_12_12_22_45"
data_dir <- "C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/"
args <- fread("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/agedist_args.csv")


# Get data --------------------------------------------------------------------------

myfiles <- list.files(paste0(data_dir, "synthea_raw_", datetime), pattern = ".csv", 
                      recursive = TRUE, full.names = TRUE)


conditions <- rbindlist(lapply(myfiles[grep(pattern = "conditions", myfiles)], fread), fill = TRUE)
encounters <- rbindlist(lapply(myfiles[grep(pattern = "encounters", myfiles)], fread), fill = TRUE)
imaging_studies <- rbindlist(lapply(myfiles[grep(pattern = "imaging_studies", myfiles)], fread), fill = TRUE)
medications <- rbindlist(lapply(myfiles[grep(pattern = "medications", myfiles)], fread), fill = TRUE)
observations <- rbindlist(lapply(myfiles[grep(pattern = "observations", myfiles)], fread), fill = TRUE)
patients <- rbindlist(lapply(myfiles[grep(pattern = "patients", myfiles)], fread), fill = TRUE)
procedures <- rbindlist(lapply(myfiles[grep(pattern = "procedures", myfiles)], fread), fill = TRUE)

patients <- rename(patients, PATIENT = Id)
data_list <- list(conditions, encounters, imaging_studies, medications, observations, patients, procedures)
names(data_list) <- c("conditions", "encounters", "imaging_studies", "medications", "observations", "patients", "procedures")


# Age dist trim (patients.csv) --------------------------------------------------------------------

  
# age vars 
patients$BIRTHDATE <- ymd(patients$BIRTHDATE)
patients$age <- floor(as.numeric((ymd("2021-12-31") - patients$BIRTHDATE)/365.25))
breaks <- args$min
patients$age_strata <- cut(patients$age, breaks = c(breaks, Inf), right = FALSE)

# existing age_dist
tb <- patients %>%
  group_by(age_strata) %>%
  summarise(n = n()) %>%
  mutate(ext_prop = n/sum(n))
tb <- as.data.frame(left_join(tb, args, by = "age_strata"))

# new strata counts
# There will always need to be the most people in the 
# 65-70 strata - use this to determine new pop total 
new_total <- round(tb[tb$min == 65, "n"]*(1/tb[tb$min == 65, 'US_rel_percent']))
tb$new_strata_counts <- round(tb$US_rel_percent*new_total)

# randomly subsample
patients <- left_join(patients, tb, by = "age_strata")
new_patients <- patients %>%
  group_by(age_strata) %>%
  sample_n(new_strata_counts)






# Trim other .csv files -------------------------------------------------------------------

data_list <- lapply(data_list, function(x) x[x$PATIENT %in% new_patients$PATIENT])




# Write ----------------------------------------------------------------------------------

dir.create(paste0(data_dir, "synthea_dist_", datetime))
lapply(1:length(data_list), function(x) fwrite(data_list[[x]], 
                                              file = paste0(data_dir, "synthea_dist_", datetime, "/", names(data_list[x]), ".csv"), 
                                              row.names = FALSE))

