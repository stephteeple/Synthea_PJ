###################################################################################
## 
##
##
##
###################################################################################


# Set up 
library(dplyr)
library(data.table)
library(tidyr)
library(table1)
library(flextable)
library(lubridate)


mydir <- "C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice" # fill in with your own directory 

datetime <- "2021_11_12_10_24_09" # How to keep track of the data version you're using 

# Data -----------------------------------------------------------------------------

patients_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/patients.csv"))
conditions_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/conditions.csv"))
procedures_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/procedures.csv"))
observations_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/observations.csv"))
patients <- patients_orig
conditions <- conditions_orig
procedures <- procedures_orig
observations <- observations_orig




# Data prep --------------------------------------------------------------------------. 

# Calculate patient age (as of November 12th 2021, when this dataset was created)
patients$BIRTHDATE <- ymd(patients$BIRTHDATE)
patients$age <- floor(as.numeric((ymd("2021-11-12") - patients$BIRTHDATE)/365.25))
patients$DEATHDATE <- ymd(patients$DEATHDATE)


# patients.csv - Eliminating extra variables, renaming columns and filtering  
patients <- patients %>% 
  subset(select = c("PATIENT","RACE", "ETHNICITY", "GENDER", "age", "BIRTHDATE", "DEATHDATE")) %>% 
  rename(patient = PATIENT,sex = GENDER,race = RACE, ethnicity = ETHNICITY, birthdate = BIRTHDATE,
         deathdate = DEATHDATE)


# conditions.csv - eliminating columns, keeping selected conditions of interest, reshaping 
conditions <- conditions %>%
  subset(select = c("PATIENT","START","DESCRIPTION")) %>% 
  rename(start_condition = 
           START,condition = DESCRIPTION,patient = PATIENT) 

    # reshape wide to get each row to be a unique patient (rather than encounter)
    keep_conditions <- c("Myocardial Infarction", "Cardiac Arrest", "Diabetes")
    conditions <- conditions %>%
      filter(condition %in% keep_conditions) %>%
      mutate(condition = recode(condition, "Myocardial Infarction" = "MI", "Cardiac Arrest" = "cardiac_arrest", "Diabetes" = "diabetes")) %>%
      pivot_wider(id_cols = patient, names_from = condition, values_from = start_condition) # In this dataset, patients only ever have 1 MI
    
    ## combine MI and cardiac arrest 
    # conditions$MI <- pmax(conditions$MI, conditions$cardiac_arrest, na.rm = TRUE)
    # conditions$cardiac_arrest <- NULL

    
    
# procedures.csv - eliminating columns, keeping/recoding selected procedures of interest 
procedures <- procedures %>%
  subset(select = c("PATIENT","DESCRIPTION")) %>%
  rename(procedure = DESCRIPTION,
         patient = PATIENT) 

procedures$PCI <- ifelse(procedures$procedure == "Percutaneous coronary intervention", 1, 0) # create binary indicator variable for PCI
procedures$CABG <- ifelse(procedures$procedure == "Coronary artery bypass grafting", 1, 0) # create binary indicator variable for CABG
procedures$procedure <- NULL # no longer need original 'procedure' variable 
procedures <- procedures %>% # to get 1 patient per row, group dataset by patient ids and keep only the maximum value for 'PCI' and 'CABG' variables
  group_by(patient) %>%
  summarise_all(max) # this tells us whether each patient had each procedure (e.g., PCI and CABG == 1)


# observations.csv - eliminating columns, keeping/recoding selected observations of interest
observations <- observations %>%
  subset(select = c("PATIENT","DESCRIPTION")) %>%
  rename(observation = DESCRIPTION,
         patient = PATIENT) 

observations$HA1c <- ifelse(observations$observation == "Hemoglobin A1c/Hemoglobin.total in Blood", 1, 0) # binary indicator variable for HA1c
observations$Total_chol <- ifelse(observations$observation == "Total Cholesterol", 1, 0) # binary indicator variable for Total_chol
observations$observation <- NULL 
observations <- observations %>% 
  group_by(patient) %>%
  summarise_all(max) 



# merge ----------------------------------------------------------------------------- 

# Merge all
df <- conditions %>% full_join(patients, by="patient") # full join so we keep all patients at this step, even those who did not have MI
df <- full_join(x = df, y = procedures, by = "patient")
df <- full_join(x = df, y = observations, by = "patient")
df <- df %>% replace_na(list(PCI = 0, CABG = 0, HA1c = 0, Total_chol = 0))





# Write merged data ----------------------------------------------------------------

### Again here, using the string saved in mydir so you don't have to getwd() again. 
### fwrite is like fread - it's faster than write.csv. 
fwrite(df, paste0(mydir, "/data/Synthea_merged_", datetime, ".csv"))



#Done 