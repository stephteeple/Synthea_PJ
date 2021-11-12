###############################################################################
## Synthea Data cleaning for Diabetes's observations for
## total Hemoglobin A1c/Hemoglobin in Blood and Total cholesterol.
##
################################################################################

#On the other script you mentioned to not filter on cleaning but at the end I think it happened
# does that affect anything?

# Setup ------------------------------------------------------------------------
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)

# mydir <- ("~/Documents/ST_lab/Jaya_lab")
mydir <- "C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice"

# Data -------------------------------------------------------------------------

#Importing
patients_orig <- fread(paste0(mydir, "/data/synthea_dist_092321/patients.csv"))
conditions_orig <- fread(paste0(mydir, "/data/synthea_dist_092321/conditions.csv"))
observations_orig <- fread(paste0(mydir, "/data/synthea_dist_092321/observations.csv"))

patients <- patients_orig
conditions <- conditions_orig
observations <- observations_orig

# Data prep --------------------------------------------------------------------

# Calculate patient age
patients$BIRTHDATE <- ymd(patients$BIRTHDATE) #Format 
patients$age <- floor(as.numeric((ymd("2021-09-23") - patients$BIRTHDATE)/365.25))

###  Eliminating extra variables and renaming columns 
patients <- patients %>% 
  subset(select = c("PATIENT","RACE", "ETHNICITY", "GENDER", "age", "BIRTHDATE")) %>% 
  rename(patient = PATIENT, sex = GENDER,race = RACE, ethnicity = ETHNICITY, birthdate = BIRTHDATE)


conditions <- conditions %>%
  subset(select = c("PATIENT","START","DESCRIPTION")) %>% 
  rename(start_condition = 
           START,condition = DESCRIPTION, patient = PATIENT) 

### Reshaping for conditions wide to get each row to be a unique patient (rather than encounter)
keep_conditions <- c("Diabetes")
conditions <- conditions %>%
  filter(condition %in% keep_conditions) %>%
  pivot_wider(id_cols = patient, names_from = condition, values_from = start_condition)  #What is the purpose of Id_cols argument


observations <- observations %>%
  subset(select = c("PATIENT","DESCRIPTION")) %>%
  rename(observation = DESCRIPTION,
         patient = PATIENT) 



# merge ----------------------------------------------------------------------------- 

observations$HA1c <- ifelse(observations$observation == "Hemoglobin A1c/Hemoglobin.total in Blood", 1, 0) # binary indicator variable for HA1c
observations$Total_chol <- ifelse(observations$observation == "Total Cholesterol", 1, 0) # binary indicator variable for Total_chol
observations$observation <- NULL 
observations <- observations %>% 
  group_by(patient) %>%
  summarise_all(max) # I have no idea what is happening!!! I understand that is grouping the patients but on the web it shows that its supposed to output an result


#######How exactly summarise_all(max) works 

# Merge all
df <- conditions %>% full_join(patients, by="patient") # full join so we keep all patients at this step, even those who did not have Diabetes
df <- full_join(x = df, y = observations, by = "patient")
df <- df %>% replace_na(list(HA1c = 0, Total_chol = 0))

# summary(df$HA1c) # % of patients with diabetes received HA1c
# summary(df$Total_chol) # % of patients with diabetes received Total cholesterol 


# Write merged data ------------------------------------------------------------

fwrite(df, paste0(mydir,"/data/synthea_dist_092321/synthea_merged_db.csv"))

#Done 