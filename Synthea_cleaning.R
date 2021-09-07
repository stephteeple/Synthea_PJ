
# Set up 
library(dplyr)
library(data.table)
library(tidyr)
library(table1)
library(flextable)

### For now, it's better to store any directories that you're using in a 
### character string, like I've done below. This makes it easier for people 
### using your code to change it if necessary - it's also easy to mess up or 
### break your code by being in the wrong directory. This prevents that. 
### # directories 
mydir <- "U:/Synthea_privacy_justice" # fill in with your own directory!



# Data -----------------------------------------------------------------------------

### When I have big datasets that take a while to read in, I also like to 
### keep the original in memory (e.g, patients_orig). It saves time. 
##Importing data
patients_orig <- fread(paste0(mydir, "/synthea/output/csv/patients.csv"))
conditions_orig <- fread(paste0(mydir, "/synthea/output/csv/conditions.csv"))
procedures_orig <- fread(paste0(mydir, "/synthea/output/csv/procedures.csv"))
patients <- patients_orig
conditions <- conditions_orig
procedures <- procedures_orig





# Data prep --------------------------------------------------------------------------

### In this data cleaning script, let's not filter for patients with MI until 
### we have to - that keeps us from having to clean the same data over and over 
### again while we change up analyses. 

# Calculate patient age (as of August 6th 2021, when this dataset was created)
patients$BIRTHDATE <- ymd(patients$BIRTHDATE)
patients$age <- floor(as.numeric((ymd("2021-08-06") - patients$BIRTHDATE)/365.25))
patients$DEATHDATE <- ymd(patients$DEATHDATE)


##Eliminating extra variables, renaming columns and filtering  
patients <- patients %>% 
  subset(select = c("Id","RACE", "ETHNICITY", "GENDER", "age", "BIRTHDATE", "DEATHDATE")) %>% 
  rename(patient = Id,sex = GENDER,race = RACE, ethnicity = ETHNICITY, birthdate = BIRTHDATE,
         deathdate = DEATHDATE)



conditions <- conditions %>%
  subset(select = c("PATIENT","START","DESCRIPTION")) %>% 
  rename(start_condition = 
           START,condition = DESCRIPTION,patient = PATIENT) 
  # filter(condition == "Myocardial Infarction") # only for Myocardial infarction

### reshape conditions wide to get each row to be a unique patient (rather than encounter)
keep_conditions <- c("Myocardial Infarction")
condition_names <- c("MI")
conditions <- conditions %>%
  filter(condition %in% keep_conditions) %>%
  pivot_wider(id_cols = patient, names_from = condition, values_from = start_condition) # In this dataset, patients only ever have 1 MI
colnames(conditions) <- c("patient", condition_names)

procedures <- procedures %>%
  subset(select = c("PATIENT","DESCRIPTION")) %>%
  rename(procedure = DESCRIPTION,
         patient = PATIENT) 
  # filter(procedure == "Coronary artery bypass grafting" |
  #        procedure ==  "Percutaneous coronary intervention")







# merge ----------------------------------------------------------------------------- 

# Reshape procedures
procedures$PCI <- ifelse(procedures$procedure == "Percutaneous coronary intervention", 1, 0) # create binary indicator variable for PCI
procedures$CABG <- ifelse(procedures$procedure == "Coronary artery bypass grafting", 1, 0) # create binary indicatory variable for CABG
procedures$procedure <- NULL # no longer need original 'procedure' variable 
procedures <- procedures %>% # to get 1 patient per row, group dataset by patient ids and keep only the maximum value for 'PCI' and 'CABG' variables
  group_by(patient) %>%
  summarise_all(max) # this tells us whether each patient had each procedure (e.g., PCI and CABG == 1)

# Merge all
df <- conditions %>% full_join(patients, by="patient") # full join so we keep all patients at this step, even those who did not have MI
df <- full_join(x = df, y = procedures, by = "patient")
df <- df %>% replace_na(list(PCI = 0, CABG = 0))

summary(df$PCI) # 92.37% of patients with MI got PCI
summary(df$CABG) # 92.37% of patients with MI got CABG




# Write merged data ----------------------------------------------------------------

### Again here, using the string saved in mydir so you don't have to getwd() again. 
### fwrite is like fread - it's faster than write.csv. 
fwrite(df, paste0(mydir, "/data/Synthea_merged.csv"))



#Done 