# libraries
library(dplyr)


datetime <- "2021_11_12_12_22_45" 


# Import data ----------------------------------------------------------------------

patients_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/patients.csv"))
conditions_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/conditions.csv"))
procedures_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/procedures.csv"))
observations_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/observations.csv"))
patients <- patients_orig
conditions <- conditions_orig
procedures <- procedures_orig
observations <- observations_orig

encounters_orig <- fread(paste0(mydir, "/data/synthea_dist_", datetime, "/encounters.csv"))
encounters <- encounters_orig

df_orig <- fread(paste0(mydir, "/data/Synthea_merged_", datetime, ".csv"))
df <- df_orig




# MI --------------------------------------------------------------------------------

### Are all patients diagnosed with MI hospitalized? 
##### A: No - a small number (n = 44) die without making to hospital. Will exclude these patients. 

check <- encounters[encounters$PATIENT == "009f2d4f-187b-ff88-38c2-1fe95c1bd040",] # patient had MI 03/22/1990
View(check) # Yes, emergency encounter on that date for DESCRIPTION = "Myocardial infarction" 

encounters <- rename(encounters, patient = PATIENT)
mi <- df[!is.na(df$MI),]
mi <- left_join(mi, encounters, by = "patient")
mi$mi_encounter <- ifelse(mi$DESCRIPTION == "Myocardial Infarction", 1, 0)
mi_enc <- mi %>% 
  group_by(patient) %>%
  summarise(mi_encounter = sum(mi_encounter))
head(mi_enc)
table(mi_enc$mi_encounter)

# Patients with no encounter for MI 
no_enc <- mi_enc[mi_enc$mi_encounter == 0,]
View(no_enc)
View(df[df$patient == "0cda58e0-bb57-c3bd-b3c4-eafe14a9ab54",]) # patient had MI on 2015-11-15
pt1 <- mi[mi$patient == "0cda58e0-bb57-c3bd-b3c4-eafe14a9ab54",]
View(pt1) # No ENCOUNTERCLASS = emergency for DESCRIPTION = Myocardial infarction (weirdly an "Encounter for check up (procedure)" the day AFTER he died from the MI though)

