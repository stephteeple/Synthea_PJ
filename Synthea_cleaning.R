
# Set up 
library(dplyr)
library(data.table)
library(tidyr)
library(table1)
library(flextable)

##Importing data

patients <- fread("~/Documents/ST_lab/Jaya_lab/patients.csv")
conditions <- fread("~/Documents/ST_lab/Jaya_lab/conditions.csv")
procedures <- fread("~/Documents/ST_lab/Jaya_lab/procedures.csv")

##Eliminating extra variables, renaming columns and filtering  
patients <- patients %>% 
  subset(select = c("Id","RACE", "ETHNICITY", "GENDER")) %>% 
  rename(patient = Id,sex = GENDER,race = RACE, ethnicity = ETHNICITY)

conditions <- conditions %>%
  subset(select = c("PATIENT","START","DESCRIPTION")) %>% 
  rename(start_condition = 
           START,condition = DESCRIPTION,patient = PATIENT)%>% 
  filter(condition == "Myocardial Infarction") #only for Myocardial infarction

procedures <- procedures %>%
  subset(select = c("PATIENT","DESCRIPTION")) %>%
  rename(procedure = DESCRIPTION,
         patient = PATIENT) %>% filter(procedure == 
                                         "Coronary artery bypass grafting" |
                                         procedure == 
                                         "Percutaneous coronary intervention")

#Merging datasets 

procedures$PCI <- ifelse(procedures$procedure == "Percutaneous coronary intervention", 1, 0) # create binary indicator variable for PCI
procedures$CABG <- ifelse(procedures$procedure == "Coronary artery bypass grafting", 1, 0) # create binary indicatory variable for CABG
procedures$procedure <- NULL # no longer need original 'procedure' variable 
procedures <- procedures %>% # to get 1 patient per row, group dataset by patient ids and keep only the maximum value for 'PCI' and 'CABG' variables
  group_by(patient) %>%
  summarise_all(max) # this tells us whether each patient had each procedure (e.g., PCI and CABG == 1)

# Note - it seems that all the patients who had procedures (n = 230) had BOTH PCI and CABG. No one had one or the other. 


df <- conditions %>% left_join(patients, by="patient") 
df <- full_join(x = df, y = procedures, by = "patient")
df <- df %>% replace_na(list(PCI = 0, CABG = 0))


summary(df$PCI) # 92.37% of patients with MI got PCI
summary(df$CABG) # 92.37% of patients with MI got CABG
getwd()
write.csv(df, "output/Synthea_merged.csv")



#Done 