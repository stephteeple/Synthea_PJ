# Setup ------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(flextable)
#library(table1)
#library(tidyverse)
#library(xtable)
#library(webshot)

# mydir <- ("~/Documents/ST_lab/Jaya_lab") 

mydir <- "C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice"

# Data -------------------------------------------------------------------------

#Importing
df_orig <- fread(paste0(mydir, "/data/synthea_dist_092321/synthea_merged_db.csv"))
df <- df_orig

# Only diabetes patients
df <- df[!is.na(df$Diabetes)]


# Stats ------------------------------------------------------------------------

# Counting patients with diabetes by sex and race
denom <- df  %>% 
  group_by(sex, race) %>%
  count() %>%
  rename(total = n) 

##### Observation Hemoglobin A1c/Hemoglobin.total in Blood

# Counting observations of HA1c within diabetes patients 
tab_HA1c <- df %>% 
  group_by(sex, race) %>% 
  count(HA1c) %>% 
  filter(HA1c == 1) %>% 
  full_join(denom, by = c("sex","race")) %>% 
  filter(race == "black" | race == "white") %>% 
  mutate(proportion = signif(n/total, 3)) %>% 
  mutate(sex = recode(sex, M = "Male", F = "Female"),
         race = recode(race, black = "African American", white = "White")) %>% 
  mutate(across(where(is.numeric), round, 3))

#Pasting names of categories 

tab_HA1c$race.sex <- paste(tab_HA1c$race, tab_HA1c$sex, sep= " ")      #Uniting sex and race to use the same categories from the paper

tab_HA1c <- tab_HA1c %>% ungroup() %>% 
  select(c(-race,-sex)) %>% 
  relocate(race.sex, .before = HA1c) %>% 
  arrange(race.sex)


##### Observation Total Cholesterol

tab_cholesterol <- df %>% 
  group_by(sex, race) %>% 
  count(Total_chol) %>% 
  filter(Total_chol == 1) %>% 
  full_join(denom, by = c("sex","race")) %>% 
  filter(race == "black" | race == "white") %>% 
  mutate(proportion = n/total) %>% 
  mutate(sex = recode(sex, M = "Male", F = "Female"),
         race = recode(race, black = "African American", white = "White")) %>% 
  mutate(across(where(is.numeric), round, 3))

tab_cholesterol$race.sex <- paste(tab_cholesterol$race, 
                                  tab_cholesterol$sex, sep= " ")  

tab_cholesterol <- tab_cholesterol %>% ungroup() %>% 
  select(c(-race,-sex)) %>% 
  relocate(race.sex, .before = Total_chol) %>% 
  arrange(race.sex)