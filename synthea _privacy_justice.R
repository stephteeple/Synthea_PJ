#Set up 
library(dplyr)
library(data.table)
library(tidyr)
library(table1)
library(flextable)
library(DescTools)
install.packages("DescTools")

##Importing data
patients <- fread("~/Documents/ST lab/Jaya lab /patients.csv")
conditions <- fread("~/Documents/ST lab/Jaya lab /conditions (1).csv")
procedures <- fread("~/Documents/ST lab/Jaya lab /procedures.csv")

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
         patient = PATIENT)

#Merging datasets (left = "conditions", right = "patients", because I only want the IDs of people
#with Myocardial Infarction)

df <- conditions %>% left_join(patients, by="patient") 

# I couln't left join "procedures" because the patients ID are repeated in this dataset,instead I created 2 new 
# variables in which I extracted just the two procedures of interest

procedure1 <- procedures %>% filter(procedure == "Coronary artery bypass grafting")
procedure2 <- procedures %>% filter(procedure == "Percutaneous coronary intervention")

df$CAG <- procedure1$procedure[match(df$patient, procedure2$patient)] #If matches patient id return name of procedure in the new variable "procedure1"
df$PCI <- procedure2$procedure[match(df$patient, procedure2$patient)]

# df$procedure1 <- ifelse(conditions$patient==procedure1$patient,procedure1$procedure,"") 
# I tried using ifelse but it only returned 16 matches, I don't know why


#table 1: Summary Statistics for Myocardial Infarction patients 

dg$start_condition <- as.Date(df$start_condition, format= "%Y-%m-%d") #filtering by date 2009 - 2010 and race groups of interest 

df <- df %>% mutate(sex = recode(sex, F = "female", M = "male"), 
         race = recode(na_if(race, ""), asian = "Asian", black = "Black or African-American", 
                       white = "White", 
                       native = "Native"),
         ethnicity = recode(na_if(ethnicity, ""), 
                            hispanic = "Hispanic or Latino", 
                            nonhispanic = "Not Hispanic or Latino"))

# df<- df %>% mutate(CAG = recode(na_if(CAG,""), Coronary artery bypass grafting = "Recieved procedure")))
#I have no idea why is not working 
                            
df_2 <- df
table1 <- table1(~ sex + race + ethnicity + CAG + PCI,
      data = df_2, 
      caption = "Table 1. Summary Statistics: Myocardial Infarction patients")
table1

#Doubts on proportions 

#df_3 <- df %>% filter(race == "black" | race == "white") 
# should I? because the proportion would be only Black vs White not the other races but the paper only considers Black and White


#df_3 <- df %>% filter(between(start_condition, as.Date("2009-01-01"), as.Date("2010-12-31"))) 
      #%>% arrange(start_condition) %>% filter(race == "black" | race == "white") 


# Table 2 : Proportion of myocardial infarction for White men, White women, Black men and Black women

variable_rs <- df %>% count(sex,race)  %>% 
  mutate(n = prop.table(n))  
variable_rs
tab_rs<- ftable(xtabs(n ~ sex + race, data = variable_rs)) 
tab_rs

#Table 3:  Proportion of patients that recieved CAG 
variable_CAG <- df %>% count(sex,race,CAG) %>% 
  mutate(n = prop.table(n)) 

tab_cag <- ftable(xtabs(n ~ sex + race + CAG, data = variable_CAG)) 
tab_cag


#Table 4: Proportion oF patients that recieved PCI 
variable_PCI <- df %>% count(sex,race,PCI) %>% 
  mutate(n = prop.table(n)) 

tab_pci <- ftable(xtabs(n ~ sex + race + PCI, data = variable_PCI))
tab_pci


