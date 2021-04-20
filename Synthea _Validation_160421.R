#####################################################
#####################################################
##
##   Validation Test: Second phase.Data analysis 
##                   for Synthea's Synthethic EHR Data
## 
##   Name: Luis Muñoz-Negrón       Data: 15/04/2021
## 
#####################################################
#####################################################

library(dplyr)
library(data.table)
library(stringr)
library(flextable)
library(table1)
library(tidyr)
library(survey)
library(ggplot2)
library(arsenal)
library(tibble)

###Importing data 
setwd("~/Dropbox/Synthea_validation")
cleaned_validation_data_040621 <- fread("~/Dropbox/Synthea_validation/cleaned_validation_data_040621.csv")
NHAMCS_orig<- fread("~/Dropbox/Synthea_validation/NHAMCS_2007_2017_weights.csv")

########Data Manipulation for Condition's Proportion########

###Synthea
##Adding Source Column
cleaned_validation_data_040621$Source <- "Synthea"

##Turning Conditions to Binary Variables
#Diabetes 
Diabetes = cleaned_validation_data_040621 %>% 
  filter_all(any_vars(str_detect(., pattern = "Diabetes")))

cleaned_validation_data_040621$Diabetes <- ifelse(cleaned_validation_data_040621$patient_id %in% 
                                                    Diabetes$patient_id, 1, 0)

#Coronary Heart Disease = Coronary Heart Disease
CAD = cleaned_validation_data_040621 %>% 
  filter_all(any_vars(str_detect(., pattern = "Coronary Heart Disease")))

cleaned_validation_data_040621$CAD <- ifelse(cleaned_validation_data_040621$patient_id %in% 
                                               CAD$patient_id, 1, 0)

#Chronic Congestive Heart Failure 
CHF = cleaned_validation_data_040621 %>% 
  filter_all(any_vars(str_detect(., pattern = "Chronic congestive heart failure")))

cleaned_validation_data_040621$CHF <- ifelse(cleaned_validation_data_040621$patient_id %in% 
                                               CHF$patient_id, 1, 0)

#Chronic Kidney Disease Stage 1-3
CKD1 = cleaned_validation_data_040621 %>% 
  filter_all(any_vars(str_detect(., pattern = "Chronic kidney disease stage 1")))
CKD2 = cleaned_validation_data_040621 %>% 
  filter_all(any_vars(str_detect(., pattern = "Chronic kidney disease stage 2")))
CKD3 = cleaned_validation_data_040621 %>% 
  filter_all(any_vars(str_detect(., pattern = "Chronic kidney disease stage 3")))

#doubt about if the patients with CKD stage should be counted repeatedly 
#or if a patient has CKD stage 1 and Stage 2 should be counted as 1

cleaned_validation_data_040621$CKD <- ifelse(cleaned_validation_data_040621$patient_id %in% 
                                               CKD1$patient_id | cleaned_validation_data_040621$patient_id %in% 
                                               CKD2$patient_id | cleaned_validation_data_040621$patient_id %in% 
                                               CKD3$patient_id, 1, 0)

Synthea <-cleaned_validation_data_040621 

###NHAMCS
##Filtering by Year and Age
NHAMCS <- NHAMCS_orig %>% filter(YEAR == "2016" | YEAR == "2017") %>% 
  filter(AGE >= 18) 

##Adding Source Variable
NHAMCS$Source <- "NHAMCS"

##Uniting TRUE for Diabetes 
NHAMCS$Diabetes <- ifelse(NHAMCS$DIABTYP0 == 1 | 
                            NHAMCS$DIABTYP1 == 1 | 
                            NHAMCS$DIABTYP2 == 1, 1,0)


#Subset Variables
Synthea_C <- Synthea[, c("CKD","CAD", "CHF","Diabetes","Source")]
NHAMCS_C <- NHAMCS[, c("CKD","CAD", "CHF","Diabetes","Source")]

Synthea_SV <- Synthea[, c("age", "sex", "race", "ethnicity","Source")]
NHAMCS_SV <- NHAMCS[, c("AGE", "SEX", "RACEUN", "ETHUN", "Source")]

#Renaming NHAMCS columns to match Synthea
names(NHAMCS_C) <- c("CKD", "CAD", "CHF","Diabetes","Source")
names(NHAMCS_SV) <- c("age","sex", "race","ethnicity","Source")


##Binding Datasets
SN_SV<- rbind(NHAMCS_SV, Synthea_SV) #Subset for Social Variables
SN_C<- rbind(NHAMCS_C, Synthea_C) #Subset for Conditions 

###Recoding Social Variables
SN_SV <- SN_SV  %>% mutate(sex = recode(sex, Female = "female", Male = "male", F = "female", M = "male"), 
                           race = recode(na_if(race, ""), asian = "Asian", black = "Black or African-American", 
                                         "Black/African American" = "Black or African-American", white = "White", 
                                         other = "Other", native = "'Native'", .missing = "Missing", 
                                         "American Indian/AlaskaNative" = "American Indian/Alaskan Native"), 
                           ethnicity = recode(na_if(ethnicity, ""), hispanic = "Hispanic or Latino", 
                                              nonhispanic = "Not Hispanic or Latino", .missing = "Missing"))

SN_SV$race <- factor(SN_SV$race, levels = c("American Indian/Alaskan Native", "Asian", "Black or African-American", 
                                            "Native Hawaiian/Other Pacific Islander", "White", "More than one", 
                                            "'Native'", "Other", "Missing"))
SN_SV$ethnicity <- factor(SN_SV$ethnicity, levels = c("Hispanic or Latino", 
                                                      "Not Hispanic or Latino", "Missing"))


##########Results##########

###Table 1: Summary Statistics for all conditions
Table_SV<-table1(~ age + sex + race + ethnicity | Source, data = SN_SV, overall = FALSE, caption = "Table 1. Summary Statistics")
Table_SV


###Table 2. Proportions from each condition 
Tab <- SN_C %>% group_by(Source) %>% 
  summarise(across(CKD:Diabetes, mean))

Table2 <- Tab %>% column_to_rownames("Source") %>% t %>% as.data.frame %>% 
  rownames_to_column("Condition")
Table2$NHAMCS <- round(Table2$NHAMCS ,4)
Table2$Synthea <- round(Table2$Synthea ,4)
Table2 <- flextable(Table2)
Table2


###Practice: Creating a table of summary statistics for each condition (Template for Social variables in each condition) 
##CAD
#Subset Variables for Synthea 
CAD_s <- Synthea[Synthea$CAD == '1',]
CAD_n <- NHAMCS[NHAMCS$CAD == '1',]
CAD_n <- CAD_n[, c("AGE", "SEX", "RACEUN", "ETHUN", "Source")]
CAD_s <- CAD_s[, c("age", "sex", "race", "ethnicity", "Source")] #

#Renaming NHAMCS columns to match Synthea
names(CAD_n) <- c("age","sex", "race","ethnicity","Source")

#Binding Datasets
CAD_SV<- rbind(CAD_s, CAD_n)

#Recoding Social Variables
CAD_SV <- CAD_SV  %>% mutate(sex = recode(sex, Female = "female", Male = "male", F = "female", M = "male"), 
                             race = recode(na_if(race, ""), asian = "Asian", black = "Black or African-American", 
                                           "Black/African American" = "Black or African-American", white = "White", 
                                           other = "Other", native = "'Native'", .missing = "Missing", 
                                           "American Indian/AlaskaNative" = "American Indian/Alaskan Native"), 
                             ethnicity = recode(na_if(ethnicity, ""), hispanic = "Hispanic or Latino", 
                                                nonhispanic = "Not Hispanic or Latino", .missing = "Missing"))

CAD_SV$race <- factor(CAD_SV$race, levels = c("American Indian/Alaskan Native", "Asian", "Black or African-American", 
                                              "Native Hawaiian/Other Pacific Islander", "White", "More than one", 
                                              "'Native'", "Other", "Missing"))
CAD_SV$ethnicity <- factor(CAD_SV$ethnicity, levels = c("Hispanic or Latino", 
                                                        "Not Hispanic or Latino", "Missing"))
##Creating table for CAD
tableCAD <- tableby(Source ~ ., data = CAD_SV) 
tableCAD<-summary(tableCAD, title = "Coronary Artery Disease")
tableCAD

#or

Table_SV <-table1(~ age + sex + race + ethnicity | Source, data = CAD_SV, overall = 
                    FALSE, caption = "Table 3. Coronary Artery Disease: Summary Statistics")
Table_SV




###GGPLOTs
##Data manipulation 
Plot1<- Tab %>% gather(Conditions, Proportion, CKD:Diabetes)

#plot 1
barplot1 <-ggplot(data=Plot1, aes(x=Conditions, y=Proportion, fill=Source)) +
  geom_bar(stat="identity",width=.8, position=position_dodge()) + ggtitle('Chronic Condition Proportion') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=15, face="bold", margin = margin(15, 0, 15, 0))) +
  theme(axis.title.y = element_text(margin = margin(t = , r = 6 , b = 0, l = 10))) +
  theme(axis.title.x = element_text(margin = margin(t = 9, r = 0 , b = 10, l = 0))) + scale_color_distiller(palette = "Spectral")
barplot1

#plot 2
barplot2 <- ggplot(data=Plot1, aes(x=Source, y=Proportion, fill=Conditions)) +
  geom_bar(stat="identity", width=.8, position=position_dodge()) + ggtitle('Data Source Proportion') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=15, face="bold", margin = margin(15, 0, 15, 0))) +
  theme(axis.title.y = element_text(margin = margin(t = , r = 6 , b = 0, l = 10))) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0 , b = 5, l = 0)))
barplot2

########NHAMCS Weighted########

##Filtering by Age and Year 
NHAMCS_W<- NHAMCS_orig %>% filter(YEAR == "2016" | YEAR == "2017") %>% filter( AGE >= 18)

NHAMCS_W$PATWT <- as.numeric(as.character(NHAMCS$PATWT))

NHAMCS_W$Diabetes <- ifelse(NHAMCS_W$DIABTYP0 == 1 | 
                              NHAMCS_W$DIABTYP1 == 1 | 
                              NHAMCS_W$DIABTYP2 == 1, 1, 0)

NHAMCS_weighted <- svydesign(id=~CPSUM, 
                             strata=~CSTRATM, 
                             weight=~PATWT, 
                             data=NHAMCS, nest=TRUE)

##proportion for conditions 
svymean(~CAD + CHF + CKD + Diabetes, NHAMCS_weighted)

##Proportion of each condition by Social Variable 
#Sex
svyby(~CKD+CHF+CAD+Diabetes, ~SEX, NHAMCS_weighted, svymean, keep.var=FALSE)

#race
svyby(~CKD+CHF+CAD+Diabetes, ~RACEUN, NHAMCS_weighted, svymean, keep.var=FALSE)

#ethnicity
svyby(~CKD+CHF+CAD+Diabetes, ~ETHUN, NHAMCS_weighted, svymean, keep.var=FALSE)





