#####################################################
#####################################################
##
##   Validation Test: Second phase.The data analysis 
##                   for Synthea's Synthethic EHR Data
## 
##   Name: Luis Muñoz-Negrón       
##
#####################################################
#####################################################

## Session Setup ##
library(dplyr)
library(readr)
library(tidyverse)
library(survey)
library(flextable)
library(dplyr)
library(data.table)
library(ggplot2)


options(max.print=999999)
setwd("~/")
NHAMCS <- fread("Dropbox/Synthea_validation/NHAMCS_2007_2017_weights.csv")
synthea_validation_data_120120 <- fread("Dropbox/Synthea_validation/synthea_validation_data_120120.csv")

##########Data prep##########
# 1. NHAMCS

NHAMCS<- filter(NHAMCS, YEAR == "2016" | YEAR == "2017")

NHAMCS <- filter(NHAMCS, AGE >= 18)

NHAMCS$PATWT <- as.numeric(as.character(NHAMCS$PATWT))

NHAMCS$CKD<- ifelse(NHAMCS$CKD == 1, 1, 0)
NHAMCS$CHF<- ifelse(NHAMCS$CHF == 1, 1, 0)
NHAMCS$CAD<- ifelse(NHAMCS$CAD == 1, 1, 0)
NHAMCS$Diabetes <- ifelse(NHAMCS$DIABTYP0 == 1 | 
                            NHAMCS$DIABTYP1 == 1 | 
                            NHAMCS$DIABTYP2 == 1, 1, 0)

NHAMCS_weighted <- svydesign(id=~CPSUM, 
                             strata=~CSTRATM, 
                             weight=~PATWT, 
                             data=NHAMCS, nest=TRUE)


# 2. Synthea

Diabetes <- synthea_validation_data_120120 %>% filter_all(any_vars(str_detect(., pattern = "Diabetes")))
synthea_validation_data_120120$diabetes <- 0
synthea_validation_data_120120$diabetes <- ifelse(synthea_validation_data_120120$patient_id %in% Diabetes$patient_id, 1, synthea_validation_data_120120$diabetes)

CKD <- synthea_validation_data_120120 %>% filter_all(any_vars(str_detect(., pattern = "Chronic kidney disease stage 1")))
synthea_validation_data_120120$ckd <- 0
synthea_validation_data_120120$ckd <- ifelse(synthea_validation_data_120120$patient_id %in% CKD$patient_id, 1, synthea_validation_data_120120$ckd)
table(synthea_validation_data_120120$ckd)

CAD <- synthea_validation_data_120120 %>% filter_all(any_vars(str_detect(., pattern = "Coronary Heart Disease")))
synthea_validation_data_120120$cad <- 0
synthea_validation_data_120120$cad <- ifelse(synthea_validation_data_120120$patient_id %in% CAD$patient_id, 1, synthea_validation_data_120120$cad)

CHF <- synthea_validation_data_120120 %>% filter_all(any_vars(str_detect(., pattern = "Chronic congestive heart failure")))
synthea_validation_data_120120$chf <- 0
synthea_validation_data_120120$chf <- ifelse(synthea_validation_data_120120$patient_id %in% CHF$patient_id, 1, synthea_validation_data_120120$chf)

##########Statistics##########

# 1. Proportion NHAMCS & converting to values (Condition/Whole dataset)
N_cad<-svymean(~CAD, NHAMCS_weighted)
N_chf <- svymean(~CHF, NHAMCS_weighted)
N_ckd <- svymean(~CKD, NHAMCS_weighted)
N_diabetes <- svymean(~Diabetes, NHAMCS_weighted)


# 2. Proportion Synthea & converting to values (Condition/Whole dataset)
S_cad <- mean(synthea_validation_data_120120$cad, trim = 0, na.rm = FALSE)
S_chf <- mean(synthea_validation_data_120120$chf, trim = 0, na.rm = FALSE)
S_ckd <- mean(synthea_validation_data_120120$ckd, trim = 0, na.rm = FALSE)
S_diabetes <- mean(synthea_validation_data_120120$diabetes, trim = 0, na.rm = FALSE)

# 3. Proportion NHAMCS Social features (Working on it)
sex <- svyby(~CKD+CHF+CAD+Diabetes, ~SEX, NHAMCS_weighted, svymean, keep.var=FALSE)
sex$Source <- 'NHAMCS'
sex<- gather(sex, conditions, proportion,  2:5, factor_key=TRUE)
sex <- spread(sex, SEX, proportion)

race <- svyby(~CKD+CHF+CAD+Diabetes, ~RACEUN, NHAMCS_weighted, svymean, keep.var=FALSE)
race$Source <- 'NHAMCS'
race<- gather(race, conditions, proportion,  2:5, factor_key=TRUE)
race<- race[-c(1,8,15,22), ]
race <- spread(race, RACEUN, proportion)

sex.race<- sex %>% left_join(race)

ethnicity <- svyby(~CKD+CHF+CAD+Diabetes, ~ETHUN, NHAMCS_weighted, svymean, keep.var=FALSE)
ethnicity$Source <- 'NHAMCS'
ethnicity<- gather(ethnicity, conditions, proportion,  2:5, factor_key=TRUE) 
ethnicity<- ethnicity[-c(1, 4, 7, 10), ]
ethnicity <- spread(ethnicity, ETHUN, proportion)

N_social<- sex.race %>% left_join(ethnicity)
View(N_social)

#######Table 1. Summary Statistics###########

## Keep only the columns of interest from each dataset
NHAMCS_tab1 <- NHAMCS[, c("AGE", "SEX", "RACEUN", "ETHUN")]
synthea_tab1 <- synthea_validation_data_120120[, c("age", "sex", "race", "ethnicity")]

## Rename columns to match in each dataset 
names(NHAMCS_tab1) <- c("age", "sex", "race", "ethnicity")

## rowbind original NHAMCS and synthea datasets together 
## will allows to compare each dataset side-by-side in the final table 
NHAMCS_tab1$source <- "NHAMCS"
synthea_tab1$source <- "Synthea"
tab1 <- rbind(NHAMCS_tab1, synthea_tab1)

## Recode the responses in each column so we can 
## compare both sources directly, using 'recode' function from dplyr
tab1 <- tab1 %>%
  mutate(sex = recode(sex, Female = "female", Male = "male", F = "female", M = "male"), 
         race = recode(na_if(race, ""), asian = "Asian", black = "Black or African-American", 
                       "Black/African American" = "Black or African-American", white = "White", 
                       other = "Other", native = "'Native'", .missing = "Missing", 
                       "American Indian/AlaskaNative" = "American Indian/Alaskan Native"), 
         ethnicity = recode(na_if(ethnicity, ""), hispanic = "Hispanic or Latino", 
                            nonhispanic = "Not Hispanic or Latino", .missing = "Missing"))
tab1
tab1$race <- factor(tab1$race, levels = c("American Indian/Alaskan Native", "Asian", "Black or African-American", 
                                          "Native Hawaiian/Other Pacific Islander", "White", "More than one", 
                                          "'Native'", "Other", "Missing"))
tab1$ethnicity <- factor(tab1$ethnicity, levels = c("Hispanic or Latino", 
                                                    "Not Hispanic or Latino", "Missing")
                 
t1 <- table1(~ age + sex + race + ethnicity | source, data = tab1, overall = FALSE, caption = "Table 1. Summary Statistics")
t1


##########Table 2. Chronic Condition Proportion##########
tab2  <- data.frame(condition=c("Coronary Artery Disease", "Congestive Heart Failure", "Chronic Kidney Disease", "Diabetes", "Coronary Artery Disease", "Congestive Heart Failure", "Chronic Kidney Disease", "Diabetes" ),
                    proportion=c(S_cad, S_chf, S_ckd, S_diabetes,N_cad,N_chf, N_ckd, N_diabetes),
                    source=c("Synthea", "Synthea", "Synthea", "Synthea", "NHAMCS", "NHAMCS", "NHAMCS", "NHAMCS"))
tab2$proportion<- round(tab2$proportion, 3)
tab2
tab2 <- pivot_wider(tab2, id_cols = "condition", names_from = "source", values_from = "proportion")

ft <- flextable(tab2) 
ft <- set_caption(ft, caption = "Table 2. Chronic Condition Proportion")
ft


##########Barplots##########

df <- data.frame(condition=c("Coronary Artery Disease", "Congestive Heart Failure", "Chronic Kidney Disease", "Diabetes", "Coronary Artery Disease", "Congestive Heart Failure", "Chronic Kidney Disease", "Diabetes" ),
                 proportion=c(S_cad, S_chf, S_ckd, S_diabetes,N_cad,N_chf, N_ckd, N_diabetes),
                 source=c("Synthea", "Synthea", "Synthea", "Synthea", "NHAMCS", "NHAMCS", "NHAMCS", "NHAMCS"))



#1.
barplot1 <-ggplot(data=df, aes(x=condition, y=proportion, fill=source)) +
  geom_bar(stat="identity",width=.8, position=position_dodge()) + ggtitle('Chronic Condition Proportion') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=15, face="bold", margin = margin(15, 0, 15, 0))) +
  theme(axis.title.y = element_text(margin = margin(t = , r = 6 , b = 0, l = 10))) +
  theme(axis.title.x = element_text(margin = margin(t = 9, r = 0 , b = 10, l = 0)))
barplot1

#2.
barplot2 <- ggplot(data=df, aes(x=source, y=proportion, fill=condition)) +
  geom_bar(stat="identity", width=.8, position=position_dodge()) + ggtitle('Data Source Proportion') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=15, face="bold", margin = margin(15, 0, 15, 0))) +
  theme(axis.title.y = element_text(margin = margin(t = , r = 6 , b = 0, l = 10))) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0 , b = 5, l = 0)))
barplot2



                         
