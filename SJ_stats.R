 #setup
 library(data.table)
 library(dplyr)
 library(flextable)
 #library(table1)
 #library(tidyverse)
 #library(xtable)
 
#import

df_orig <- fread("~/Documents/ST_lab/Jaya_lab/Validation_Study/output/Synthea_merged.csv")
df <- df_orig
df$Source <- "Synthea"

#Replicating Table 1 from research article: "Characteristics of black and white men and women hospitalized with acute myocardial infarction (AMI)

tab <- df %>% count(sex,race,Source) %>%                             #Frequency of Myocardial infarction by race and sex
  mutate(proportion = prop.table(n)) %>%                             #Converting frequency to proportion   
  filter(race == "black" | race == "white") %>%                      #Keeping races of interest
  mutate(across(where(is.numeric), round, 2)) %>%                    #Rounding by 2 digits
  mutate(sex = recode(sex, M = "men", F = "women"),
         race = recode(race, 
            black = "Black or African-American", white = "White"))  #Recoding categories

tab$race_sex <- paste(tab$race, tab$sex, sep= " ")                  #Joining sex and race to use the same categories from research article

tab <- tab %>% select(c(-race,-sex,-n)) %>%                         #Removing extra variables
   arrange(race_sex)


#creating table with information from the paper
Source <- 'Research article'

race_sex <- c("Black or African-American women", "White women", 
              "Black or African-American men", "White men") 

proportion <- c('43.1','41.0','46.7', '55.6')                      #using proportions of research article table 1 for 2009-2010


df_ra <- data.frame(Source,race_sex,proportion)                    #Creating dataframe with information from research paper
df_ra <- df_ra %>% arrange(race_sex)
tab1 <- tab %>% rbind(df_ra)                                      #Stacking proportions from both sources



#creating flextable1
ftable1 <- as_grouped_data(x = tab1, groups = c("Source"))
ftable1 <- as_flextable(ftable1) %>%
  italic(j = 1, i = ~ !is.na(Source), 
         italic = TRUE, part = "body") %>%                      #Applying italic to source row
  bold(j = 1, i = ~ !is.na(Source),                             #Applying bold to source row
       bold = TRUE, part = "body") %>% 
  bold(bold = TRUE, part = "header") %>%  
  colformat_double(i = ~ is.na(Source), 
                   j = "race_sex", digits = 0,           
                   big.mark = "") %>%     
  fontsize(part = "all", size = 12) %>%                        #Changing font size
  padding(i = ~ !is.na(Source), padding = 4 ) %>% 
  line_spacing(space = .8, part = "body") %>%                  #Adding line spacing between rows
  line_spacing(space = 1.5, i = ~ !is.na(Source)) %>% 
  set_header_labels(race_sex = "Sex - race groups",            #Renaming column for the table
                    proportion = "Proportion") %>% 
  add_header_lines(values = "Table 1: Proportion of Patients with myocardial infarction") %>% 
  autofit()

ftable1




#Table 2 Percentage of black and white men and women who underwent selected procedures within 30 days of admission for AMI
tab_CABG <- df %>% count(sex,race, CABG, Source) %>%                #Frequency of recieved procedure by sex-race
  mutate(proportion = prop.table(n)) %>%                            #Converting frequency into proportion 
  filter(CABG == "1") %>%                                           #Filtering proportion in which recieving CABG is TRUE
  filter(race == "black" | race == "white") %>%                        
  mutate(CABG = proportion, 
         sex = recode(sex, M = "men", F = "women"),
         race = recode(race, black = "Black or African-American", white = "White")) %>% 
  mutate(across(where(is.numeric), round, 2))

tab_CABG$race_sex <- paste(tab_CABG$race, tab_CABG$sex, sep= " ")      #Uniting sex and race to use the same categories from the paper

tab_CABG <- tab_CABG %>% select(c(-race,-sex,-n,-proportion)) %>% 
  relocate(race_sex, .before = CABG) %>% arrange(race_sex)



tab_PCI <- df %>% count(sex,race, PCI, Source) %>% 
   mutate(proportion = prop.table(n)) %>% filter(PCI == "1") %>% 
  filter(race == "black" | race == "white") %>% 
  mutate(PCI = proportion, 
         sex = recode(sex, M = "men", F = "women"),
         race = recode(race, black = "Black or African-American", white = "White")) %>% 
  mutate(across(where(is.numeric), round, 2))


tab_PCI$race_sex <- paste(tab_PCI$race, tab_PCI$sex, sep= " ")  
tab_PCI <- tab_PCI %>% select(c(-race,-sex,-n,-proportion,-Source)) %>%
  relocate(race_sex, .before = PCI) %>% arrange(race_sex)

tab_PCI
tab_CABG

##creating table with information from the paper
Source <- 'Research article'
race_sex <- c("Black or African-American men", "Black or African-American women", 
                          "White men", "White women") 

CABG <- c('8.3','5.1','11.6','5.9')           #Proportion: Among those who underwent CABG, number (%) during the index admissionb
PCI <- c('32.6','24.2','40.7','30.5')         #Proportion: Among those who underwent PCI, number (%) during the index admissionb

df_procedure <- data.frame(Source,race_sex,CABG,PCI) #creating dataframe

tab2 <- tab_CABG %>% left_join(tab_PCI, by="race_sex") %>% rbind(df_procedure)

tab2


# Creating flextable 
ftable2 <- as_grouped_data(x = tab2, groups = c("Source"))
ftable2 <- as_flextable(ftable2) %>%
  italic(j = 1, i = ~ !is.na(Source), italic = TRUE, part = "body") %>%
  bold(j = 1, i = ~ !is.na(Source), bold = TRUE, part = "body") %>% 
  bold(bold = TRUE, part = "header") %>% colformat_double(i = ~ is.na(Source), j = "race_sex", digits = 0, big.mark = "") %>%
  fontsize(part = "all", size = 12) %>% 
  padding(i = ~ !is.na(Source), padding = 5 ) %>% line_spacing(space = .8, part = "body") %>% 
  line_spacing(space = 2, i = ~ !is.na(Source)) %>% 
  set_header_labels(race_sex = "Sex - race groups", proportion = "Proportion") %>% 
  add_header_lines(values = "Table 2: Proportion of black and white men and women who underwent selected procedures") %>%
  autofit() 

ftable2


#Done