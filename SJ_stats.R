 #setup
 library(data.table)
 library(dplyr)
 library(flextable)
 #library(table1)
 #library(tidyverse)
 #library(xtable)
 
 # directories 
mydir <- "U:/Synthea_privacy_justice"
 


# Import data ----------------------------------------------------------------------

df_orig <- fread(paste0(mydir, "/data/Synthea_merged.csv"))
df <- df_orig
df$Source <- "Synthea"






# Replicate Table 1 ----------------------------------------------------------------

# "Characteristics of black and white men and women hospitalized with acute myocardial infarction (AMI)

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
tab1 <- tab %>% rbind(df_ra) %>% relocate(race_sex, .before = proportion)                                     #Stacking proportions from both sources


tab1
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


### So the reason you got such different results with your Table 1 replication is because
### you used a different denominator. If you look closely at the research article, 
### the results for Table 1 are 'rate per 10,000 Medicare enrollees'. 
### With all patients now included in Synthea_merged.csv, we can calculate the equivalent 
### values in the Synthea data. For this value as well, it's important to look 
### only across 1 year - I'll do it for 2009-2010. 

### Subset to people who were Medicare enrollees in 2009-2010 (approximately everyone
### > 65 years old at the time)

### Remove everyone who died before this year 
tab1 <- df[df$deathdate > "2010-01-01" | is.na(df$deathdate), ]

### calculate age at start of 2010
tab1$age_start_2010 <- floor(as.numeric((ymd("2010-01-01") - tab1$birthdate)/365.25))

### Keep only 65+ 
tab1 <- tab1[tab1$age_start_2010 >= 65,]

### Recode those who had MI in 2010 
tab1$MI_2010 <- ifelse(tab1$MI >= "2010-01-01" & tab1$MI < "2011-01-01", 1, 0)

### Calculate sex and race-specific rates of MI 
### TODO: don't have a large enough dataset to do year-specific rates.
check <- tab1 %>%
  group_by(race, sex) %>%
  summarise







# Table 2 ---------------------------------------------------------------------------

# filter for patients who had MI 
tab2 <- df %>%
  filter(!is.na(MI))


### So your problem with this table is each group needs to use its own respective denominator 
### For example, only 4 Black women had MIs in this dataset (explaining the original low
### proportion you got.) See footnote a under Table 2: "The denominator is AMI patients in each stratum..." 

### Calculate denominators (AMI patients in each stratum)
denom <- tab2 %>% 
  group_by(sex, race) %>%
  count() %>%
  rename(total = n)


# Percentage of black and white men and women who underwent selected procedures within 30 days of admission for AMI
tab_CABG <- tab2 %>% 
  count(sex,race, CABG, Source) %>%                # Counts of recieved procedure by sex-race
  full_join(denom, by = c("sex", "race")) %>%      # Merge on stratum-specific denominators
  mutate(proportion = n/total) %>%                 # Divide counts by totals 
  filter(CABG == "1") %>%                                           #Filtering proportion in which recieving CABG is TRUE
  filter(race == "black" | race == "white") %>%                        
  mutate(CABG = proportion, 
         sex = recode(sex, M = "men", F = "women"),
         race = recode(race, black = "Black or African-American", white = "White")) %>% 
  mutate(across(where(is.numeric), round, 2))

tab_CABG$race_sex <- paste(tab_CABG$race, tab_CABG$sex, sep= " ")      #Uniting sex and race to use the same categories from the paper

tab_CABG <- tab_CABG %>% 
  select(c(-race,-sex,-n,-proportion)) %>% 
  relocate(race_sex, .before = CABG) %>% 
  arrange(race_sex)



tab_PCI <- tab2 %>% 
  count(sex,race, PCI, Source) %>%                 # Counts of recieved procedure by sex-race
  full_join(denom, by = c("sex", "race")) %>%      # Merge on stratum-specific denominators
  mutate(proportion = n/total) %>%                 # Divide counts by totals 
  filter(PCI == "1") %>%                           # Filtering proportion in which recieving PCI is TRUE
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

## creating table with information from the paper
Source <- 'Singh et al 2014'
race_sex <- c("Black or African-American men", "Black or African-American women", 
                          "White men", "White women") 
CABG <- c('0.083','0.051','0.116','0.059')           #Proportion: Among those who underwent CABG, number (%) during the index admissionb
PCI <- c('0.326','0.242','0.407','0.305')         #Proportion: Among those who underwent PCI, number (%) during the index admissionb
total <- c("16209", "11446", "148622", '153962')
df_procedure <- data.frame(Source, race_sex, CABG, PCI, total) #creating dataframe

tab2 <- tab_CABG %>% 
  select(-total) %>%
  left_join(tab_PCI, by="race_sex") %>% 
  rbind(df_procedure)

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
  footnote(value = as_paragraph("The denominators (total) for Synthea is the number of synthetic patients in each stratum who have had an MI. The denominators (total) for Singh 2014 is the number of Medicare enrollees in each stratum who were hospitalized with acute MI in 2009-2010. "), 
           ref_symbols = "") %>%
  autofit() 

ftable2


#Done