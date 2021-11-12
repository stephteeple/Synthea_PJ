# Setup ------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(flextable)
library(webshot) # needed to save flextable objects as images 


# mydir <- ("~/Documents/ST_lab/Jaya_lab") 

mydir <- "C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice"

# Data -------------------------------------------------------------------------

#Importing
df_orig <- fread(paste0(mydir, "/data/synthea_dist_092321/synthea_merged_db.csv"))
df <- df_orig

df <- df[!is.na(df$Diabetes),]


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
  mutate(proportion = n/total) %>% 
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


# Table Synthea -----------------------------------------------------------------------
table_HA1c <- tab_HA1c %>% mutate(HA1c = proportion)
table_cholesterol <- tab_cholesterol %>% mutate(Total_chol =proportion)

tableS <- table_HA1c %>% full_join(table_cholesterol, by = "race.sex") %>% 
  select(-c("n.x", "n.y", "proportion.x","proportion.y", "total.x")) %>% relocate(Total_chol, .before = total.y)

tableS$Source <- "Synthea"
tableS$HA1c <- format(round(tableS$HA1c), nsmall = 3) # display trailing zero decimals heres to emphasize that yes, all diabetes patients get these tests in Synthea. 
tableS$Total_chol <- format(round(tableS$Total_chol), nsmall = 3)


# Table2 paper
Source <- "Chou 2007" #Change name

race.sex <- c("African American Female", "White Female", 
              "African American Male", "White Male") 

HA1c <- c('0.869','0.896','0.836', '0.882') 
Total_chol <- c('0.903','0.932','0.886', '0.931') 
# total.x<- c('6693','35865','4334', '93.1') There should only be one total column - for the paper these numbers come from Table 3. We want the overall denominator. 
total.y <- c('7702', '5187', '40022', '43114')

tableP <- data.frame(Source, race.sex,HA1c,Total_chol,total.y) 
tableP <- tableP %>% arrange(race.sex)
table1 <- tableS %>% rbind(tableP) 

#Flex table
ftable1 <- as_grouped_data(x = table1, groups = c("Source"))
ftable1 <- as_flextable(ftable1) %>%
  italic(j = 1, i = ~ !is.na(Source), italic = TRUE, part = "body") %>%
  bold(j = 1, i = ~ !is.na(Source), bold = TRUE, part = "body") %>% 
  bold(bold = TRUE, part = "header") %>% colformat_double(i = ~ is.na(Source), j = "race.sex", digits = 0, big.mark = "") %>%
  fontsize(part = "all", size = 12) %>% 
  padding(i = ~ !is.na(Source), padding = 5 ) %>% line_spacing(space = .8, part = "body") %>% 
  line_spacing(space = 2, i = ~ !is.na(Source)) %>% 
  set_header_labels(race.sex = "Sex - race groups", total.y = "Total") %>% 
  add_header_lines(values = "Table 1: Proportions of Black and white female and male patients who have diabetes and recieved selected tests") %>% 
  align(j = c(2:4), align = "center", part = "all" ) %>% autofit() 

  ftable1
  
  
# Export ---------------------------------------------------------------------------
  
  
  # the 'format' argument saves the image with a date-time stamp 
  # (helpful if you're going to be generating figures multiple times.)
save_as_image(ftable1, paste0(mydir, "/figs/diabetes_table_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".png")) 


  