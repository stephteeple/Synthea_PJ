# Setup ------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(flextable)
library(webshot) # needed to save flextable objects as images 


# mydir <- ("~/Documents/ST_lab/Jaya_lab") 

mydir <- "C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice"
datetime <- "2021_11_12_12_22_45"

# Data -------------------------------------------------------------------------

# Importing
df_orig <- fread(paste0(mydir, "/data/Synthea_merged_", datetime, ".csv"))
df <- df_orig

# Filter for only patients with selected condition
df$COPD <- pmax(df$COPD_bronch, df$COPD_emph, na.rm = TRUE) # combine two types of COPD into one
df <- df[!is.na(df$COPD),]
df$COPD_bronch <- NULL
df$COPD_emph <- NULL


# Stats ------------------------------------------------------------------------

# Counting patients with COPD by race
denom <- df  %>% 
  group_by(race) %>%
  count() %>%
  rename(total = n) 

##### Procedure = pulmonary rehabilition

# Counting observations of pulm_rehab within COPD patients 
tab_pulm_rehab <- df %>% 
  group_by(race) %>% 
  count(pulm_rehab) %>% 
  filter(pulm_rehab == 1) %>% 
  full_join(denom, by = "race") %>% 
  filter(race == "black" | race == "white") %>% 
  mutate(proportion = n/total) %>% 
  mutate(race = recode(race, black = "African American", white = "White")) %>% 
  mutate(across(where(is.numeric), round, 3))

# #Pasting names of categories 
# 
# tab_pulm_rehab$race.sex <- paste(tab_pulm_rehab$race, tab_pulm_rehab$sex, sep= " ")      #Uniting sex and race to use the same categories from the paper
# 
# tab_pulm_rehab <- tab_pulm_rehab %>% ungroup() %>% 
#   select(c(-race,-sex)) %>% 
#   relocate(race.sex, .before = pulm_rehab) %>% 
#   arrange(race.sex)



# Table Synthea -----------------------------------------------------------------------
table_pulm_rehab <- tab_pulm_rehab %>% mutate(pulm_rehab = proportion)
tableS <- table_pulm_rehab
tableS$Source <- "Synthea"
tableS$proportion <- NULL
tableS$pulm_rehab <- as.character(tableS$pulm_rehab)
tableS$total <- as.character(tableS$total)
tableS$n <- NULL


# Table2 paper
Source <- "Nishi 2016" #Change name
race <- c("African American", "White")
pulm_rehab <- c('0.0265', '0.0333') 
total <- c('5244', '89258')

tableP <- data.frame(Source, race, pulm_rehab, total) 
tableP <- tableP %>% arrange(race)
table1 <- tableS %>% rbind(tableP) 

#Flex table
ftable1 <- as_grouped_data(x = table1, groups = c("Source"))
ftable1 <- as_flextable(ftable1) %>%
  italic(j = 1, i = ~ !is.na(Source), italic = TRUE, part = "body") %>%
  bold(j = 1, i = ~ !is.na(Source), bold = TRUE, part = "body") %>% 
  bold(bold = TRUE, part = "header") %>% colformat_double(i = ~ is.na(Source), j = "race", digits = 0, big.mark = "") %>%
  fontsize(part = "all", size = 12) %>% 
  padding(i = ~ !is.na(Source), padding = 5 ) %>% line_spacing(space = .8, part = "body") %>% 
  line_spacing(space = 2, i = ~ !is.na(Source)) %>% 
  set_header_labels(race = "Race groups", total = "Total") %>% 
  add_header_lines(values = "Table 1: Proportions of Black and white patients who have COPD and recieved pulmonary rehabilitation") %>% 
  align(j = c(2:3), align = "center", part = "all" ) %>% autofit() 

ftable1


# Export ---------------------------------------------------------------------------


# the 'format' argument saves the image with a date-time stamp 
# (helpful if you're going to be generating figures multiple times.)
save_as_image(ftable1, paste0(mydir, "/figs/COPD_table_", format(Sys.time(), "%Y-%m-%d_%H.%M"), ".png")) 


