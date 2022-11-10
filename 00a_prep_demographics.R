########################################################################################
## Steph Teeple
## 10/31/22 
## Create custom Medicare-aged demographics input file for Synthea. 
##
## Synthea uses census demographic data (age and race and ethnicity) from here: 
## https://www2.census.gov/programs-surveys/popest/datasets/2010-2015/cities/totals/
## and here: 
## https://www2.census.gov/programs-surveys/popest/datasets/2010-2015/counties/asrh/
## per the Synthea wiki. It uses the 2010 Census population from the county files 
## and (presumably) POPESTIMATE2015 from the subcounty file.
##
## Synthea also uses 2010-2014 5-year pooled ACS education and income estimates
## (from the S1501 and S1901 tables) in the default demographics.csv. I do not produce 
## a custom Medicare-age version of these because demographics.csv uses one set of 
## estimates for all age groups by geography (e.g., a synthetic patient age 18 from 
## County A will be 'exposed' to the same set of edu and income conditions as a 
## synthetic patient age 70). 
##
## Relatedly, I do not produce custom race/ethnicity distributiosn for 65+ (same reason-
## fixed distribution by place in demographics.csv). 
##
## When you look at the default demographics.csv and sort by county, some 
## estimates (e.g., age group, income, and edu distributions, etc) vary at the *subcounty*
## level. I don't know how Synthea is doing this - their documentation says that
## they only use total population estimates at the subcounty level (ie, the distributions
## shouldn't change).
##
#######################################################################################

## libraries
library(data.table)
library(dplyr)
# library(tidycensus)
library(tictoc)
library(tidyverse)
library(stringr)

# census_api_key("9b664cf83243d9f3110457ddd3f6ca02970b9a2f", install = TRUE, overwrite = TRUE)

## functions

## directories


# 1. Get data ----------------------------------------------------------------------

# Census data 
# sub <- fread("U:/Synthea_privacy_justice/data/from_census/sub-est2015_all.csv")
counties <- fread("U:/Synthea_privacy_justice/data/from_census/cc-est2015-alldata.csv")

  # Synthea uses year = 1 2010 Census population for county-level
  # https://www2.census.gov/programs-surveys/popest/datasets/2010-2015/counties/asrh/cc-est2015-alldata.pdf
  counties <- counties[counties$YEAR == 1,]
  
  # Only keep 65+ age groups, total population vars
  counties <- counties[counties$AGEGRP >= 14, ]
  counties <- select(counties, SUMLEV, STATE, COUNTY, STNAME, CTYNAME, AGEGRP, TOT_POP)

# Synthea default demographics file
dems <- fread("U:/Synthea_privacy_justice/synthea/src/main/resources/geography/demographics.csv")





# 2. Calculate 65+ total pops and age distributions (county level) -----------------

# Don't know how/why sometimes distributions vary at the subcounty level in 
# default demographics.csv. Documentation says the only thing they get 
# at the sub-county level is total population. 

# Therefore, we will calculate our new 65+ TOT_POP and age distributions 
# at the county-level only, and use the sub-county estimates only for 
# merging back on to demographics.csv. 

# Create county-level FIPS 
counties$county_fips <- paste0(str_pad(counties$STATE, width = 2, side = "left", pad = "0"), 
                             str_pad(counties$COUNTY, width = 3, side = "left", pad = "0"))

# Calculate new 65+ county-level total population and 65+ age group distributions
counties <- counties %>%
  group_by(county_fips) %>%
  mutate(TOT_POP_65 = sum(TOT_POP), 
         prop_65 = TOT_POP/TOT_POP_65)

# Reshape wide (unique on county)
counties <- pivot_wider(counties, id_cols = c(SUMLEV, STATE, COUNTY, STNAME, CTYNAME, county_fips, TOT_POP_65),
                     names_from = AGEGRP, names_prefix = "prop_65_", values_from = "prop_65")





# 3. Fix Dona Ana County in New Mexico -----------------------------------------------

# Apparently, there is only one US county with a special character in the name -
# Doña Ana County, New Mexico. The character is present in the census data. In the 
# default Synthea demographics file for some rows it is (1) not present and others
# (2) screwy - e.g., DoÃ±a Ana County. This messes up the merge based on 
# county name (resulting in no Synthea records produced for New Mexico).

# check <- dems[dems$STNAME == "New Mexico",]
# check <- arrange(check, CTYNAME)
# View(check) # Dona Ana County, no special characters

# see <- counties[counties$STNAME == "New Mexico",]
# see <- arrange(see, CTYNAME)
# View(see) # Doña Ana County


counties$CTYNAME[counties$CTYNAME == "Doña Ana County"] <- "Dona Ana County"
View(counties[counties$CTYNAME == "Dona Ana County",])

dems$CTYNAME[dems$CTYNAME == "DoÃ±a Ana County"] <- "Dona Ana County"




# 4.  Merge new counties 65+ data onto  Synthea default demongraphics file -----------

mydems <- left_join(x = dems, y = counties, by = c("STNAME", "COUNTY", "CTYNAME"))
nomatch <- anti_join(x = dems, y = counties, by = c("STNAME", "COUNTY", "CTYNAME")) # Now 0
# View(nomatch)






# 5. Create custom mydemogrphics.csv --------------------------------------------------


### Update age distribution columns 

# zero out younger ages
zero_cols <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")
mydems[, zero_cols] <- 0

# replace 65+ ages 
replace_cols <- c("14", "15", "16", "17", "18")
mydems <- select(mydems, -all_of(replace_cols))
colnames(mydems) <- sub("prop_65_", "", colnames(mydems))

# Update sub-est level total population estimates to reflect only 65+
# check <- mydems$TOT_POP_65/mydems$TOT_POP
# summary(check)
mydems$POPESTIMATE2015 <- mydems$POPESTIMATE2015*(mydems$TOT_POP_65/mydems$TOT_POP)

# Rename TOT_POP_65 -> TOT_POP
mydems$TOT_POP <- NULL
mydems <- mydems %>% rename(TOT_POP = TOT_POP_65)






# 5. Format and save ---------------------------------------------------------------

myorder <- names(dems)
mydems <- select(mydems, all_of(myorder))
fwrite(mydems, "U:/Synthea_privacy_justice/synthea/src/main/resources/geography/my65demographics.csv")
