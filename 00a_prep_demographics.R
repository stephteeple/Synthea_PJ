########################################################################################
## Steph Teeple
## 10/31/22 
## Create custom Medicare-aged demographics input file for Synthea. 
##
## Census data from here: 
## https://www2.census.gov/programs-surveys/popest/datasets/2010-2015/cities/totals/
## and here: 
## https://www2.census.gov/programs-surveys/popest/datasets/2010-2015/counties/asrh/
## per the Synthea wiki. 
#######################################################################################

## libraries
library(data.table)
library(dplyr)

## functions

## directories


# 1. Get data ----------------------------------------------------------------------

# Census data 
sub <- fread("U:/Synthea_privacy_justice/data/from_census/sub-est2015_all.csv")
counties <- fread("U:/Synthea_privacy_justice/data/from_census/cc-est2015-alldata.csv")

# Synthea default demographics file
dems <- fread("U:/Synthea_privacy_justice/synthea/src/main/resources/geography/demographics.csv")



# Merge on to Synthea demongraphics file ------------------------------------------

summary(counties$COUNTY)
summary(sub$COUNTY)

# Merge census sub-estimates file onto county-level file 
# Note: there are rows in the sub-est.csv that have county FIPS = 0 
# (residents in a city that reside outside of... any county? another state?)
# Dropping these for now
census <- left_join(x = counties, y = sub, by = c("STATE", "COUNTY", "STNAME"))



# Merge onto Synthea demographics file using state, county, and estimate (POPESTIMATE2015)
census <- select(census, STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2015)
census$join_check <- 1
check <- left_join(x = dems, y = census, by = c("STNAME", "CTYNAME", "POPESTIMATE2015"))
