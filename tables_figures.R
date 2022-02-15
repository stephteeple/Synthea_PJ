##
##
##



# libraries
library(dplyr)
library(data.table)
library(tidyr)
library(table1)
library(flextable)
library(lubridate)


# arguments
mydir <- "C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice"
datetime <- "2021_11_12_12_22_45"

# Import data ----------------------------------------------------------------------

# Importing
df_orig <- fread(paste0(mydir, "/data/Synthea_merged_", datetime, ".csv"))
df <- df_orig
df$COPD <- pmax(df$COPD_bronch, df$COPD_emph, na.rm = TRUE)

# Cleaning 
df$COPD <- as.factor(recode(as.character(df$COPD), .missing = 0, .default = 1))
df$MI <- as.factor(recode(as.character(df$MI), .missing = 0, .default = 1))
df$diabetes <- as.factor(recode(as.character(df$diabetes), .missing = 0, .default = 1))




# Table 1 --------------------------------------------------------------------------

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x, ), digits = 4),
       c("",
         "median (Q1-Q3)" =
           sprintf(paste("%s (",Q1,"- %s)"), MEDIAN,Q3)))
}

# table1(~ age + sex + race + COPD + MI + diabetes, data = df, render.continuous = my.render.cont)
table1(~ age + sex + race + COPD + MI + diabetes, data = df)
