##Code repository
#####Resources

#In spanish intro of R 
https://rsanchezs.gitbooks.io/rprogramming/content/chapter9/mutate.html
#####Packages
library(data.table)  ##fread
libeary(dplyr) ##For filtering a managing data

#####Functions

#setting up directory
setwd("~/[Folder 1]/[folder 2]") 

#importing data
<- fread("~/[Folder 1]/[folder 2]") 

#remove everything in the enviorment
rm(list=ls()) 

# %>% Cmd + Shift + M
first(x) %>% second(x) %>% third(x)

#dropping columns 
subset(cleaned_validation_data_040621, select 
       = c(""))

#remove objects
rm() 

#Verify count of a string
Df %>% str_count("String")

#Ifelse https://www.listendata.com/2017/03/if-else-in-r.html
mydata$x4 = ifelse(mydata$x2>150,1,0)

#lapply to apply a function acrross the dataset
lapply(dataset, function)

#If you want to calculate something from 2 variables and create it into a new one 
mutate(, Vector with therow s to be created into a column)
mutate(df, New varable=Measure between other variables) 
Example -> mutate(storms, ratio=pressure/wind, inverse=ratio^-1)

#Across all dataset + Creaing a new dataset from extranting a grouping by a column 
cleaned_validation_data_040621 %>% group_by(Source) %>%
  summarise(across(Diabetes:CHF, mean))

       