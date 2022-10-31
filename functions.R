######################################################################################
## Steph Teeple
## 10/31/22
## Functions.R for the Synthea project 
######################################################################################

## libraries

library(data.table)



## directories


## functions


# 00a_run_synthea.R ----------------------------------------------------------------

synthea_test <- function(state, pop, datetime) {
  
  # run Synthea
  syn <- paste0('run_synthea.bat -p ', pop, ' -a 65-100 "', state, 
                '" -c C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/mysynthea.properties')
  shell(paste0(syn), intern = TRUE)
  
  # Save data
  dir.create(paste0("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/synthea_test_", datetime, "/", state))
  # remove uneeded files here. 
  out_files <- list.files("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  for(f in out_files) file.rename(f, gsub('[.]csv',paste0('_', state_string, '.csv'), f))
  renamed_files <- list.files("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  file.copy(renamed_files,  
            to = paste0("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/synthea_test_", datetime, "/", state),
            recursive = TRUE, copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)
  file.remove(renamed_files)
  
}




synthea_state <- function(s, datetime) {
  
  # state and pop
  message(s)
  state_string <- gsub(pattern = "\\^ ", replacement = "_", s) # remove '^' for file names
  pop <- round(as.numeric(args[args$state == s, 4]))
  
  # synthea call
  # shQuote() gives the messed-up quotes escaping that 
  # has to happen for two-word states on Windows cmd
  syn <- shQuote(paste0('run_synthea.bat -p ', pop, ' -a 65-100 "', s, 
                        '" -c C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/mysynthea.properties')) 
  shell(paste0(syn), intern = TRUE)
  
  # organize output files
  dir.create(paste0("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/synthea_raw_", datetime, "/", state_string))
  # remove uneeded files here. 
  out_files <- list.files("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  for(f in out_files) file.rename(f, gsub('[.]csv',paste0('_', state_string, '.csv'), f))
  renamed_files <- list.files("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  file.copy(renamed_files,  
            to = paste0("C:/Users/Steph/Dropbox/projects/Synthea_privacy_justice/data/synthea_raw_", datetime, "/", state_string),
            recursive = TRUE, copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)
  file.remove(renamed_files)
  
}