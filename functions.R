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
  
  # state and pop
  message("Running synthea test command...")
  message(paste0("State = ", state, ", pop = ", pop))
  state_string <- gsub(pattern = "\\^ ", replacement = "_", state) # remove '^' for file names
  
  # Check to make sure the default synthea output directory is clear 
  # (e.g., if we interrupted the previous run) - can mess up next run
  old_files <- list.files("U:/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  message(paste0(length(old_files), " old files found in Synthea dir"))
  remove(old_files)
  
  # run Synthea
  syn <- shQuote(paste0('run_synthea.bat -c U:/Synthea_privacy_justice/mysynthea.properties -p ',
                        pop, ' "', state, '"'))
  shell(paste0(syn), intern = TRUE)
  
  # Save data
  dir.create(paste0("U:/Synthea_privacy_justice/data/synthea_test_", datetime, "/", state_string), recursive = TRUE)
  # remove uneeded files here. 
  out_files <- list.files("U:/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  for(f in out_files) file.rename(f, gsub('[.]csv',paste0('_', state_string, '.csv'), f))
  renamed_files <- list.files("U:/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  file.copy(renamed_files,  
            to = paste0("U:/Synthea_privacy_justice/data/synthea_test_", datetime, "/", state_string),
            recursive = FALSE, copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)
  file.remove(renamed_files)
  
}




synthea_state <- function(state, datetime) {
  
  # state and pop
  message(state)
  state_string <- gsub(pattern = "\\^ ", replacement = "_", state) # remove '^' for file names
  pop <- round(as.numeric(args[args$states == state, wt_pop]))
  
  # synthea call
  # shQuote() gives the messed-up quotes escaping that 
  # has to happen for two-word states on Windows cmd
  syn <- shQuote(paste0('run_synthea.bat -c U:/Synthea_privacy_justice/mysynthea.properties -p ',
                        pop, ' "', state, '"'))
  shell(paste0(syn), intern = TRUE)
  
  # organize output files
  dir.create(paste0("U:/Synthea_privacy_justice/data/synthea_raw_", datetime, "/", state_string), recursive = TRUE)
  # remove uneeded files here. 
  out_files <- list.files("U:/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  for(f in out_files) file.rename(f, gsub('[.]csv',paste0('_', state_string, '.csv'), f))
  renamed_files <- list.files("U:/Synthea_privacy_justice/synthea/output/csv", pattern = ".csv", full.names = TRUE)
  file.copy(renamed_files,  
            to = paste0("U:/Synthea_privacy_justice/data/synthea_raw_", datetime, "/", state_string),
            recursive = TRUE, copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)
  file.remove(renamed_files)

  
}
