###### GLOBIOM to AgMIP post processing

## Author: Michael Wögerer, (IIASA)

## Aim: Provides a routine for seamless connection of GLOBIOM and 
## the Accelerator: https://accelerator.iiasa.ac.at/

## Notes: Use the user settings to add local specific settings, 
## such as root directory

## V1 of this script, if you find bugs, feel free to fix them and send 
## a pull request or email me via wogerer@iiasa.ac.at


### from main directory call: 
### Rscript codes/main_GLOBIOM_to_AgMIP.R path/to/my_file.gdx path/to/output_dir



rm(list=ls())

# source("codes/user_settings.R")
# source("codes/user_settings_SOC_scen.R")
# source("codes/user_settings_Baseline.R")
source("codes/user_settings_comprehensive.R")

source("codes/prep_GLOBIOM_agmip_accelerator_comprehensive.R")

