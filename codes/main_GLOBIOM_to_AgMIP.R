###### GLOBIOM to AgMIP post processing

## Author: Michael Wögerer, (IIASA)

## Aim: Provides a routine for seamless connection of GLOBIOM and 
## the Accelerator: https://accelerator.iiasa.ac.at/

## Notes: Use the user settings to add local specific settings, 
## such as root directory

## V1 of this script, if you find bugs, feel free to fix them and send 
## a pull request or email me via wogerer@iiasa.ac.at



rm(list=ls())

source("codes/user_settings.R")

source("codes/prep_GLOBIOM_agmip_accelerator.R")


