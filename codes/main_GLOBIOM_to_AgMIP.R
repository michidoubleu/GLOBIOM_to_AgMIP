###### GLOBIOM to AgMIP post processing

## Author: Michael Wögerer, (IIASA)

## Aim: Transforms a GLOBIOM output GDX into a CSV in the AgMIP reporting
## format (Model, Scenario, Region, Item, Variable, Year, Unit, Value),
## including the economic indicators (VAAD, BUDG).

## If you find bugs, feel free to fix them and send a pull request or email
## me via wogerer@iiasa.ac.at

### from the repository root call:
### Rscript codes/main_GLOBIOM_to_AgMIP.R inputs/my_file.gdx outputs/my_file.csv

rm(list=ls())

###### USER SETTINGS -- can also be given on the command line (ARG1, ARG2)
##############################################################################################################################

args <- commandArgs(trailingOnly = TRUE)

# GLOBIOM output GDX (with OUTPUT_AG, OUTPUT and REGION_AG_MAP)
GLOBIOM.file <- if (length(args) >= 1) args[1] else "inputs/GLOBIOM_output.gdx"

# Output CSV
output.file  <- if (length(args) >= 2) args[2] else "outputs/GLOBIOM_AgMIP.csv"

##############################################################################################################################
##### DONE WITH SETTINGS

if (!file.exists(GLOBIOM.file)) stop("GDX file not found: ", GLOBIOM.file)
dir.create(dirname(output.file), showWarnings = FALSE, recursive = TRUE)

source("codes/prep_GLOBIOM_to_AgMIP.R")
