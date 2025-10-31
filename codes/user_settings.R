###### SOME SETTINGS, YOU MAY CHANGE THESE TO YOUR NEEDS
##############################################################################################################################
##############################################################################################################################

GLOBIOM.file <- "inputs/Output_Baseline_forMichi.gdx" # set location of the accelerator merge file!!!
# -------------------------
# Configuration / args
# -------------------------
args <- commandArgs(trailingOnly = TRUE)
gdx_file <- ifelse(length(args) >= 1, args[1], GLOBIOM.file)
out_dir  <- ifelse(length(args) >= 2, args[2], "./outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

##############################################################################################################################
##############################################################################################################################

##### DONE WITH SETTINGS, YOU MAY RUN THAT STUFF NOW