###### SOME SETTINGS, YOU MAY CHANGE THESE TO YOUR NEEDS
##############################################################################################################################
##############################################################################################################################

GLOBIOM.file <- "inputs/output_4390_merged.gdx" # set location of the accelerator merge file!!!
# -------------------------
# Configuration / args
# -------------------------
args <- commandArgs(trailingOnly = TRUE)
gdx_file <- ifelse(length(args) >= 1, args[1], GLOBIOM.file)
out_dir  <- ifelse(length(args) >= 2, args[2], "./outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (length(args) >= 3) {
  scen.filter <- args[3:length(args)]
} else {
  scen.filter <- c()
}

rename.model <- "GLOBIOM"
accelerator.name <- "GLOBIOM_WP9"

##############################################################################################################################
##############################################################################################################################

##### DONE WITH SETTINGS, YOU MAY RUN THAT STUFF NOW