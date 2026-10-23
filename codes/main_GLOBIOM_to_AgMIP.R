###### GLOBIOM to AgMIP post processing

## Author: Michael Wögerer, (IIASA)

## Aim: Provides a routine for seamless connection of GLOBIOM and
## the Accelerator: https://accelerator.iiasa.ac.at/

## Notes: All user settings are set below and used by the child scripts.
## Settings not set here fall back to codes/settings_defaults.R

## V1 of this script, if you find bugs, feel free to fix them and send
## a pull request or email me via wogerer@iiasa.ac.at


### from main directory call:
### Rscript codes/main_GLOBIOM_to_AgMIP.R path/to/my_file.gdx path/to/output_dir [SCEN1 SCEN2 ...]



rm(list=ls())

###### USER SETTINGS
##############################################################################################################################

# --- CLI arguments: ARG1 = GDX file, ARG2 = output dir, ARG3+ = scenario filter ---
args <- commandArgs(trailingOnly = TRUE)

GLOBIOM.file <- ifelse(length(args) >= 1, args[1], "inputs/output_30_merged.gdx")
out_dir      <- ifelse(length(args) >= 2, args[2], "./outputs")
if (length(args) >= 3) {
  scen.filter <- args[3:length(args)]
} else {
  scen.filter <- c() # empty = all scenarios, e.g. c("CP", "FP")
}

# --- Labels ---
rename.model     <- "GLOBIOM"        # "Model" column of the output
accelerator.name <- "GLOBIOM"        # file name tag
output.suffix    <- "comprehensive"  # <date>_<accelerator.name>_<output.suffix>.csv

# Scenario dimensions composing the scenario name, joined with "-"
# c("SCEN3") -> "CP" (LAMASUS); c("SCEN2", "SCEN3") -> "SSP2-CP"
scen.dims <- c("SCEN3")

# --- Scenario handling (LAMASUS) ---
# CP is the reference; single measures are CP_<FP|EA><measure> on top of CP
# Renamed in the reporting only (small GLOBIOM naming errors, not worth a rerun)
scen.rename <- c()
# Scenarios missing from the GDX are filled as a copy of scen.fill.from in all
# modules; WDM/BSR are forest-only measures, their forest data comes from the
# forest inputs (Forest-GLOBIOM RCP4p5_* runs mapped to these names)
scen.fill.from <- "CP"
scen.fill <- c("CP", "FP", "EA",
               "CP_FPFRS", "CP_EAFRS",
               "CP_FPSOC", "CP_EASOC",
               "CP_FPPTL", "CP_EAPTL",
               "CP_FPCAP", "CP_EACAP",
               "CP_FPPST", "CP_EAPST",
               "CP_FPFRT", "CP_EAFRT",
               "CP_FPSWF", "CP_EASWF",
               "CP_FPORG", "CP_EAORG",
               "CP_FPWDM", "CP_EAWDM",
               "CP_FPBSR", "CP_EABSR")

# --- Filters (applied to the full output, all modules) ---
years_keep <- seq(2000L, 2100L, by = 10L)
soc_items  <- c("SOC", "SOCE", "SOCC", "SOCG", "SOCB")
exclude_items <- c(
  "Meat", "ALL", "CER", "SRP", "PULP", "MEAL", "BIOM", "LUC", "LUCF", "LUCC", "LUCR", "LUCR2",
  "LUCG", "LUCE", "LUCS", "PEAT", "IP_Biomass", "EW_Biomass",
  soc_items # soil carbon flux, not yet in the AgMIP template
)
exclude_regions   <- c("ROW", "CHE")
exclude_variables <- c()

# --- Exogenous module inputs ---
costs.file         <- "open_input/LAMASUS_costs_country_20260420.csv"
country.map.file   <- "inputs/a6_LAMASUS_baseline.gdx"
predicts.file      <- "open_input/PREDICTS_update_modified.csv"
affor.factors.file <- "open_input/affor_emis_factors.rds"
forest.hwp.file    <- "open_input/processed_forest_HWP_emis_CP.rds"
forest.other.file  <- "open_input/processed_forest_CP.rds"
forest.year.max    <- 2050L

##############################################################################################################################
##### DONE WITH SETTINGS

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(GLOBIOM.file)) stop("GDX file not found: ", GLOBIOM.file)

source("codes/prep_GLOBIOM_agmip_accelerator_comprehensive.R")
