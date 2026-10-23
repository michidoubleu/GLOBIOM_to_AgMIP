#!/usr/bin/env Rscript
# -------------------------
# GDX -> AgMIP CSV exporter
# Reads OUTPUT_AG (core variables) and OUTPUT (economic indicators, via
# Econ_calc.R) from GLOBIOM.file and writes output.file.
# Settings (GLOBIOM.file, output.file) come from codes/main_GLOBIOM_to_AgMIP.R
# -------------------------

suppressWarnings({
  library(tidyverse) # str_trim + dplyr/tidyr used by Econ_calc.R
  library(data.table) # main heavy lifting
  library(gamstransfer)
})

# -------------------------
# Fixed reporting conventions
# -------------------------
model.name <- "GLOBIOM"
years_keep <- seq(2000L, 2100L, by = 10L)
soc_items  <- c("SOC", "SOCE", "SOCC", "SOCG", "SOCB")
exclude_items <- c(
  "Meat", "ALL", "CER", "SRP", "PULP", "MEAL", "BIOM", "LUC", "LUCF", "LUCC", "LUCR", "LUCR2",
  "LUCG", "LUCE", "LUCS", "PEAT", "IP_Biomass", "EW_Biomass",
  soc_items # soil carbon flux, not yet in the AgMIP template
)
exclude_regions <- c("ROW", "CHE")

# Exogenous cost data for the economic indicators (country level, EU)
costs.file <- "open_input/LAMASUS_costs_country_20260420.csv"

# Scenario name = the 3 scenario dimensions of the GDX joined with "-",
# written to ALLSCEN3 so all code keyed on ALLSCEN3 uses the reported name
compose_scenario <- function(d) {
  d$ALLSCEN3 <- paste(as.character(d$ALLSCEN1), as.character(d$ALLSCEN2),
                      as.character(d$ALLSCEN3), sep = "-")
  d
}

# -------------------------
# Read GDX
# -------------------------
output_ag <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "OUTPUT_AG"),
  error = function(e) stop("Failed to read OUTPUT_AG: ", e$message)
)

output_ag <- output_ag$OUTPUT_AG$records
setDT(output_ag)

new_names <- c(
  "ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
  "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year", "Value"
)

# Detect if first 9 columns are unlabeled uni_*
if (all(grepl("^uni_", names(output_ag)[1:9]))) {
  setnames(output_ag, old = names(output_ag), new = new_names)
} else {
  message("Column names already labeled — skipped renaming.")
}

# -------------------------
# Standardize columns dynamically
# -------------------------
rename_map <- list(
  ScenYear = "Year",
  value    = "Value"
)
for (old in names(rename_map)) {
  if (old %in% names(output_ag)) setnames(output_ag, old, rename_map[[old]])
}

output_ag[, ALLRUN := NULL][]

# Convert factor columns to character
fact_cols <- names(which(sapply(output_ag, is.factor)))
output_ag[, (fact_cols) := lapply(.SD, as.character), .SDcols = fact_cols]

# Trim whitespace in character columns
char_cols <- names(which(sapply(output_ag, is.character)))
output_ag[, (char_cols) := lapply(.SD, str_trim), .SDcols = char_cols]

output_ag <- compose_scenario(output_ag)

# Standardize region names
output_ag[, ANYREGION := fifelse(ANYREGION == "World", "WLD", ANYREGION)]

# Convert Year and Value
output_ag[, Year := as.integer(Year)]
output_ag[, Value := as.numeric(Value)]

# -------------------------
# Lookup maps
# -------------------------
varid_dm_map <- c(
  YILD = "YILD_dm", YIRF = "YIRF_dm", YIIR = "YIIR_dm", YEXO = "YEXO_dm",
  food = "FOOD_dm", Feed = "FEED_dm", OTHU = "OTHU_dm", IMPO = "IMPO_dm",
  EXPO = "EXPO_dm", Prod = "PROD_dm", CONS = "CONS_dm", NETT = "NETT_dm"
)

unit_map_dm <- c("1000 t dm" = "1000 t", "dm t/ha" = "t/ha")
unit_map_other <- c(
  "Mln pers" = "Million", "Bn USD 2005" = "bn USD 2005 MER", "USD 2000 per ton" = "USD/t",
  "1000 ha" = "1000 ha", "t/ha" = "t/ha", "1000 t" = "1000 t", "km3" = "km3",
  "kcal/cap/d" = "kcal/cap/d", "Mt CO2eq/yr" = "MtCO2e", "USD/tCO2e" = "USD/tCO2e",
  "kg protein/ha" = "kg prt/ha"
)
unit_map_emis <- c("Mt CH4/yr" = "ktCH4", "Mt N2O/yr" = "ktN2O")

dm_vars <- names(varid_dm_map)
# FRTN/FRTP are built in Section F only
other_vars <- c(
  "POPT", "GDPT", "Area", "ARRF", "ARIR", "YILD", "YIRF", "YIIR", "YEXO",
  "Feed", "OTHU", "IMPO", "EXPO", "WATR", "CALO", "CALI", "Prod", "CONS",
  "NETT", "EMIS", "ECH4", "EN2O", "ECO2", "CTAX", "NBAL",
  "PBAL", "LYLD", "LYXO", "YEXO_I", "YEXO_R", "XPRP", "XPRX", "ABII"
)
emis_vars <- c("CH4", "N2O")

common_cols <- c(
  "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
  "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year", "Value"
)

# -------------------------
# Section A: DM-series
# -------------------------
Output_AG_DM <- output_ag[VAR_ID %in% dm_vars & VAR_UNIT %in% c("1000 t dm", "dm t/ha"), ..common_cols]
Output_AG_DM[, VAR_ID := ifelse(VAR_ID %in% names(varid_dm_map), varid_dm_map[VAR_ID], VAR_ID)]
Output_AG_DM[, VAR_UNIT := ifelse(VAR_UNIT %in% names(unit_map_dm), unit_map_dm[VAR_UNIT], VAR_UNIT)]

# -------------------------
# Section B: Other variables
# -------------------------
OUTPUT_AG_Oth <- output_ag[VAR_ID %in% other_vars & VAR_UNIT %in% names(unit_map_other), ..common_cols]
OUTPUT_AG_Oth[, VAR_ID := fifelse(
  VAR_ID == "ECH4", "ECH4_eq",
  fifelse(VAR_ID == "EN2O", "EN2O_eq", VAR_ID)
)]
OUTPUT_AG_Oth[, VAR_UNIT := unit_map_other[VAR_UNIT]]

# -------------------------
# Section C: Emissions
# -------------------------
OUTPUT_AG_Emis <- output_ag[VAR_ID %in% emis_vars, ..common_cols]
OUTPUT_AG_Emis[, VAR_ID := fifelse(
  VAR_ID == "CH4", "ECH4",
  fifelse(VAR_ID == "N2O", "EN2O", VAR_ID)
)]
OUTPUT_AG_Emis[, VAR_UNIT := unit_map_emis[VAR_UNIT]]
OUTPUT_AG_Emis[, Value := Value * 1000] # Mt -> kt

# -------------------------
# Section D: Protein / Nutrition
# -------------------------
Output_Ag_Proto <- output_ag[VAR_ID == "NUTR" & VAR_UNIT == "gprot/cap/d", ..common_cols]
Output_Ag_Proto[, `:=`(VAR_ID = "PROTO", VAR_UNIT = "gprt/cap/day")]

Output_Ag_Proti <- output_ag[VAR_ID == "NTRI" & VAR_UNIT == "gprot/cap/d", ..common_cols]
Output_Ag_Proti[, `:=`(VAR_ID = "PROTI", VAR_UNIT = "gprt/cap/day")]

# -------------------------
# Section E: Land categories
# -------------------------
land_noncrop_items <- c("1.2.2.PlantationEnerCrp", "3.Forest", "2.Grassland", "5.OthLand", "4.OthNatLand", "0.TotLand")
Output_Ag_Land_noncrop <- output_ag[VAR_ID == "LAND2" & ITEM_AG %in% land_noncrop_items, ..common_cols]
Output_Ag_Land_noncrop[, VAR_ID := "LAND"]
land_noncrop_recode <- c(
  "0.TotLand" = "TOT", "3.Forest" = "FOR", "1.2.2.PlantationEnerCrp" = "ECP",
  "2.Grassland" = "GRS", "5.OthLand" = "NLD", "4.OthNatLand" = "ONV"
)
Output_Ag_Land_noncrop[, ITEM_AG := land_noncrop_recode[ITEM_AG]]

crop_items <- c("1.1.AnnualCrp", "1.2.1.PlantationFoodCrp", "1.3.OthCrpLnd")
Output_Ag_Land_CRP <- output_ag[VAR_ID == "LAND2" & ITEM_AG %in% crop_items, ..common_cols]
Output_Ag_Land_CRP[, `:=`(VAR_ID = "LAND", ITEM_AG = "CRP")]
Output_Ag_Land_CRP <- Output_Ag_Land_CRP[, .(Value = sum(Value)), by = .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

Output_Ag_Land <- rbindlist(list(Output_Ag_Land_noncrop, Output_Ag_Land_CRP), use.names = TRUE, fill = TRUE)

# AGR = CRP + GRS
Output_Ag_Land_AGR <- Output_Ag_Land[
  ITEM_AG %in% c("CRP", "GRS"),
  .(Value = sum(Value)),
  by = .(VAR_ID, VAR_UNIT, ANYREGION, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)
]
Output_Ag_Land_AGR[, ITEM_AG := "AGR"]

Output_Ag_Land <- rbindlist(list(Output_Ag_Land, Output_Ag_Land_AGR), use.names = TRUE, fill = TRUE)

# -------------------------
# Section F: FOOD and Fertiliser
# -------------------------
Output_Ag_FOOD <- output_ag[VAR_ID %in% c("food", "WAST") & VAR_UNIT == "1000 t", ..common_cols]
Output_Ag_FOOD[, VAR_ID := "FOOD"]
Output_Ag_FOOD <- Output_Ag_FOOD[, .(Value = sum(Value)), by = .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

# Fertiliser: inorganic + organic 5-letter codes (FRTIN+FRTON, FRTIP+FRTOP)
# where they exist in the GDX, otherwise the 4-letter total (FRTN, FRTP).
# Decided per item: the split is only reported for some items (e.g. TOT), the
# crop-level items (AGR, CRP, WHT, ...) only come as FRTN/FRTP.
process_fert <- function(split_ids, total_id) {
  fert_by <- c("VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year")
  # a missing split component is a zero (GAMS does not write zeros to the GDX)
  split <- output_ag[VAR_ID %in% split_ids & VAR_UNIT == "1000 t", ..common_cols]
  split[, VAR_ID := total_id]
  split <- split[, .(Value = sum(Value)), by = fert_by]

  total <- output_ag[VAR_ID == total_id & VAR_UNIT == "1000 t", ..common_cols]
  total <- total[!split, on = setdiff(fert_by, "VAR_ID")]

  rbindlist(list(split, total), use.names = TRUE)
}
Output_Ag_FRTN <- process_fert(c("FRTIN", "FRTON"), "FRTN")
Output_Ag_FRTP <- process_fert(c("FRTIP", "FRTOP"), "FRTP")

# -------------------------
# Combine all tables
# -------------------------
OUTPUT_AG_t <- rbindlist(list(
  Output_AG_DM, OUTPUT_AG_Oth, OUTPUT_AG_Emis,
  Output_Ag_Proto, Output_Ag_Proti,
  Output_Ag_FOOD, Output_Ag_Land, Output_Ag_FRTP, Output_Ag_FRTN
), use.names = TRUE, fill = TRUE)

dt_out <- OUTPUT_AG_t[, .(
  Model    = model.name,
  Scenario = ALLSCEN3,
  Region   = ANYREGION,
  Item     = ITEM_AG,
  Variable = VAR_ID,
  Year     = Year,
  Unit     = VAR_UNIT,
  Value    = Value
)]

# SOC reporting -- soil carbon flux items (soc_items) are currently dropped via
# exclude_items as they are not yet in the AgMIP template. To report them,
# remove soc_items from exclude_items above and uncomment below:
# EMIS and ECO2 are identical for these items (pure CO2 flux) -- keep only
# EMIS; CTAX is not reported.
# dt_out <- dt_out[!(Item %in% soc_items & Variable %in% c("ECO2", "CTAX"))]

# -------------------------
# Economic indicators (OUTPUT + exogenous costs)
# -------------------------
source("./codes/region_mapping.R")
source("./codes/Econ_calc.R")

Econ_tidy <- as.data.table(Econ_tidy)
Econ_tidy[, `:=`(Model = model.name, Scenario = as.character(Scenario),
                 Region = as.character(Region), Year = as.integer(Year))]
dt_out <- rbindlist(list(dt_out, Econ_tidy), use.names = TRUE, fill = TRUE)

# -------------------------
# Filters
# -------------------------
dt_out[Region == "World", Region := "WLD"]
dt_out <- dt_out[
  Year %in% years_keep &
    !(Item %in% exclude_items) &
    !(Region %in% exclude_regions)
]

n_dup <- sum(duplicated(dt_out, by = c("Model", "Scenario", "Region", "Item", "Variable", "Year", "Unit")))
if (n_dup > 0) warning(n_dup, " duplicated Model/Scenario/Region/Item/Variable/Year/Unit rows in output")

# -------------------------
# Export
# -------------------------
message("Writing ", uniqueN(dt_out$Scenario), " scenario(s), ", nrow(dt_out), " rows to ", output.file)
data.table::fwrite(dt_out, output.file)

message("Done.")
