#!/usr/bin/env Rscript
# -------------------------
# Cleaned + dynamic GDX -> Accelerator CSV exporter
# Fully adapts to the domain columns in the GDX file
# Hybrid tidyverse/data.table approach: data.table for speed
# Settings come from codes/main_GLOBIOM_to_AgMIP.R (defaults: codes/settings_defaults.R)
# -------------------------

suppressWarnings({
  library(tidyverse) # str_trim + dplyr/tidyr used by the sourced modules
  library(data.table) # main heavy lifting
  library(gamstransfer)
})

source("codes/settings_defaults.R")

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
# Map domain columns to Accelerator expectations
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

# Scenario name from the dimensions chosen in scen.dims
output_ag <- compose_scenario(output_ag)

# Standardize region names
if ("ANYREGION" %in% names(output_ag)) {
  output_ag[, ANYREGION := fifelse(ANYREGION == "World", "WLD", ANYREGION)]
}

# Convert Year and Value
if ("Year" %in% names(output_ag)) output_ag[, Year := as.integer(Year)]
if ("Value" %in% names(output_ag)) output_ag[, Value := as.numeric(Value)]

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
# FRTN/FRTP are built in Section F only (listing them here duplicated them)
other_vars <- c(
  "POPT", "GDPT", "Area", "ARRF", "ARIR", "YILD", "YIRF", "YIIR", "YEXO",
  "Feed", "OTHU", "IMPO", "EXPO", "WATR", "CALO", "CALI", "Prod", "CONS",
  "NETT", "EMIS", "ECH4", "EN2O", "ECO2", "CTAX", "NBAL",
  "PBAL", "LYLD", "LYXO", "YEXO_I", "YEXO_R", "XPRP", "XPRX", "ABII"
)
emis_vars <- c("CH4", "N2O")

# -------------------------
# Helper: dynamic column selector
# -------------------------
common_cols <- intersect(
  c(
    "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
    "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year", "Value"
  ),
  names(output_ag)
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
OUTPUT_AG_Emis[, Value := Value / 1000] # Mt -> kt

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

# 1. Create a copy of only the Forest rows
forest_area <- copy(Output_Ag_Land_noncrop[ITEM_AG == "FOR"])
# 2. Change the VAR_ID to AREA
forest_area[, VAR_ID := "AREA"]
# 3. Append it back to the original table
Output_Ag_Land_noncrop <- rbind(Output_Ag_Land_noncrop, forest_area)

crop_items <- c("1.1.AnnualCrp", "1.2.1.PlantationFoodCrp", "1.3.OthCrpLnd")
Output_Ag_Land_CRP <- output_ag[VAR_ID == "LAND2" & ITEM_AG %in% crop_items, ..common_cols]
Output_Ag_Land_CRP[, `:=`(VAR_ID = "LAND", ITEM_AG = "CRP")]
Output_Ag_Land_CRP <- Output_Ag_Land_CRP[, .(Value = sum(Value)), by = .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

Output_Ag_Land <- rbindlist(list(Output_Ag_Land_noncrop, Output_Ag_Land_CRP), use.names = TRUE, fill = TRUE)

# Duplicate CRP/GRS totals into VAR_ID=AREA too (same pattern as the Forest
# copy above), so the intensive/extensive area split added later reconciles
# against AREA/CRP and AREA/GRS directly instead of a different Variable (LAND).
crp_grs_area <- copy(Output_Ag_Land[ITEM_AG %in% c("CRP", "GRS")])
crp_grs_area[, VAR_ID := "AREA"]
Output_Ag_Land <- rbind(Output_Ag_Land, crp_grs_area)

# Create AGR = CRP + GRS
Output_Ag_Land_AGR <- Output_Ag_Land[
  ITEM_AG %in% c("CRP", "GRS"),
  .(Value = sum(Value)),
  by = .(VAR_ID, VAR_UNIT, ANYREGION, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)
]

Output_Ag_Land_AGR[, ITEM_AG := "AGR"]


# Append to main table
Output_Ag_Land <- rbindlist(
  list(Output_Ag_Land, Output_Ag_Land_AGR),
  use.names = TRUE,
  fill = TRUE
)
# -------------------------
# Section F: FOOD and Fertiliser
# -------------------------
Output_Ag_FOOD <- output_ag[VAR_ID %in% c("food", "WAST") & VAR_UNIT == "1000 t", ..common_cols]
Output_Ag_FOOD[, VAR_ID := "FOOD"]
Output_Ag_FOOD <- Output_Ag_FOOD[, .(Value = sum(Value)), by = .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

# Fertiliser: inorganic + organic 5-letter codes (FRTIN+FRTON, FRTIP+FRTOP)
# if both exist in the GDX, otherwise the 4-letter total (FRTN, FRTP)
process_fert <- function(split_ids, total_id) {
  if (all(split_ids %in% output_ag$VAR_ID)) {
    res <- output_ag[VAR_ID %in% split_ids & VAR_UNIT == "1000 t", ..common_cols]
    res[, VAR_ID := total_id]
    res[, .(Value = sum(Value)), by = .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]
  } else {
    message(total_id, ": ", paste(split_ids, collapse = "/"), " not found, using ", total_id)
    output_ag[VAR_ID == total_id & VAR_UNIT == "1000 t", ..common_cols]
  }
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

# Final column ordering
dt_sub <- OUTPUT_AG_t[, .(
  Model = "GLOBIOM",
  Scenario = ALLSCEN3,
  Region = ANYREGION,
  Item = ITEM_AG,
  Variable = VAR_ID,
  Year = Year,
  Unit = VAR_UNIT,
  Value = Value
)]

# SOC reporting -- soil carbon flux items (soc_items) are currently dropped via
# exclude_items as they are not yet in the AgMIP template. To report them,
# remove soc_items from exclude_items in the main script and uncomment below:
# EMIS and ECO2 are identical for the soil-carbon items (pure CO2 flux, no
# CH4/N2O) -- keep only EMIS to avoid reporting the same flux twice. CTAX
# (shadow carbon price for these items) is not reported.
# dt_sub <- dt_sub[!(Item %in% soc_items & Variable %in% c("ECO2", "CTAX"))]


##### scenario renaming + fill of missing scenarios (settings: scen.rename, scen.fill)
# renaming first, so a renamed scenario is not also filled from scen.fill.from
dt_sub <- apply_scen_rename(dt_sub)

missing.scens <- setdiff(scen.fill, dt_sub$Scenario)
if (length(missing.scens) > 0) {
  if (!(scen.fill.from %in% dt_sub$Scenario)) stop("scen.fill.from '", scen.fill.from, "' not found in the data")
  message("Filling ", length(missing.scens), " missing scenario(s) as copy of ", scen.fill.from, ": ",
          paste(missing.scens, collapse = ", "))
  dt_sub <- rbindlist(c(
    list(dt_sub),
    lapply(missing.scens, \(s) copy(dt_sub[Scenario == scen.fill.from])[, Scenario := s])
  ))
}



#### add afforetation emissions
source("codes/integrate_affor_emissions.R")
dt_forest_2000 <- for.res %>%
  filter(Year == 2010) %>%
  mutate(Year = 2000, Value = 0)
for.res <- bind_rows(for.res, dt_forest_2000) %>% filter(Year <= forest.year.max)

dt_main <- as_report_dt(dt_sub)
dt_forest <- as_report_dt(for.res)
dt_combined <- rbindlist(list(dt_main, dt_forest), use.names = TRUE, fill = TRUE)



#### add HWP emissions
for.res.emis <- readRDS(forest.hwp.file)
dt_forest_2000 <- for.res.emis %>%
  filter(Year == 2010) %>%
  mutate(Year = 2000)
for.res.emis <- bind_rows(for.res.emis, dt_forest_2000) %>% filter(Year <= forest.year.max)

dt_forest <- as_report_dt(for.res.emis)
dt_combined <- rbindlist(list(dt_combined, dt_forest), use.names = TRUE, fill = TRUE)

# afforestation sinks come from output_ag, so still carry the original names
dt_combined <- apply_scen_rename(dt_combined)
dt_updated <- dt_combined[, .(Value = sum(Value, na.rm = TRUE)),
  by = .(Model, Scenario, Region, Item, Variable, Year, Unit)
]


#### add forest other
for.res.other <- readRDS(forest.other.file)

for.res.other <- for.res.other %>% filter(Year <= forest.year.max)

dt_for.res.other <- as_report_dt(for.res.other)
dt_updated <- rbindlist(list(dt_updated, dt_for.res.other), use.names = TRUE, fill = TRUE)

#### add mitigation technologies
source("./codes/mitigtech_calc.R")

dt_updated <- rbindlist(list(dt_updated, as_report_dt(mitigtech_tidy)), use.names = TRUE, fill = TRUE)

#### add crop/grassland management-intensity area split
# All sourced from mitigtech_calc.R's all_mitigtech_summary (ITEM_AG):
#   AREA_til     = CropMgmt_SOC   -> CRP|EXT
#   AREA_biochar = Biochar_apl    -> used only for the CRP mutual-exclusivity
#                                     check below; not exported as CRP|BCH
#                                     (already reported under AREA_biochar/TOT)
#   AREA_aem     = GrsMgmt_SOC    -> GRS|EXT
#   AREA_agrofor = Silvopasture (excl. Silvopasture_IP) -> subtracted out of
#                                     GRS|INT's remainder but not exported as
#                                     GRS|SVP (already reported under
#                                     AREA_agrofor/TOT)
#
# CRP: EXT + INT = AREA/CRP (unchanged 2-way split).
#
# GRS: AREA/GRS (from LAND2/"2.Grassland") covers ALL grassland, including
# grassland not used for feeding animals at all (no input) -- see
# codes/GRS_LSP_area_land_trace.txt. So GRS|EXT/GRS|INT are based on
# Area/LSP ("livestock/pasture", the grassland actually used for animals),
# duplicated here into AREA/LSP for a same-Variable reconciliation total.
# GRS|EXT + GRS|INT does NOT sum to AREA/LSP -- the silvopasture portion is
# subtracted out of the remainder but reported separately via AREA_agrofor.
#
# Region label is harmonised ("World" -> "WLD") since mitigtech_calc.R reads
# OUTPUT_AG_REG directly and doesn't get the WLD recode applied to output_ag above.
mgmt_source <- all_mitigtech_summary[
  ITEM_AG %in% c("AREA_til", "AREA_biochar", "AREA_aem", "AREA_agrofor"),
  .(area = sum(Value, na.rm = TRUE)),
  by = .(ANYREGION, ALLSCEN3, ITEM_AG, Year)
]
mgmt_source[, `:=`(
  Region   = fifelse(ANYREGION == "World", "WLD", as.character(ANYREGION)),
  Scenario = as.character(ALLSCEN3),
  Year     = as.integer(as.character(Year))
)]
mgmt_source <- apply_scen_rename(mgmt_source)

crp_ext <- mgmt_source[ITEM_AG == "AREA_til",     .(Region, Scenario, Year, crp_ext = area)]
crp_bch <- mgmt_source[ITEM_AG == "AREA_biochar", .(Region, Scenario, Year, crp_bch = area)]
grs_ext <- mgmt_source[ITEM_AG == "AREA_aem",     .(Region, Scenario, Year, grs_ext = area)]
grs_svp <- mgmt_source[ITEM_AG == "AREA_agrofor", .(Region, Scenario, Year, grs_svp = area)]

total_lsp <- dt_updated[
  Variable == "Area" & Item == "LSP",
  .(Model, Scenario, Region, Year, Unit, total_area = Value)
]

# expose LSP under Variable=AREA too, matching the CRP/GRS/FOR convention,
# so GRS|EXT+GRS|SVP+GRS|INT has a same-Variable total to reconcile against
dt_updated <- rbindlist(list(
  dt_updated,
  total_lsp[, .(Model, Scenario, Region, Item = "LSP", Variable = "AREA", Year, Unit, Value = total_area)]
), use.names = TRUE, fill = TRUE)

## ---- CRP: EXT/INT (unchanged 2-way) + separate, non-exclusive BCH ----
total_crp <- dt_updated[
  Variable == "AREA" & Item == "CRP",
  .(Model, Scenario, Region, Year, Unit, total_area = Value)
]
crp_joined <- merge(total_crp, crp_ext, by = c("Region", "Scenario", "Year"), all.x = TRUE)
crp_joined <- merge(crp_joined, crp_bch, by = c("Region", "Scenario", "Year"), all.x = TRUE)
crp_joined[is.na(crp_ext), crp_ext := 0]
crp_joined[is.na(crp_bch), crp_bch := 0]

n_neg_crp_with_bch <- sum((crp_joined$total_area - crp_joined$crp_ext - crp_joined$crp_bch) < 0)
message(
  "CRP intensity split: ", n_neg_crp_with_bch, " of ", nrow(crp_joined),
  " region/scenario/year rows would go negative if BCH were also subtracted ",
  "from AREA/CRP alongside EXT. Zero negatives does NOT prove EXT/BCH are ",
  "non-overlapping (biochar area may just be small relative to AREA/CRP even ",
  "if applied on the same land) -- absent a data source that can confirm ",
  "mutual exclusivity, BCH is kept as a separate, non-subtracted item rather ",
  "than assumed exclusive (EXT+INT still sums to AREA/CRP)."
)

# BCH is not exported as its own AREA item -- it's already reported under
# AREA_biochar/TOT. crp_bch is only used above for the mutual-exclusivity check.
crp_tidy <- rbindlist(list(
  crp_joined[, .(Model, Scenario, Region, Item = "CRP|EXT", Variable = "AREA",
                 Year, Unit, Value = crp_ext)],
  crp_joined[, .(Model, Scenario, Region, Item = "CRP|INT", Variable = "AREA",
                 Year, Unit, Value = pmax(0, total_area - crp_ext))]
))

## ---- GRS: 3-way mutually-exclusive EXT/SVP/INT, based on AREA/LSP ----
grs_joined <- merge(total_lsp, grs_ext, by = c("Region", "Scenario", "Year"), all.x = TRUE)
grs_joined <- merge(grs_joined, grs_svp, by = c("Region", "Scenario", "Year"), all.x = TRUE)
grs_joined[is.na(grs_ext), grs_ext := 0]
grs_joined[is.na(grs_svp), grs_svp := 0]
grs_joined[, remainder := total_area - grs_ext - grs_svp]

n_neg_grs <- sum(grs_joined$remainder < 0)
message(
  "GRS intensity split: ", n_neg_grs, " of ", nrow(grs_joined),
  " region/scenario/year rows have AREA/LSP - GRS|EXT - GRS|SVP < 0 ",
  "(floored at 0 via pmax; GRS|EXT and GRS|SVP are otherwise assumed mutually exclusive)."
)

# SVP is not exported as its own AREA item -- it's already reported under
# AREA_agrofor/TOT. It's still subtracted out of GRS|INT's remainder below
# (not folded back in), so GRS|EXT + GRS|INT does NOT sum to AREA/LSP; the
# gap is the silvopasture portion, covered by AREA_agrofor instead.
grs_tidy <- rbindlist(list(
  grs_joined[, .(Model, Scenario, Region, Item = "GRS|EXT", Variable = "AREA",
                 Year, Unit, Value = grs_ext)],
  grs_joined[, .(Model, Scenario, Region, Item = "GRS|INT", Variable = "AREA",
                 Year, Unit, Value = pmax(0, remainder))]
))

dt_updated <- rbindlist(list(dt_updated, crp_tidy, grs_tidy), use.names = TRUE, fill = TRUE)

#### region mapping (region_map for Econ, country_region_map for BII)
source("./codes/region_mapping.R")

#### add BII
source("./codes/BII_calc.R")

dt_updated <- rbindlist(list(dt_updated, as_report_dt(BII_tidy)), use.names = TRUE, fill = TRUE)

#### add Economic Variables
source("./codes/Econ_calc.R")

dt_updated <- rbindlist(list(dt_updated, as_report_dt(Econ_tidy)), use.names = TRUE, fill = TRUE)

# -------------------------
# Final scenario renaming + filters (all modules)
# -------------------------
dt_updated <- apply_scen_rename(dt_updated)

dt_updated[Region == "World", Region := "WLD"]
dt_updated <- dt_updated[
  Year %in% years_keep &
    !(Item %in% exclude_items) &
    !(Region %in% exclude_regions) &
    !(Variable %in% exclude_variables)
]

if (length(scen.filter) != 0) {
  dt_updated <- dt_updated[Scenario %in% scen.filter]
}

dt_updated[, Model := rename.model]

n_dup <- sum(duplicated(dt_updated, by = c("Model", "Scenario", "Region", "Item", "Variable", "Year", "Unit")))
if (n_dup > 0) warning(n_dup, " duplicated Model/Scenario/Region/Item/Variable/Year/Unit rows in output")

# -------------------------
# Export
# -------------------------
date.tag <- gsub("-", "", Sys.Date())
fname <- file.path(out_dir, paste0(date.tag, "_", accelerator.name, "_", output.suffix, ".csv"))
message("Writing ", uniqueN(dt_updated$Scenario), " scenario(s) to ", fname)

data.table::fwrite(dt_updated, fname)

message("Done. Exported files are in: ", out_dir)
