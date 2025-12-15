#!/usr/bin/env Rscript
# -------------------------
# Cleaned + dynamic GDX -> Accelerator CSV exporter
# Fully adapts to the domain columns in the GDX file
# Hybrid tidyverse/data.table approach: data.table for speed
# -------------------------

suppressWarnings({
  library(tidyverse)  # used mainly for str_trim
  library(data.table) # main heavy lifting
  library(unpivotr)   # keep in case used downstream
  library(gamstransfer)
})

# -------------------------
# Read GDX
# -------------------------
output_ag <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "OUTPUT_AG"),
  error = function(e) stop("Failed to read OUTPUT_AG: ", e$message)
)

output_ag <- output_ag$OUTPUT_AG$records
setDT(output_ag)

new_names <- c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
               "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")

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

if(length(scen.filter)!=0){
#filter scenarios
output_ag <- output_ag[ALLSCEN2 %in% scen.filter]
}

# Convert factor columns to character
fact_cols <- names(which(sapply(output_ag, is.factor)))
output_ag[, (fact_cols) := lapply(.SD, as.character), .SDcols = fact_cols]

# Trim whitespace in character columns
char_cols <- names(which(sapply(output_ag, is.character)))
output_ag[, (char_cols) := lapply(.SD, str_trim), .SDcols = char_cols]

# Standardize region names
if ("ANYREGION" %in% names(output_ag)) {
  output_ag[, ANYREGION := fifelse(ANYREGION == "World", "WLD", ANYREGION)]
}

# Convert Year and Value
if ("Year" %in% names(output_ag))  output_ag[, Year := as.integer(Year)]
if ("Value" %in% names(output_ag)) output_ag[, Value := as.numeric(Value)]

# -------------------------
# Lookup maps
# -------------------------
varid_dm_map <- c(
  YILD="YILD_dm", YIRF="YIRF_dm", YIIR="YIIR_dm", YEXO="YEXO_dm",
  food="FOOD_dm", Feed="FEED_dm", OTHU="OTHU_dm", IMPO="IMPO_dm",
  EXPO="EXPO_dm", Prod="PROD_dm", CONS="CONS_dm", NETT="NETT_dm"
)

unit_map_dm <- c("1000 t dm"="1000 t", "dm t/ha"="t/ha")
unit_map_other <- c(
  "Mln pers"="Million", "Bn USD 2005"="bn USD 2005 MER", "USD 2000 per ton"="USD/t",
  "1000 ha"="1000 ha", "t/ha"="t/ha", "1000 t"="1000 t", "km3"="km3",
  "kcal/cap/d"="kcal/cap/d", "Mt CO2eq/yr"="MtCO2e", "USD/tCO2e"="USD/tCO2e",
  "kg protein/ha"="kg prt/ha"
)
unit_map_emis <- c("Mt CH4/yr"="ktCH4", "Mt N2O/yr"="ktN2O")

dm_vars <- names(varid_dm_map)
other_vars <- c("POPT","GDPT","Area","ARRF","ARIR","YILD","YIRF","YIIR","YEXO",
                "Feed","OTHU","IMPO","EXPO","WATR","CALO","CALI","Prod","CONS",
                "NETT","EMIS","ECH4","EN2O","CTAX","NBAL","FRTIN","FRTON","FRTIP",
                "FRTOP","PBAL","LYLD","LYXO","YEXO_I","YEXO_R","XPRP","XPRX", "ABII")
emis_vars <- c("CH4","N2O")

# -------------------------
# Helper: dynamic column selector
# -------------------------
common_cols <- intersect(
  c("VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
    "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value"),
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
OUTPUT_AG_Oth[, VAR_ID := fifelse(VAR_ID == "ECH4", "ECH4_eq",
                                  fifelse(VAR_ID == "EN2O", "EN2O_eq", VAR_ID))]
OUTPUT_AG_Oth[, VAR_UNIT := unit_map_other[VAR_UNIT]]

# -------------------------
# Section C: Emissions
# -------------------------
OUTPUT_AG_Emis <- output_ag[VAR_ID %in% emis_vars, ..common_cols]
OUTPUT_AG_Emis[, VAR_ID := fifelse(VAR_ID == "CH4", "ECH4",
                                   fifelse(VAR_ID == "N2O", "EN2O", VAR_ID))]
OUTPUT_AG_Emis[, VAR_UNIT := unit_map_emis[VAR_UNIT]]
OUTPUT_AG_Emis[, Value := Value / 1000] # Mt -> kt

# -------------------------
# Section D: Protein / Nutrition
# -------------------------
Output_Ag_Proto <- output_ag[VAR_ID=="NUTR" & VAR_UNIT=="gprot/cap/d", ..common_cols]
Output_Ag_Proto[, `:=`(VAR_ID="PROTO", VAR_UNIT="gprt/cap/day")]

Output_Ag_Proti <- output_ag[VAR_ID=="NTRI" & VAR_UNIT=="gprot/cap/d", ..common_cols]
Output_Ag_Proti[, `:=`(VAR_ID="PROTI", VAR_UNIT="gprt/cap/day")]

# -------------------------
# Section E: Land categories
# -------------------------
land_noncrop_items <- c("1.2.2.PlantationEnerCrp","3.Forest","2.Grassland","5.OthLand","4.OthNatLand","0.TotLand")
Output_Ag_Land_noncrop <- output_ag[VAR_ID=="LAND2" & ITEM_AG %in% land_noncrop_items, ..common_cols]
Output_Ag_Land_noncrop[, VAR_ID := "LAND"]
land_noncrop_recode <- c("0.TotLand"="TOT","3.Forest"="FOR","1.2.2.PlantationEnerCrp"="ECP",
                         "2.Grassland"="GRS","5.OthLand"="NLD","4.OthNatLand"="ONV")
Output_Ag_Land_noncrop[, ITEM_AG := land_noncrop_recode[ITEM_AG]]

crop_items <- c("1.1.AnnualCrp","1.2.1.PlantationFoodCrp","1.3.OthCrpLnd")
Output_Ag_Land_CRP <- output_ag[VAR_ID=="LAND2" & ITEM_AG %in% crop_items, ..common_cols]
Output_Ag_Land_CRP[, `:=`(VAR_ID="LAND", ITEM_AG="CRP")]
Output_Ag_Land_CRP <- Output_Ag_Land_CRP[, .(Value=sum(Value)), by=.(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

Output_Ag_Land <- rbindlist(list(Output_Ag_Land_noncrop, Output_Ag_Land_CRP), use.names=TRUE, fill=TRUE)

# -------------------------
# Section F: FOOD and Fertiliser
# -------------------------
Output_Ag_FOOD <- output_ag[VAR_ID %in% c("food","WAST") & VAR_UNIT=="1000 t", ..common_cols]
Output_Ag_FOOD[, VAR_ID := "FOOD"]
Output_Ag_FOOD <- Output_Ag_FOOD[, .(Value=sum(Value)), by=.(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

Output_Ag_FRTN <- output_ag[VAR_ID %in% c("FRTIN","FRTON") & VAR_UNIT=="1000 t", ..common_cols]
if (nrow(Output_Ag_FRTN) > 0) {
  Output_Ag_FRTN[, VAR_ID := "FRTN"]
  Output_Ag_FRTN <- Output_Ag_FRTN[, .(Value=sum(Value)), by=.(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]
}

Output_Ag_FRTP <- output_ag[VAR_ID %in% c("FRTIP","FRTOP") & VAR_UNIT=="1000 t", ..common_cols]
if (nrow(Output_Ag_FRTP) > 0) {
  Output_Ag_FRTP[, VAR_ID := "FRTP"]
  Output_Ag_FRTP <- Output_Ag_FRTP[, .(Value=sum(Value)), by=.(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]
}

# -------------------------
# Combine all tables
# -------------------------
OUTPUT_AG_t <- rbindlist(list(
  Output_AG_DM, OUTPUT_AG_Oth, OUTPUT_AG_Emis,
  Output_Ag_Proto, Output_Ag_Proti,
  Output_Ag_FOOD, Output_Ag_Land, Output_Ag_FRTP, Output_Ag_FRTN
), use.names=TRUE, fill=TRUE)

# Construct scenario string
OUTPUT_AG_t[, Scen := paste(ALLSCEN1, ALLSCEN2, ALLSCEN3, sep="-")]

# Final column ordering
OUTPUT_AG_ACCELERATOR <- OUTPUT_AG_t[, .(
  Model="GLOBIOM",
  Scenario=Scen,
  Region=ANYREGION,
  Item=ITEM_AG,
  Variable=VAR_ID,
  Year=Year,
  Unit=VAR_UNIT,
  Value=Value
)]

# -------------------------
# Export per scenario
# -------------------------
years_keep <- seq(2000L, 2100L, by=10L)
exclude_items <- c("Meat","ALL","CER","SRP","PULP","MEAL","BIOM","LUC","LUCF","LUCC","LUCR","LUCR2",
                   "LUCG","LUCE","LUCS","SOC","SOCE","SOCC","SOCG","SOCB","PEAT", "IP_Biomass", "EW_Biomass")
exclude_regions <- c("ROW","CHE")
exclude_variables <- c()

date.tag <- Sys.Date()

setkey(OUTPUT_AG_ACCELERATOR, Scenario)
scenarios <- unique(OUTPUT_AG_ACCELERATOR$Scenario)
message("Writing ", length(scenarios), " scenario file(s) to ", out_dir)

dt_sub <- OUTPUT_AG_ACCELERATOR[Year %in% years_keep & !(Item %in% exclude_items) & !(Region %in% exclude_regions) & !(Variable %in% exclude_variables)]

fname <- file.path(out_dir, paste0(date.tag,"_",accelerator.name,"_AgMIP_accelerator.csv"))

if(!is.null(rename.model)){
  dt_sub$Model <- rename.model
}


data.table::fwrite(dt_sub, fname)

message("Done. Exported files are in: ", out_dir)
