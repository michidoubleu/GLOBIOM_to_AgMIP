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


europe_countries <- c(
  "AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP",
  "EST","FIN","FRA","GRC","HRV","HUN","IRL","ITA",
  "LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU",
  "SVK","SVN","SWE","CHE","GBR","NOR"
)
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
                "NETT","EMIS","ECH4","EN2O","ECO2","CTAX","NBAL","FRTIN","FRTON","FRTIP",
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
# Combine all tables
# -------------------------
OUTPUT_AG_t <- rbindlist(list(
  Output_AG_DM, OUTPUT_AG_Oth,
  Output_Ag_Proto,Output_Ag_Land,
), use.names=TRUE, fill=TRUE)

# Construct scenario string
OUTPUT_AG_t[, Scen := paste0(ALLSCEN3)]

YLD_shifter <- OUTPUT_AG_t %>% dplyr::select(-starts_with("ALL")) %>% 
  filter(VAR_ID=="YILD", ITEM_AG%in%c("CRP","Meat"), ANYREGION%in%europe_countries) %>% 
  pivot_wider(names_from = "Scen", values_from = "Value") %>%
  mutate(FP_SOC=Base_FPSOC/BASE, EA_SOC=Base_EASOC/BASE, FP_PTL=Base_FPWET/BASE, EA_PTL=Base_EAWET/BASE) %>%
  dplyr::select(ANYREGION, ITEM_AG, Year, FP_SOC, EA_SOC, FP_PTL, EA_PTL) %>%
  pivot_longer(cols = c("FP_SOC", "EA_SOC", "FP_PTL", "EA_PTL"), names_to = 'Scen', values_to = 'value') %>%
  mutate(ITEM_AG=recode(ITEM_AG, "Meat"="GRS"))

write.csv(YLD_shifter, file="Yieldshifter_SOC_PTL_GLOBIOM.csv", row.names = FALSE)
