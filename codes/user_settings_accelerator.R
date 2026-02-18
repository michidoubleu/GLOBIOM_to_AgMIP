# ==============================================================================
# USER SETTINGS & CONFIGURATION
# ==============================================================================

# --- CLI Arguments & Paths ---
args <- commandArgs(trailingOnly = TRUE)

# Default input file if no ARG1 provided
GLOBIOM.file <- ifelse(length(args) >= 1, args[1], "inputs/acc_pointer.gdx")

# Default output dir if no ARG2 provided
out_dir <- ifelse(length(args) >= 2, args[2], "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  
# Scenario Filtering (ARG3+)
if (length(args) >= 3) {
  scen.filter <- args[3:length(args)]
} else {
  scen.filter <- c()
}

# --- Metadata ---
rename.model     <- "GLOBIOM"
accelerator.name <- "GLOBIOM"

# --- Mappings ---
new_names  <- c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
                "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")

rename_map <- list(ScenYear = "Year", value = "Value")

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

# --- Filters ---
years_keep    <- seq(2000L, 2100L, by=10L)
exclude_items <- c("Meat","ALL","CER","SRP","PULP","MEAL","BIOM","LUC","LUCF","LUCC",
                   "LUCR","LUCR2","LUCG","LUCE","LUCS","SOC","SOCE","SOCC","SOCG",
                   "SOCB","PEAT", "IP_Biomass", "EW_Biomass")
exclude_regions   <- c("ROW","CHE")
exclude_variables <- c()

# --- Internal Variable Lists ---
dm_vars    <- names(varid_dm_map)
emis_vars  <- c("CH4","N2O")
other_vars <- c("POPT","GDPT","Area","ARRF","ARIR","YILD","YIRF","YIIR","YEXO",
                "Feed","OTHU","IMPO","EXPO","WATR","CALO","CALI","Prod","CONS",
                "NETT","EMIS","ECH4","EN2O", "ECO2","CTAX","NBAL","FRTIN","FRTON","FRTIP",
                "FRTOP","PBAL","LYLD","LYXO","YEXO_I","YEXO_R","XPRP","XPRX", "ABII")

land_noncrop_items  <- c("1.2.2.PlantationEnerCrp","3.Forest","2.Grassland","5.OthLand","4.OthNatLand","0.TotLand")
land_noncrop_recode <- c("0.TotLand"="TOT","3.Forest"="FOR","1.2.2.PlantationEnerCrp"="ECP",
                         "2.Grassland"="GRS","5.OthLand"="NLD","4.OthNatLand"="ONV")
crop_items          <- c("1.1.AnnualCrp","1.2.1.PlantationFoodCrp","1.3.OthCrpLnd")