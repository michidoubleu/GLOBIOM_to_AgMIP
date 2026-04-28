######## easy BBI calc post script

# --- 1. Load Required Libraries ---# Note: library() is generally preferred over require() in scripts because # it will throw a clear error immediately if the package is missing.library(dplyr)
library(tidyr)
library(data.table)

# --- 2. Load Data ---# Create an ID column for the crosswalk 
# mappingpredicts_map <- read.csv("../PREDICTS-IIASA/PREDICTS_update_crosswalk.csv", header = TRUE) %>% 
# mutate(ID = row_number())

predicts_data <- read.csv("./open_input/PREDICTS_update_modified.csv") %>% dplyr::select(LUM, Abundance)



### calc high and low input cropland
# -------------------------
# Read GDX
# -------------------------
globiom_areas <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "ACR_COMPARE"),
  error = function(e) stop("Failed to read ACR_COMPARE: ", e$message)
)

globiom_areas <- globiom_areas$ACR_COMPARE$records
setDT(globiom_areas)
new_names <- c("ALLRUN", "region_c", "LUID", "shit1","shit2","shit3","shit4","shit5",
               "mgmt","ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")
# Apply column names
setnames(globiom_areas, new_names)

# Recode mgmt
globiom_areas[, mgmt := ifelse(mgmt %in% c("LI", "SS"), "LI", "HI")]

# Aggregate after recoding
globiom_agg <- globiom_areas[
  , .(Value = sum(Value, na.rm = TRUE)),
  by = .(region_c, ALLSCEN1, ALLSCEN2, ALLSCEN3, mgmt, Year)
]

# Optional ordering
setorder(globiom_agg, region_c, ALLSCEN1, ALLSCEN2, ALLSCEN3, mgmt, Year)


region.map <- readGDX(GLOBIOM.file, symbols = "REGION_AG_MAP")
region.map <- region.map$REGION_AG_MAP$records
setDT(region.map)
region.map <- unique(
  region.map[, .(region = uni_2,
                 region_61 = uni_3)]
)

country.map <- readGDX("inputs/a6_LAMASUS_baseline.gdx", symbols = "REGION61_COUNTRY_MAP")
country.map <- country.map$REGION61_COUNTRY_MAP$records
setDT(country.map)
country.map <- unique(
  country.map[, .(region_61 = ANYREGION,
                 region_c = ALLCOUNTRY)]
)

mapping <- merge(
  region.map,
  country.map,
  by = "region_61",
  allow.cartesian=TRUE
)

globiom_mapped <- merge(
  globiom_agg,
  mapping,
  by = "region_c",
  all.x = TRUE,
  ,
  allow.cartesian=TRUE
)

globiom_new_regions <- globiom_mapped[
  , .(Value = sum(Value, na.rm = TRUE)),
  by = .(region,
         ALLSCEN1,
         ALLSCEN2,
         ALLSCEN3,
         mgmt,
         Year)
]



output_land <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "OUTPUT_AG"),
  error = function(e) stop("Failed to read OUTPUT_AG: ", e$message)
)

output_land <- output_land$OUTPUT_AG$records
setDT(output_land)

new_names <- c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
               "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")

# Detect if first 9 columns are unlabeled uni_*
if (all(grepl("^uni_", names(output_land)[1:9]))) {
  setnames(output_land, old = names(output_land), new = new_names)
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
  if (old %in% names(output_land)) setnames(output_land, old, rename_map[[old]])
}

output_land[, ALLRUN := NULL][]

# Convert factor columns to character
fact_cols <- names(which(sapply(output_land, is.factor)))
output_land[, (fact_cols) := lapply(.SD, as.character), .SDcols = fact_cols]
output_land <- output_land[VAR_ID == "LAND"]

# load organic, hedgerow and extensive grassland data
organic_data = all_mitigtech_summary %>% 
  filter(ITEM_AG == "AREA_org",ITEM != "TOT")

hedgerow_data = all_mitigtech_summary %>% 
  filter(ITEM_AG == "AREA_lft",ITEM != "TOT")

grassland_data = all_mitigtech_summary %>% 
  filter(ITEM_AG == "AREA_aem")

# 1. Filter external datasets for 'AGR' and aggregate area
org_agr <- organic_data[ITEM == "AGR", .(organic_area = sum(Value, na.rm = TRUE)), 
                        by = .(ANYREGION, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

# hedgerow_data lacks ALLSCEN1 and ALLSCEN2, so merge relies on ANYREGION, ALLSCEN3, and Year
hrv_agr <- hedgerow_data[ITEM == "CRP", .(hedgerow_area = sum(Value, na.rm = TRUE)), 
                         by = .(ANYREGION, ALLSCEN3, Year)]

# Filter output_land for Cropland rows
cropland_rows <- output_land[ITEM_AG == "Cropland"]

# Filter globiom_new_regions for Cropland management (HI & LI)
# Assuming globiom_new_regions already only contains cropland data for mgmt
# If not, filter by a cropland identifier if needed

# Merge cropland rows with globiom_new_regions by scenario, region, year
# Make sure column names match for merging
cropland_merged <- merge(
  cropland_rows,
  globiom_new_regions,
  by.x = c("ANYREGION", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year"),
  by.y = c("region", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year"),
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Update ITEM_AG to indicate management type
cropland_merged[, ITEM_AG := paste0("Cropland_", mgmt)]

# Replace the Value column with the Value from globiom_new_regions
cropland_merged[, Value := Value.y]

crop_wide <- dcast(cropland_merged, 
                   ANYREGION + ALLSCEN1 + ALLSCEN2 + ALLSCEN3 + Year + VAR_ID + VAR_UNIT ~ mgmt, 
                   value.var = "Value", 
                   fill = 0)

# 3. Merge datasets
crop_wide <- merge(crop_wide, org_agr, by = c("ANYREGION", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year"), all.x = TRUE)
crop_wide <- merge(crop_wide, hrv_agr, by = c("ANYREGION", "ALLSCEN3", "Year"), all.x = TRUE)

# Null replacement for regions/years missing organic or hedgerow data
crop_wide[is.na(organic_area), organic_area := 0]
crop_wide[is.na(hedgerow_area), hedgerow_area := 0]

# 4. Sequential area allocation prioritizing High Input (HI)
crop_wide[, `:=`(
  # Hedgerow deduction
  deduct_hrv_HI = pmin(HI, hedgerow_area),
  deduct_hrv_LI = pmax(0, hedgerow_area - HI)
)]

crop_wide[, `:=`(
  temp_HI = HI - deduct_hrv_HI,
  # Floor at 0 to strictly prevent negatives if required hedgerow > (HI + LI)
  temp_LI = pmax(0, LI - deduct_hrv_LI) 
)]

crop_wide[, `:=`(
  # Organic deduction
  organic_HI = pmin(temp_HI, organic_area),
  # Cap at available temp_LI to prevent negatives
  organic_LI = pmin(temp_LI, pmax(0, organic_area - temp_HI)) 
)]

crop_wide[, `:=`(
  # Calculate final baseline categories
  Cropland_HI = temp_HI - organic_HI,
  Cropland_LI = temp_LI - organic_LI,
  Cropland_hedgerow = hedgerow_area
)]

# 5. Reshape to long format and re-standardize classifications
cropland_updated <- melt(crop_wide,
                         id.vars = c("ANYREGION", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year", "VAR_ID", "VAR_UNIT"),
                         measure.vars = c("Cropland_HI", "Cropland_LI", "organic_HI", "organic_LI", "Cropland_hedgerow"),
                         variable.name = "ITEM_AG",
                         value.name = "Value")

# Keep only relevant columns matching output_land
cropland_final <- cropland_updated[, .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year, Value)]

# Remove original Cropland rows from output_land
output_land <- output_land[ITEM_AG != "Cropland"]

# Add the new Cropland_HI and Cropland_LI rows
output_land <- rbind(output_land, cropland_final)

##### Grassland ########

# 1. Filter output_land for Grassland rows
grassland_rows <- output_land[ITEM_AG == "Grassland"]

# 2. Filter organic and hedgerow data for Grassland (ITEM == "GRS")
org_grs <- organic_data[ITEM == "LSP", .(organic_area = sum(Value, na.rm = TRUE)), 
                        by = .(ANYREGION, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

hrv_grs <- hedgerow_data[ITEM == "GRS", .(hedgerow_area = sum(Value, na.rm = TRUE)), 
                         by = .(ANYREGION, ALLSCEN3, Year)]

ext_grs <- grassland_data[ITEM == "GRS", .(extensive_area = sum(Value, na.rm = TRUE)), 
                          by = .(ANYREGION, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

# 3. Merge datasets
grass_merged <- merge(grassland_rows, org_grs, 
                      by = c("ANYREGION", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year"), all.x = TRUE)
grass_merged <- merge(grass_merged, hrv_grs, 
                      by = c("ANYREGION", "ALLSCEN3", "Year"), all.x = TRUE)
grass_merged <- merge(grass_merged, ext_grs, 
                      by = c("ANYREGION", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year"), all.x = TRUE)

# Null replacement for missing regional/temporal data
grass_merged[is.na(organic_area), organic_area := 0]
grass_merged[is.na(hedgerow_area), hedgerow_area := 0]
grass_merged[is.na(extensive_area), extensive_area := 0]

# 4. Sequential area allocation
grass_merged[, `:=`(
  # 1st order: Hedgerow deduction
  Grassland_hedgerow = pmin(Value, hedgerow_area)
)]

grass_merged[, `:=`(
  temp_avail_1 = pmax(0, Value - Grassland_hedgerow)
)]

grass_merged[, `:=`(
  # 2nd order: Organic deduction
  Grassland_organic = pmin(temp_avail_1, organic_area)
)]

grass_merged[, `:=`(
  temp_avail_2 = pmax(0, temp_avail_1 - Grassland_organic)
)]

grass_merged[, `:=`(
  # 3rd order: Extensive deduction
  Grassland_extensive = pmin(temp_avail_2, extensive_area),
  # Residual allocation to Intensive/Base grassland
  Grassland_intensive = pmax(0, temp_avail_2 - pmin(temp_avail_2, extensive_area)) 
)]

# 5. Reshape to long format
grassland_updated <- melt(grass_merged,
                          id.vars = c("ANYREGION", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year", "VAR_ID", "VAR_UNIT"),
                          measure.vars = c("Grassland_hedgerow", "Grassland_organic", "Grassland_extensive", "Grassland_intensive"),
                          variable.name = "ITEM_AG",
                          value.name = "Value")

# 6. Standardize columns and update output_land
grassland_final <- grassland_updated[, .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year, Value)]

output_land <- output_land[ITEM_AG != "Grassland"]
output_land <- rbind(output_land, grassland_final)

for_mng = dt_updated %>% 
  filter(Variable == "AREA", 
         Item %in% c("FOR|PRO", "FOR|LOC", "FOR|LON", "FOR|MIC",
                     "FOR|MIN","FOR|HIC","FOR|HIN","FOR|LRC","FOR|LRN",
                     "FOR|RUC","FOR|RUN"))

#### Forest #####

# 
# # 1. Isolate the base Forest vectors
# forest_rows <- output_land[ITEM_AG == "Forest"]
# 
# # Harmonize column names and types for a clean merge
# for_mng_clean <- copy(for_mng)
# for_mng_clean[, Year := as.character(Year)]
# for_mng_clean[, Scenario := as.character(Scenario)]
# setnames(for_mng_clean, 
#          old = c("Region", "Scenario"), 
#          new = c("ANYREGION", "ALLSCEN3"))
# 
# # 2. Pivot management classes to wide format
# # This ensures every management class (FOR|HIC, FOR|HIN, etc.) becomes a distinct column
# mng_wide <- dcast(for_mng_clean, 
#                   ANYREGION + ALLSCEN3 + Year ~ Item, 
#                   value.var = "Value", 
#                   fill = 0)
# 
# # 3. Calculate total managed forest row-wise
# mng_cols <- setdiff(names(mng_wide), c("ANYREGION", "ALLSCEN3", "Year"))
# mng_wide[, managed_sum := rowSums(.SD, na.rm = TRUE), .SDcols = mng_cols]
# 
# # 4. Merge wide management data with base forest structural rows
# forest_merged <- merge(forest_rows, 
#                        mng_wide, 
#                        by = c("ANYREGION", "ALLSCEN3", "Year"), 
#                        all.x = TRUE)
# 
# # Replace NAs with 0 for regions/years in output_land that lack for_mng data
# for (col in c(mng_cols, "managed_sum")) {
#   forest_merged[is.na(get(col)), (col) := 0]
# }
# 
# # 5. Calculate Unmanaged Forest residual
# # Floor at 0 to explicitly prevent negative residuals if managed > baseline
# forest_merged[, Forest_unmanaged := pmax(0, Value - managed_sum)]
# 
# # 6. Reshape to long format 
# # This gathers the unmanaged residual AND all specific management classes
# measure_vars <- c("Forest_unmanaged", mng_cols)
# 
# forest_updated <- melt(forest_merged,
#                        id.vars = c("ANYREGION", "ALLSCEN1", "ALLSCEN2", "ALLSCEN3", "Year", "VAR_ID", "VAR_UNIT"),
#                        measure.vars = measure_vars,
#                        variable.name = "ITEM_AG",
#                        value.name = "Value")
# 
# # Ensure ITEM_AG is character (melt sometimes coerces to factor)
# forest_updated[, ITEM_AG := as.character(ITEM_AG)]
# 
# # 7. Standardize schema and append to base table
# forest_final <- forest_updated[, .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year, Value)]
# 
# output_land <- output_land[ITEM_AG != "Forest"]
# output_land <- rbind(output_land, forest_final)


setDT(predicts_data)

# STEP 1: Map ITEM_AG to LUM (choose the best matching class)
# You will need to define a mapping. Example mapping:
# Convert lum_map to a named character vector
lum_map_vec <- c(
  "Forest_unmanaged" = "Close-to-nature management",
  "Forest" = "Close-to-nature management",
  "FOR|HIC" = "Very intensive forestry",
  "FOR|HIN" = "Very intensive forestry",
  "FOR|MIN" = "Intensive forestry",
  "FOR|MIC" = "Intensive forestry",
  "FOR|LON" = "Combined objective forestry",
  "FOR|LOC" = "Combined objective forestry",
  "FOR|PRO" = "Primary forest",
  "Plantation" = "Intensive forestry",
  "Grassland_intensive" = "Moderate density pasture/grassland",
  "Grassland_extensive" = "Low density pasture/grassland",
  "Grassland_organic" = "Low density pasture/grassland",
  "OthNatVeg" = "Unmanaged natural grassland",
  "OthAgri" = "Intensive permanent cropland",
  "Cropland_HI" = "Intensive cropland",
  "Cropland_LI" = "Extensive cropland",
  "Cropland_hedgerow" = "Agroforestry",
  "Grassland_hedgerow" = "Agroforestry",
  "organic_HI" = "Extensive cropland",
  "organic_LI" = "Extensive cropland",
  "OthLand" = "Other urban"
)

# Map ITEM_AG to LUM correctly
output_land[, LUM := lum_map_vec[ITEM_AG]]

# Now merge works
output_land <- merge(
  output_land, 
  predicts_data[, .(LUM, Abundance)], 
  by = "LUM", 
  all.x = TRUE
)

# STEP 3: Compute weighted BII
output_land[, weighted_BII := Value * Abundance]

# STEP 4: Aggregate for each region × scenario × year
BII_summary <- output_land[, .(
  BII = sum(weighted_BII, na.rm = TRUE) / sum(Value, na.rm = TRUE)
), by = .(ANYREGION, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

BII_tidy <- BII_summary[, .(
  Model = "GLOBIOM",
  Scenario = ALLSCEN3,       # or ALLSCEN1/ALLSCEN2 if you prefer
  Region = ANYREGION,
  Item = "TOT",
  Variable = "BII",
  Year = as.integer(Year),
  Unit = "Index",
  Value = BII
)]

# Optional: order by Region and Year
setorder(BII_tidy, Region, Year)

# Convert to tibble if you want
BII_tidy <- as_tibble(BII_tidy)

saveRDS(BII_tidy, "./open_input/processed_BII.rds")
