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

# Keep only relevant columns matching output_land
cropland_final <- cropland_merged[, .(VAR_ID, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year, Value)]

# Remove original Cropland rows from output_land
output_land <- output_land[ITEM_AG != "Cropland"]

# Add the new Cropland_HI and Cropland_LI rows
output_land <- rbind(output_land, cropland_final)

setDT(predicts_data)

# STEP 1: Map ITEM_AG to LUM (choose the best matching class)
# You will need to define a mapping. Example mapping:
# Convert lum_map to a named character vector
lum_map_vec <- c(
  "Forest" = "Close-to-nature management",
  "Plantation" = "Intensive forestry",
  "Grassland" = "Moderate density pasture/grassland",
  "OthNatVeg" = "Unmanaged natural grassland",
  "OthAgri" = "Intensive permanent cropland",
  "Cropland_HI" = "Intensive cropland",
  "Cropland_LI" = "Extensive cropland",
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
