
# Ensure the data is sorted by Year first
setorder(forest_area, ANYREGION, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)
# Calculate the 10-year increase

# 2. Calculate change relative to the first year (2000)
forest_area[, Affor := Value - first(Value), 
            by = .(VAR_ID, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3)]
# Optional: Replace the NA in the first year (2000) with 0 if desired
forest_area[is.na(Affor), Affor := 0]

conv.fac <- readRDS("open_input/affor_emis_factors.rds")
# 1. Ensure conv.fac is a data.table and align the Year type
setDT(conv.fac)
conv.fac[, Year := as.integer(as.character(year))]

# 2. Perform the join and multiplication
# We join forest_area with conv.fac
# Match ANYREGION to agmip_target, ITEM_AG to item, and Year to Year
forest_area[conv.fac, on = .(ANYREGION = agmip_target, ITEM_AG = item, Year), 
            `:=`(
              # Store the factor for reference (optional)
              applied_sink_fac = sink.fac,
              # Calculate the actual sink
              Sink_Value = Affor * sink.fac
            )]

# 3. Handle years/regions without a factor (like the Year 2000 anchor)
# This prevents NAs from breaking your plots later
forest_area[is.na(Sink_Value), Sink_Value := 0]

# 1. Prepare the base data with shared columns
base_data <- forest_area %>%
  filter(Year >= 2010) %>%
  transmute(
    Model    = "GLOBIOM",
    Scenario = ALLSCEN3,
    Region   = ANYREGION,
    Year     = as.integer(Year),
    Unit     = "MtCO2e",
    Value    = Sink_Value
  )

# 2. Create the duplication grid
# We define the unique variables and items we need to report
variables <- c("EMIS", "ECO2")
items     <- c("FOR", "TOT")

# 3. Generate the final dataframe by crossing the base data with our reporting levels
for.res <- map_dfr(items, function(i) {
  map_dfr(variables, function(v) {
    base_data %>%
      mutate(Item = i, Variable = v)
  })
}) %>%
  # Reorder columns to match your desired output
  select(Model, Scenario, Region, Item, Variable, Year, Unit, Value) %>%
  as_tibble()

