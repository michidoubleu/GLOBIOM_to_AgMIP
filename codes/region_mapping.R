######## Region mapping
# region_map:         GLOBIOM 61 regions (region_61) -> reporting regions (region),
#                     from REGION_AG_MAP in GLOBIOM.file. Used by Econ_calc.R.
# country_region_map: country (region_c) -> region_61 -> region, additionally
#                     using REGION61_COUNTRY_MAP from country.map.file. Only
#                     built when country.map.file is set (needed by BII_calc.R).

library(data.table)

region_map <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "REGION_AG_MAP"),
  error = function(e) stop("Failed to read REGION_AG_MAP: ", e$message)
)$REGION_AG_MAP$records
setDT(region_map)
region_map <- unique(region_map[, .(region    = as.character(uni_2),
                                    region_61 = as.character(uni_3))])

if (length(country.map.file) > 0 && nzchar(country.map.file)) {
  country_map <- tryCatch(
    readGDX(country.map.file, symbols = "REGION61_COUNTRY_MAP"),
    error = function(e) stop("Failed to read REGION61_COUNTRY_MAP: ", e$message)
  )$REGION61_COUNTRY_MAP$records
  setDT(country_map)
  country_map <- unique(country_map[, .(region_61 = as.character(ANYREGION),
                                        region_c  = as.character(ALLCOUNTRY))])

  country_region_map <- merge(region_map, country_map, by = "region_61", allow.cartesian = TRUE)
}
