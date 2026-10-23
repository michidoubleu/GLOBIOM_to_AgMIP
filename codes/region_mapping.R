######## Region mapping
# region_map: GLOBIOM 61 regions (region_61) -> reporting regions (region),
#             from REGION_AG_MAP in GLOBIOM.file. Used by Econ_calc.R.

library(data.table)

region_map <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "REGION_AG_MAP"),
  error = function(e) stop("Failed to read REGION_AG_MAP: ", e$message)
)$REGION_AG_MAP$records
setDT(region_map)
region_map <- unique(region_map[, .(region    = as.character(uni_2),
                                    region_61 = as.character(uni_3))])
