# ==============================================================================
# DATA PROCESSING ENGINE
# ==============================================================================

suppressWarnings({
  library(data.table)
  library(gamstransfer)
})

# 1. Read GDX
output_ag <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "OUTPUT_AG"),
  error = function(e) stop("Failed to read OUTPUT_AG: ", e$message)
)$OUTPUT_AG$records
setDT(output_ag)

# 2. Dynamic Renaming & Cleaning
if (all(grepl("^uni_", names(output_ag)[1:9]))) {
  setnames(output_ag, old = names(output_ag), new = new_names)
}

for (old in names(rename_map)) {
  if (old %in% names(output_ag)) setnames(output_ag, old, rename_map[[old]])
}

if ("ALLRUN" %in% names(output_ag)) output_ag[, ALLRUN := NULL]

# Clean factors and strings
fact_cols <- names(which(sapply(output_ag, is.factor)))
output_ag[, (fact_cols) := lapply(.SD, as.character), .SDcols = fact_cols]
char_cols <- names(which(sapply(output_ag, is.character)))
output_ag[, (char_cols) := lapply(.SD, trimws), .SDcols = char_cols]

# Standardize Columns
if ("ANYREGION" %in% names(output_ag)) output_ag[, ANYREGION := fifelse(ANYREGION == "World", "WLD", ANYREGION)]
if ("Year" %in% names(output_ag))  output_ag[, Year := as.integer(Year)]
if ("Value" %in% names(output_ag)) output_ag[, Value := as.numeric(Value)]

common_cols <- intersect(c("VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG", "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value"), names(output_ag))

# 3. Apply Transformations
# Section A: DM
Output_AG_DM <- output_ag[VAR_ID %in% dm_vars & VAR_UNIT %in% c("1000 t dm", "dm t/ha"), ..common_cols]
Output_AG_DM[, `:=`(VAR_ID = varid_dm_map[VAR_ID], VAR_UNIT = unit_map_dm[VAR_UNIT])]

# Section B: Other
OUTPUT_AG_Oth <- output_ag[VAR_ID %in% other_vars & VAR_UNIT %in% names(unit_map_other), ..common_cols]
OUTPUT_AG_Oth[, `:=`(VAR_ID = fcase(VAR_ID == "ECH4", "ECH4_eq", VAR_ID == "EN2O", "EN2O_eq", default = VAR_ID), VAR_UNIT = unit_map_other[VAR_UNIT])]

# Section C: Emissions
OUTPUT_AG_Emis <- output_ag[VAR_ID %in% emis_vars, ..common_cols]
OUTPUT_AG_Emis[, `:=`(VAR_ID = fcase(VAR_ID == "CH4", "ECH4", VAR_ID == "N2O", "EN2O"), VAR_UNIT = unit_map_emis[VAR_UNIT], Value = Value / 1000)]

# Section D & E: Nutrition & Land
Output_Ag_Proto <- output_ag[VAR_ID=="NUTR" & VAR_UNIT=="gprot/cap/d", ..common_cols][, `:=`(VAR_ID="PROTO", VAR_UNIT="gprt/cap/day")]
Output_Ag_Proti <- output_ag[VAR_ID=="NTRI" & VAR_UNIT=="gprot/cap/d", ..common_cols][, `:=`(VAR_ID="PROTI", VAR_UNIT="gprt/cap/day")]

Output_Ag_Land_noncrop <- output_ag[VAR_ID=="LAND2" & ITEM_AG %in% land_noncrop_items, ..common_cols]
Output_Ag_Land_noncrop[, `:=`(VAR_ID = "LAND", ITEM_AG = land_noncrop_recode[ITEM_AG])]

Output_Ag_Land_CRP <- output_ag[VAR_ID=="LAND2" & ITEM_AG %in% crop_items, ..common_cols][, `:=`(VAR_ID="LAND", ITEM_AG="CRP")]
Output_Ag_Land_CRP <- Output_Ag_Land_CRP[, .(Value=sum(Value)), by=setdiff(names(Output_Ag_Land_CRP), "Value")]
Output_Ag_Land <- rbindlist(list(Output_Ag_Land_noncrop, Output_Ag_Land_CRP), use.names=TRUE)

# Section F: Food & Fertilizer
Output_Ag_FOOD <- output_ag[VAR_ID %in% c("food","WAST") & VAR_UNIT=="1000 t", ..common_cols]
Output_Ag_FOOD <- Output_Ag_FOOD[, .(Value=sum(Value)), by=.(VAR_ID="FOOD", VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]

process_fert <- function(dt, ids, new_id) {
  res <- dt[VAR_ID %in% ids & VAR_UNIT=="1000 t", ..common_cols]
  if(nrow(res) > 0) res <- res[, .(Value=sum(Value)), by=.(VAR_ID=new_id, VAR_UNIT, ANYREGION, ITEM_AG, ALLSCEN1, ALLSCEN2, ALLSCEN3, Year)]
  return(res)
}
Output_Ag_FRTN <- process_fert(output_ag, c("FRTIN","FRTON"), "FRTN")
Output_Ag_FRTP <- process_fert(output_ag, c("FRTIP","FRTOP"), "FRTP")

# 4. Final Merge & Export
OUTPUT_AG_t <- rbindlist(list(Output_AG_DM, OUTPUT_AG_Oth, OUTPUT_AG_Emis, Output_Ag_Proto, Output_Ag_Proti, Output_Ag_FOOD, Output_Ag_Land, Output_Ag_FRTP, Output_Ag_FRTN), use.names=TRUE, fill=TRUE)
OUTPUT_AG_t[, Scen := paste(ALLSCEN1, ALLSCEN2, ALLSCEN3, sep="-")]

OUTPUT_AG_ACCELERATOR <- OUTPUT_AG_t[, .(Model=rename.model, Scenario=Scen, Region=ANYREGION, Item=ITEM_AG, Variable=VAR_ID, Year=Year, Unit=VAR_UNIT, Value=Value)]

if(length(scen.filter)!=0) OUTPUT_AG_ACCELERATOR <- OUTPUT_AG_ACCELERATOR[Scenario %in% scen.filter]

dt_sub <- OUTPUT_AG_ACCELERATOR[Year %in% years_keep & !(Item %in% exclude_items) & !(Region %in% exclude_regions) & !(Variable %in% exclude_variables)]

date.tag <- gsub("-","",Sys.Date())
fname <- file.path(out_dir, paste0(date.tag,"_",accelerator.name,"_AgMIP.csv"))
data.table::fwrite(dt_sub, fname)

message("Process complete. File saved: ", fname)