# ==============================================================================
# DEFAULT SETTINGS + SHARED HELPERS
# Sourced by the prep script. Settings are meant to be set in the main script
# (codes/main_GLOBIOM_to_AgMIP.R); anything already defined there is kept,
# only missing settings fall back to the defaults below.
# ==============================================================================

set_default <- function(name, value) {
  if (!exists(name, envir = globalenv())) assign(name, value, envir = globalenv())
}

# --- Input / output ---
set_default("GLOBIOM.file", "inputs/acc_pointer.gdx")
set_default("out_dir", "./outputs")
set_default("accelerator.name", "GLOBIOM")
set_default("output.suffix", "comprehensive")

# --- Labels ---
set_default("rename.model", "GLOBIOM")
# scenario dimensions composing the reported scenario name, joined with "-"
# e.g. c("SCEN3") -> "CP", c("SCEN2", "SCEN3") -> "SSP2-CP"
set_default("scen.dims", c("SCEN3"))

# --- Scenario handling ---
set_default("scen.filter", c())       # keep only these scenarios (empty = all)
set_default("scen.rename", c())       # named vector old = "new"
set_default("scen.fill", c())         # scenarios to create as copy of scen.fill.from if missing
set_default("scen.fill.from", "CP")

# --- Filters (applied to the full output, all modules) ---
set_default("years_keep", seq(2000L, 2100L, by = 10L))
set_default("soc_items", c("SOC", "SOCE", "SOCC", "SOCG", "SOCB"))
set_default("exclude_items", c(
  "Meat", "ALL", "CER", "SRP", "PULP", "MEAL", "BIOM", "LUC", "LUCF", "LUCC", "LUCR", "LUCR2",
  "LUCG", "LUCE", "LUCS", "PEAT", "IP_Biomass", "EW_Biomass",
  soc_items # soil carbon flux, not yet in the AgMIP template
))
set_default("exclude_regions", c("ROW", "CHE"))
set_default("exclude_variables", c())

# --- Exogenous module inputs ---
set_default("costs.file", "open_input/LAMASUS_costs_country_20260420.csv")
set_default("country.map.file", "inputs/a6_LAMASUS_baseline.gdx") # only needed by BII_calc.R
set_default("predicts.file", "open_input/PREDICTS_update_modified.csv")
set_default("affor.factors.file", "open_input/affor_emis_factors.rds")
set_default("forest.hwp.file", "open_input/processed_forest_HWP_emis_CP.rds")
set_default("forest.other.file", "open_input/processed_forest_CP.rds")
set_default("forest.year.max", 2050L)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# "SCEN3" / "ALLSCEN3" -> "ALLSCEN3"
scen.dims.cols <- paste0("ALL", sub("^ALL", "", toupper(scen.dims)))
if (!all(scen.dims.cols %in% c("ALLSCEN1", "ALLSCEN2", "ALLSCEN3"))) {
  stop("scen.dims must be a subset of c(\"SCEN1\", \"SCEN2\", \"SCEN3\"), got: ",
       paste(scen.dims, collapse = ", "))
}

# Overwrites ALLSCEN3 with the scenario name composed from scen.dims, so all
# downstream code keyed on ALLSCEN3 picks up the reported scenario name.
# Also fills missing scenarios (scen.fill), so every module that reads the GDX
# carries the filled scenarios, not only the core OUTPUT_AG block.
compose_scenario <- function(d) {
  d$ALLSCEN3 <- do.call(paste, c(lapply(scen.dims.cols, function(x) as.character(d[[x]])), sep = "-"))
  fill_scenarios(d)
}

# Scenarios in scen.fill that are missing in the GDX (e.g. forest-only measures
# like CP_FPWDM) are created as a copy of scen.fill.from. Scenarios that only
# appear after scen.rename count as present, so they are not filled as well.
fill_scenarios <- function(d) {
  present <- unique(d$ALLSCEN3)
  present <- c(present, unname(scen.rename[names(scen.rename) %in% present]))
  missing.scens <- setdiff(scen.fill, present)
  if (length(missing.scens) == 0) return(d)
  if (!(scen.fill.from %in% present)) stop("scen.fill.from '", scen.fill.from, "' not found in the data")
  message("Filling ", length(missing.scens), " missing scenario(s) as copy of ", scen.fill.from, ": ",
          paste(missing.scens, collapse = ", "))
  d <- as.data.table(d)
  src <- d[ALLSCEN3 == scen.fill.from]
  rbindlist(c(list(d), lapply(missing.scens, \(s) copy(src)[, ALLSCEN3 := s])))
}

# Exogenous forest inputs (Forest-GLOBIOM, already mapped to the reported
# scenario names): keep only reported scenarios, warn about missing ones
align_forest_scen <- function(d, label) {
  d <- as.data.table(d)
  d[, Scenario := as.character(Scenario)]
  dropped <- setdiff(unique(d$Scenario), report.scens)
  missing <- setdiff(report.scens, unique(d$Scenario))
  if (length(dropped) > 0) message(label, ": dropping ", length(dropped), " scenario(s) not in the reporting: ",
                                   paste(dropped, collapse = ", "))
  if (length(missing) > 0) warning(label, ": no data for ", length(missing), " reported scenario(s): ",
                                   paste(missing, collapse = ", "))
  d[Scenario %in% report.scens]
}

# Rename scenarios according to scen.rename (data.table with a Scenario column)
apply_scen_rename <- function(d) {
  if (length(scen.rename) > 0) d[Scenario %in% names(scen.rename), Scenario := scen.rename[Scenario]]
  d
}

# Harmonise a module result to the reporting layout before binding
as_report_dt <- function(x) {
  x <- as.data.table(x)
  chr_cols <- intersect(c("Model", "Scenario", "Region", "Item", "Variable", "Unit"), names(x))
  x[, (chr_cols) := lapply(.SD, as.character), .SDcols = chr_cols]
  if ("Year" %in% names(x)) x[, Year := as.integer(as.character(Year))]
  x[]
}
