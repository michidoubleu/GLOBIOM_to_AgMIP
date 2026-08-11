######## easy mitigation tech calc post script

# --- 1. Load Required Libraries ---# Note: library() is generally preferred over require() in scripts because # it will throw a clear error immediately if the package is missing.library(dplyr)
library(tidyr)
library(data.table)


output_reg_land <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "OUTPUT_AG_REG"),
  error = function(e) stop("Failed to read OUTPUT_AG_REG: ", e$message)
)

output_reg_land <- output_reg_land$OUTPUT_AG$records
setDT(output_reg_land)

new_names <- c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
               "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")

# Detect if first 9 columns are unlabeled uni_*
if (all(grepl("^uni_", names(output_reg_land)[1:9]))) {
  setnames(output_reg_land, old = names(output_reg_land), new = new_names)
} else {
  message("Column names already labeled — skipped renaming.")
}

mitigtech_summary = output_reg_land %>%
  filter(ITEM_AG %in% c("Silvopasture","GrsMgmt_SOC","CrpMgmt_SOC","Biochar_apl",
                        "NonProd_Element","Organic_crp","Organic_grs","ResOrg_Total"),
         VAR_UNIT == "1000 ha") %>%
  mutate(
    # Organic_crp/Organic_grs are the only source items that still carry a
    # cropland ("AGR") vs grassland ("LSP") split; BII_calc.R needs this to net
    # organic area out of the Cropland/Grassland HI-LI allocation. Everything
    # else (incl. NonProd_Element/hedgerows, which lost their crop/grass split
    # when the external CSV join was replaced) is only available as a total.
    ITEM = case_match(ITEM_AG,
      "Organic_crp" ~ "AGR",
      "Organic_grs" ~ "LSP",
      .default = "TOT"
    ),
    ITEM_AG = case_match(ITEM_AG,
      "Silvopasture"     ~ "AREA_agrofor",
      "CrpMgmt_SOC"      ~ "AREA_til",
      "GrsMgmt_SOC"      ~ "AREA_aem",
      "Biochar_apl"      ~ "AREA_biochar",
      "NonProd_Element"  ~ "AREA_lft",
      "Organic_crp"      ~ "AREA_org",
      "Organic_grs"      ~ "AREA_org",
      "ResOrg_Total"     ~ "AREA_wetland"
    )
  ) %>%
  as.data.table()

# Sum over all grouping columns (keeps the AGR/LSP split intact for internal use)
mitigtech_summary <- mitigtech_summary[
  ,
  .(Value = sum(Value, na.rm = TRUE)),
  by = setdiff(names(mitigtech_summary), "Value")
]


# #### Add organic scenarios ####
# organic_scen= read.csv("./open_input/CAP_SP_organic_plans.csv") 
# 
# 
# 
# output_land <- tryCatch(
#   readGDX(GLOBIOM.file, symbols = "OUTPUT_AG"),
#   error = function(e) stop("Failed to read OUTPUT_AG: ", e$message)
# )
# 
# output_land <- output_land$OUTPUT_AG$records
# setDT(output_land)
# 
# new_names <- c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
#                "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")
# 
# # Detect if first 9 columns are unlabeled uni_*
# if (all(grepl("^uni_", names(output_land)[1:9]))) {
#   setnames(output_land, old = names(output_land), new = new_names)
# } else {
#   message("Column names already labeled — skipped renaming.")
# }
# 
# organic_summary = output_land %>%
#   filter(ITEM_AG %in% c("AGR","CRP","LSP"),
#          VAR_ID == "Area") %>%
#   mutate(Year = as.character(Year)) %>%
#   mutate(SCEN3 = ifelse(!ALLSCEN3 %in% c("FP","EA","Base_FPORG","Base_EAORG"),"BASE",as.character(ALLSCEN3))) %>%
#   left_join(organic_scen %>% 
#               mutate(Year = as.character(Year)) %>%
#               rename(org_share = Value)) %>%
#   filter(!is.na(org_share)) %>%
#   mutate(Value = Value * org_share) %>%
#   select(-org_share) %>%
#   rename(ITEM = ITEM_AG) %>%
#   mutate(ITEM_AG = "AREA_org")
# 
# # Add EUE
# organic_summary = organic_summary %>% 
#   bind_rows(organic_summary %>%
#     group_by(VAR_ID,VAR_UNIT,ITEM,ALLSCEN1,ALLSCEN2,ALLSCEN3,Year,SCEN3,ITEM_AG) %>%
#     summarise(Value = sum(Value,na.rm=TRUE)) %>%
#     ungroup() %>%
#     mutate(ANYREGION = "EUE"))
# 
# #### Add hedgerows scenarios ####
# hedgerow_scen= read.csv("./open_input/CLU_hedgerow_area_FPEA_20260427.csv") 
# 
# scenario_map = data.frame(ALLSCEN3 = unique(mitigtech_summary$ALLSCEN3)) %>%
#   mutate(ANYSCEN = case_when(
#     ALLSCEN3 %in% c("EASWF", "Base_EASWF") ~ "easwf",
#     ALLSCEN3 %in% c("FPSWF", "Base_FPSWF") ~ "fpswf",
#     TRUE                                   ~ "base"
#   ))
# 
# hedgerow_summary <- scenario_map %>%
#   left_join(hedgerow_scen, by = c("ANYSCEN" = "ANYSCEN"), relationship = "many-to-many") %>%
#   select(ALLSCEN3, everything(), -ANYSCEN) %>%
#   mutate(ALLYEAR = as.character(ALLYEAR)) %>%
#   rename(Year = ALLYEAR) %>%
#   mutate(VARIABLE = "Area",
#          ITEM_AG = "AREA_lft") %>%
#   rename(VAR_UNIT = UNIT)
# 
# # Add EUE
# hedgerow_summary = hedgerow_summary %>% 
#   bind_rows(hedgerow_summary %>%
#               group_by(VAR_UNIT,ALLSCEN3,Year,ITEM_AG,ITEM,VARIABLE) %>%
#               summarise(Value = sum(Value,na.rm=TRUE)) %>%
#               ungroup() %>%
#               mutate(ANYREGION = "EUE"))

#### Final tidying and save ####
all_mitigtech_summary = mitigtech_summary %>%
  as.data.table()

# Reporting always goes out at the "TOT" item level (per the AgMIP template);
# the AGR/LSP split above is kept only in all_mitigtech_summary for BII_calc.R
mitigtech_export <- all_mitigtech_summary[
  ,
  .(Value = sum(Value, na.rm = TRUE)),
  by = setdiff(names(all_mitigtech_summary), c("Value", "ITEM"))
]

mitigtech_tidy <- mitigtech_export[, .(
  Model = "GLOBIOM",
  Scenario = ALLSCEN3,       # or ALLSCEN1/ALLSCEN2 if you prefer
  Region = ANYREGION,
  Item = "TOT",
  Variable = ITEM_AG,
  Year = as.integer(as.character(Year)),
  Unit = VAR_UNIT,
  Value = Value
)]

# Optional: order by Region and Year
setorder(mitigtech_tidy, Region, Year)

# Convert to tibble if you want
mitigtech_tidy <- as_tibble(mitigtech_tidy)

saveRDS(mitigtech_tidy, "./open_input/processed_mitigtech.rds")
