library(data.table)
library(dplyr)
library(tidyr)
library(knitr)
library(gamstransfer)

GLOBIOM.file <- "inputs/output_149_merged.gdx"
source("codes/Econ_calc.R")

target_scen <- "BASE"
target_year <- 2020
target_crop <- "WHT"

# Filter final_tidy for WHT and summarize
wht_tbl <- final_tidy %>%
  filter(Item == target_crop, Scenario == target_scen, Year == target_year) %>%
  group_by(Region, Variable) %>%
  summarize(Value = sum(Value, na.rm = TRUE), .groups = 'drop')

if (nrow(wht_tbl) == 0) {
  cat("No data found for WHT in BASE 2020!\n")
} else {
  wide_tbl <- as.data.table(wht_tbl) %>%
       dcast(Region ~ Variable, value.var = "Value", fill = 0)
  
  cols_order <- intersect(c("Region", "EcoVal", "Subsidy", "TotalIncome"), names(wide_tbl))
  
  cat("MARKDOWN TABLE:\n")
  cat(kable(wide_tbl[, ..cols_order], format = "markdown"))
  cat("\n")
}
