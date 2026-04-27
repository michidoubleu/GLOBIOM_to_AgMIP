library(data.table)
library(gamstransfer)

globiom_file <- "inputs/output_149_merged.gdx"
output_ag <- tryCatch({
  readGDX(globiom_file, symbols = "OUTPUT_AG")$OUTPUT_AG$records
}, error=function(e) NULL)

setDT(output_ag)
setnames(output_ag, old = names(output_ag), new = c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
                                                    "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value"))

cat("Items in YILD:\n")
print(head(unique(output_ag[VAR_ID == "YILD" & ANYREGION == "AUT", ITEM_AG]), 20))

# Can we calculate Barly Area = Prod / YILD?
barly_prod <- output_ag[VAR_ID == "Prod" & ANYREGION == "AUT" & ITEM_AG == "Barly" & ALLSCEN3 == "BASE" & Year == 2010]
barly_yild <- output_ag[VAR_ID == "YILD" & ANYREGION == "AUT" & ITEM_AG == "Barly" & ALLSCEN3 == "BASE" & Year == 2010]
cat("\nBarly Prod (AUT 2010):\n")
print(barly_prod)
cat("\nBarly YILD (AUT 2010):\n")
print(barly_yild)
