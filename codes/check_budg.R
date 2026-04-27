library(data.table)
library(gamstransfer)

GLOBIOM.file <- "inputs/output_149_merged.gdx"

output_ag <- tryCatch(
  readGDX(GLOBIOM.file, symbols = "OUTPUT_AG"),
  error = function(e) stop("Failed to read OUTPUT_AG: ", e$message)
)

output_ag <- output_ag$OUTPUT_AG$records
setDT(output_ag)

new_names <- c("ALLRUN", "VAR_ID", "VAR_UNIT", "ANYREGION", "ITEM_AG",
               "ALLSCEN1","ALLSCEN2","ALLSCEN3","Year","Value")

if (all(grepl("^uni_", names(output_ag)[1:9]))) {
  setnames(output_ag, old = names(output_ag), new = new_names)
}

# Subsidies / BUDG check
budg_data <- output_ag[VAR_ID == "BUDG"]
if(nrow(budg_data) == 0){
  cat("No data found for VAR_ID == 'BUDG'\n")
} else {
  cat("BUDG Data found!\n")
  cat("Dimensions (Unique values):\n")
  cat("VAR_UNIT: ", paste(unique(budg_data$VAR_UNIT), collapse = ", "), "\n")
  cat("ITEM_AG: ", paste(head(unique(budg_data$ITEM_AG), 20), collapse = ", "), "\n")
  if (length(unique(budg_data$ITEM_AG)) > 20) {
      cat("... (and more items)\n")
  }
  cat("Sample Rows:\n")
  print(head(budg_data))
}
