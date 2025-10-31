package.list <- c("tidyverse", "data.table", "unpivotr", "gamstransfer")

for (pkg in package.list) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE) # Load the package after installation
  }
}
