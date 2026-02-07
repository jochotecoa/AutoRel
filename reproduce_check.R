# AutoRel Reproducibility Check

message("Verifying AutoRel environment...")

required_packages <- c("DESeq2", "tidyverse", "glmnet", "randomForest")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  message("Warning: The following R packages are missing:")
  print(missing_packages)
} else {
  message("All required packages are available.")
}

core_script <- "scripts/autorrel/run_autorrel.R"
if(file.exists(core_script)) {
  message("SUCCESS: Core model script found.")
} else {
  message("ERROR: Core model script NOT found at ", core_script)
}

message("Ready to run. Use scripts/autorrel/run_autorrel.R as your entry point.")
