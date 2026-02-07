# AutoRel Installer

# 1. Set CRAN Mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

message("Installing base dependencies...")
required_packages <- c("optparse", "tidyverse", "caret", "progress", "magrittr", "randomForest", "e1071", "BiocManager", "pheatmap", "pbmcapply", "rmarkdown", "knitr", "DT", "devtools")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 2. Install Bioconductor dependencies
message("Installing Bioconductor dependencies...")
bioc_packages <- c("biomaRt", "tximport", "DESeq2", "BiocParallel", "edgeR")
BiocManager::install(bioc_packages, ask = FALSE, update = FALSE)

# 3. Download Model
message("Downloading model file...")
# Using a slightly different approach for the download to handle potential redirects/headers
model_url <- "https://onedrive.live.com/download?cid=3B8629BED5CB140F&resid=3B8629BED5CB140F%21117&authkey=ABgUnQNkPS48vZI"
model_path <- "models/autorrel.rds"

if (!dir.exists("models")) dir.create("models")

tryCatch({
  download.file(model_url, model_path, mode = "wb", quiet = FALSE)
  message("Model downloaded successfully to models/autorrel.rds")
}, error = function(e) {
  message("Warning: Model download failed (403 Forbidden or Network Error).")
  message("The original OneDrive link might be expired.")
  message("Please manually place your 'autorrel.rds' model file into the 'models/' folder.")
})

message("AutoRel installation complete.")