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
message("Downloading model file from GitHub...")
# We use the version from DESCRIPTION or a hardcoded fallback
repo_url <- "https://github.com/jochotecoa/AutoRel"
model_name <- "autorrel.rds"
model_path <- "inst/extdata/autorrel.rds"

# For now, we point to the 'latest' release or a specific tag
# Once you create a release on GitHub, this link will work:
model_url <- paste0(repo_url, "/releases/download/v0.1.0/", model_name)

if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)

tryCatch({
  download.file(model_url, model_path, mode = "wb", quiet = FALSE)
  message(paste("Model downloaded successfully to", model_path))
}, error = function(e) {
  message("Warning: Model download from GitHub failed.")
  message("This is expected if you haven't created the 'v0.1.0' release yet.")
  message("Falling back to OneDrive for now...")
  
  onedrive_url <- "https://onedrive.live.com/download?cid=3B8629BED5CB140F&resid=3B8629BED5CB140F%21117&authkey=ABgUnQNkPS48vZI"
  tryCatch({
    download.file(onedrive_url, model_path, mode = "wb")
    message("Model downloaded successfully from OneDrive fallback.")
  }, error = function(e2) {
    message("ERROR: All download attempts failed.")
  })
})

message("AutoRel installation complete.")