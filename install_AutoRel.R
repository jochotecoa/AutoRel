# AutoRel Installer

message("Installing dependencies...")
required_packages <- c("optparse", "tidyverse", "caret", "progress", "magrittr", "randomForest", "e1071")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos="http://cran.rstudio.com/")

message("Downloading model file...")
model_url <- "https://onedrive.live.com/download?cid=3B8629BED5CB140F&resid=3B8629BED5CB140F%21117&authkey=ABgUnQNkPS48vZI"
model_path <- "models/autorrel.rds"

if (!file.exists(model_path)) {
  download.file(model_url, model_path, mode = "wb")
  message("Model downloaded successfully to models/autorrel.rds")
} else {
  message("Model file already exists.")
}

message("AutoRel installation complete.")
