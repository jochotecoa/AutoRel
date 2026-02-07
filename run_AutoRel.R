#!/usr/bin/env Rscript
library(optparse)

# 1. Define CLI Arguments
option_list = list(
  make_option(c("-c", "--counts"), type="character", default=NULL, help="Path to normalized counts CSV", metavar="FILE"),
  make_option(c("-r", "--results"), type="character", default=NULL, help="Path to DESeq2 results CSV", metavar="FILE"),
  make_option(c("-m", "--metadata"), type="character", default=NULL, help="Path to coldata/metadata CSV", metavar="FILE"),
  make_option(c("-g", "--contrast"), type="character", default=NULL, help="Column name for contrast group"),
  make_option(c("-l", "--control"), type="character", default=NULL, help="Name of the control level"),
  make_option(c("-o", "--output"), type="character", default="output/autorrel_results", help="Output directory")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$counts) || is.null(opt$results) || is.null(opt$metadata) || is.null(opt$contrast)) {
  print_help(opt_parser)
  stop("Missing mandatory arguments. Please provide --counts, --results, --metadata, and --contrast.", call.=FALSE)
}

# 2. Load Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(magrittr)
  library(progress)
})

# 3. Read Data
message("Loading data...")
norm_counts <- read.csv(opt$counts, row.names = 1)
res <- read.csv(opt$results, row.names = 1)
coldata <- read.csv(opt$metadata, stringsAsFactors = TRUE, row.names = 1)
contrast_group <- opt$contrast
output_path <- opt$output

# 4. Handle Control Group
lvls = levels(coldata[, contrast_group])
if (!is.null(opt$control)) {
  if (!(opt$control %in% lvls)) {
    stop(paste("Control level", opt$control, "not found in levels:", paste(lvls, collapse=", ")))
  }
  control_level <- opt$control
  message(paste("Using control group:", control_level))
} else {
  # Fallback to interactive if not provided
  message("No control group specified via --control.")
  print(paste(seq(1, length(lvls)), ':', lvls))
  cat("Which level is your control group? (Type a number): ")
  control_idx = scan(n=1, quiet = TRUE)
  control_level = lvls[control_idx]
}

# 5. Execute Pipeline
# We need to set up variables that pipeline_autorrel.R expects
message("Running AutoRel Pipeline...")

# The original pipeline script has hardcoded model path and interactive scan.
# I will create a temporary 'patched' version of the pipeline to make it a tool.

pipeline_code <- readLines("scripts/autorrel/pipeline_autorrel.R")

# Patch 1: Remove the interactive scan
start_idx <- grep("lvls = levels", pipeline_code)
end_idx <- grep("control_level = lvls", pipeline_code)
pipeline_code[start_idx:end_idx] <- "# Control level handled by CLI wrapper"

# Patch 2: Fix model path
pipeline_code <- gsub("../temp_dir/autorrel.rds", "models/autorrel.rds", pipeline_code)

# Execute the patched code
eval(parse(text = pipeline_code))

message(paste("AutoRel finished. Results saved to:", output_path))
