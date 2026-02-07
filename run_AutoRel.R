#!/usr/bin/env Rscript
# AutoRel CLI Wrapper

# 1. Load Dependencies and Package Functions
# In a local dev environment, we source the R folder. 
# Once installed, user would do library(AutoRel)
if (file.exists("R/core_logic.R")) {
  invisible(lapply(list.files("R", full.names = TRUE), source))
} else {
  library(AutoRel)
}

library(optparse)

# 2. Define CLI Arguments
option_list = list(
  make_option(c("-c", "--counts"), type="character", default=NULL, help="Path to normalized counts CSV", metavar="FILE"),
  make_option(c("-r", "--results"), type="character", default=NULL, help="Path to DESeq2 results CSV", metavar="FILE"),
  make_option(c("-m", "--metadata"), type="character", default=NULL, help="Path to coldata/metadata CSV", metavar="FILE"),
  make_option(c("-g", "--contrast"), type="character", default=NULL, help="Column name for contrast group"),
  make_option(c("-l", "--control"), type="character", default=NULL, help="Name of the control level"),
  make_option(c("-o", "--output"), type="character", default="output/autorrel_results", help="Output directory"),
  make_option(c("--report"), type="logical", action="store_true", default=FALSE, help="Generate HTML report")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$counts) || is.null(opt$results) || is.null(opt$metadata) || is.null(opt$contrast)) {
  print_help(opt_parser)
  stop("Missing mandatory arguments. Please provide --counts, --results, --metadata, and --contrast.", call.=FALSE)
}

# 3. Load Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(magrittr)
  library(randomForest)
})

# 4. Resolve Model Path
# When installed, model is in system.file("extdata", "autorrel.rds", package="AutoRel")
model_path <- "inst/extdata/autorrel.rds"
if (!file.exists(model_path)) {
  model_path <- system.file("extdata", "autorrel.rds", package="AutoRel")
}

# 5. Run Prioritization
results <- run_prioritization(
  norm_counts = read.csv(opt$counts),
  res = read.csv(opt$results),
  coldata = read.csv(opt$metadata, stringsAsFactors = TRUE),
  contrast_group = opt$contrast,
  control_level = opt$control,
  model_path = model_path,
  output_path = opt$output
)

# 6. Generate Report
if (opt$report) {
  message("Generating Visual Report...")
  abs_output_path <- normalizePath(opt$output, mustWork = FALSE)
  report_template <- "inst/reports/report_template.Rmd"
  if (!file.exists(report_template)) {
    report_template <- system.file("reports", "report_template.Rmd", package="AutoRel")
  }
  
  rmarkdown::render(report_template, 
                    output_file = "AutoRel_Report.html",
                    output_dir = abs_output_path,
                    params = list(output_path = abs_output_path, contrast_group = opt$contrast),
                    quiet = TRUE)
  message(paste("Report generated at:", file.path(abs_output_path, "AutoRel_Report.html")))
}

message("AutoRel Analysis Complete.")