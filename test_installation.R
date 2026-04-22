# Generate Mock Data for Testing
set.seed(42)
genes <- paste0("GENE_", 1:100)
samples <- paste0("SAMPLE_", 1:6)

# 1. Normalized Counts
counts <- matrix(rnorm(600, mean=10, sd=2), nrow=100, ncol=6)
colnames(counts) <- samples
rownames(counts) <- genes
write.csv(counts, "data/test_data/counts.csv")

# 2. Results (DESeq2 style)
res <- data.frame(
  baseMean = runif(100, 10, 1000),
  log2FoldChange = rnorm(100, 0, 2),
  lfcSE = runif(100, 0.1, 0.5),
  stat = rnorm(100, 0, 5),
  pvalue = runif(100, 0, 0.05),
  padj = runif(100, 0, 0.05),
  row.names = genes
)
write.csv(res, "data/test_data/results.csv")

# 3. Metadata (coldata)
metadata <- data.frame(
  Group = factor(c(rep("Control", 3), rep("Treatment", 3))),
  row.names = samples
)
write.csv(metadata, "data/test_data/metadata.csv")

# 4. Ensure Model exists (create dummy if missing for testing)
if (!file.exists("inst/extdata/autorrel.rds")) {
  message("Model not found. Creating a DUMMY model for testing purposes...")
  if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)
  
  library(randomForest)
  # Create a dummy model with at least 2 rows and 2 classes
  dummy_df <- data.frame(
    rowname=c("gene1", "gene2"), 
    baseMean=c(1, 2), 
    pred=factor(c("relevant", "irrelevant"), levels=c("relevant", "irrelevant"))
  )
  dummy_mod <- randomForest(pred ~ baseMean, data=dummy_df)
  saveRDS(dummy_mod, "inst/extdata/autorrel.rds")
}

message("Testing installation...")

# 4. Run the Tool
rscript_bin <- file.path(R.home("bin"), "Rscript")
cmd <- paste(shQuote(rscript_bin), "run_AutoRel.R --counts data/test_data/counts.csv --results data/test_data/results.csv --metadata data/test_data/metadata.csv --contrast Group --control Control --output output/test_run --report")
message(paste("Executing:", cmd))
system(cmd)

# 5. Verify Results
if (file.exists("output/test_run/all_results.csv") && file.exists("output/test_run/AutoRel_Report.html")) {
  message("\nSUCCESS: AutoRel test run (including Report) completed successfully!")
  message("Results found in output/test_run/all_results.csv")
  message("Report found in output/test_run/AutoRel_Report.html")
} else {
  stop("\nFAILURE: Output files or Report were not generated. Check for R errors above.")
}
