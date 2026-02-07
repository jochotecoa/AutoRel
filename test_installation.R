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

message("Testing installation...")

# 4. Run the Tool
cmd <- "Rscript run_AutoRel.R --counts data/test_data/counts.csv --results data/test_data/results.csv --metadata data/test_data/metadata.csv --contrast Group --control Control --output output/test_run"
message(paste("Executing:", cmd))
system(cmd)

# 5. Verify Results
if (file.exists("output/test_run/all_results.csv")) {
  message("\nSUCCESS: AutoRel test run completed successfully!")
  message("Results found in output/test_run/all_results.csv")
} else {
  stop("\nFAILURE: Output files were not generated. Check for R errors above.")
}
