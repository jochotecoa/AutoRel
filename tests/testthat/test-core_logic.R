test_that("derive_features_from_counts computes standard stats correctly", {
  # Setup minimal mock counts and metadata
  counts <- matrix(c(10, 20, 30, 40, 0, 0, 100, 200), nrow = 2, byrow = TRUE)
  rownames(counts) <- c("geneA", "geneB")
  colnames(counts) <- c("S1", "S2", "S3", "S4")
  counts_df <- as.data.frame(counts)
  
  coldata <- data.frame(
    Group = c("Control", "Control", "Treatment", "Treatment"),
    row.names = c("S1", "S2", "S3", "S4")
  )
  
  # Source core_logic if not in package context
  if (file.exists("../../R/core_logic.R")) {
    source("../../R/core_logic.R")
  }
  
  # Execute
  feats <- derive_features_from_counts(counts_df, coldata, "Group", "Control")
  
  # Assertions
  expect_true(is.data.frame(feats))
  expect_equal(nrow(feats), 2)
  expect_true(all(c("mean_Control", "sd_Treatment", "foldchange_mean") %in% colnames(feats)))
  
  # Check some values
  # geneA Control: S1=10, S2=20 -> mean=15
  expect_equal(feats["geneA", "mean_Control"], 15)
  # geneA Treatment: S3=30, S4=40 -> mean=35
  expect_equal(feats["geneA", "mean_Treatment"], 35)
  
  # foldchange mean for geneA = (35 + 0.01) / (15 + 0.01)
  expected_fc <- (35 + 0.01) / (15 + 0.01)
  expect_equal(feats["geneA", "foldchange_mean"], expected_fc)
  
  # geneB Control: S1=0, S2=0 -> 2 nonexpressed
  expect_equal(feats["geneB", "N_nonexpressed_samples_Control"], 2)
  expect_equal(feats["geneB", "Proportion_nonexpressed_samples_Control"], 1.0)
})

test_that("derive_features_from_res handles missing values and formats correctly", {
  if (file.exists("../../R/core_logic.R")) {
    source("../../R/core_logic.R")
  }
  
  mock_res <- data.frame(
    log2FoldChange = c(1.5, NA),
    lfcSE = c(0.5, NA),
    stat = c(3.0, NA),
    pvalue = c(0.05, NA),
    padj = c(0.01, NA),
    row.names = c("geneA", "geneB")
  )
  
  res_feats <- derive_features_from_res(mock_res)
  
  expect_equal(res_feats["geneB", "log2FoldChange"], 0)
  expect_equal(res_feats["geneB", "padj"], 1)
  expect_equal(res_feats["geneB", "pvalue"], 1)
  
  expect_true("fdrlowerthan0.01" %in% colnames(res_feats))
  expect_false(res_feats["geneA", "fdrlowerthan0.01"]) # 0.01 is not < 0.01
})
