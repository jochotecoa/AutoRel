#' Derive Features from Normalized Counts
#' @import tidyverse magrittr edgeR
#' @export
derive_features_from_counts <- function(norm_counts, coldata, contrast_group, control_level) {
  # 1. Clean row names
  if (is.character(norm_counts[,1])) {
    norm_counts <- norm_counts %>% tibble::column_to_rownames(colnames(norm_counts)[1])
  }
  
  # 2. Identify groups
  colnames_con_logi <- grepl(pattern = control_level, coldata[, contrast_group])
  colnames_treat_logi <- !grepl(pattern = control_level, coldata[, contrast_group])
  
  # 3. Rename columns internally for consistency
  orig_names <- colnames(norm_counts)
  colnames(norm_counts)[colnames_con_logi] <- paste0('ConDMSO_', orig_names[colnames_con_logi])
  colnames(norm_counts)[colnames_treat_logi] <- paste0('APA_The_', orig_names[colnames_treat_logi])
  
  colnames_con <- colnames(norm_counts)[grep('ConDMSO', colnames(norm_counts))]
  colnames_treat <- colnames(norm_counts)[grep('APA_The', colnames(norm_counts))]
  
  # 4. CPM Rule
  cpm_norm <- norm_counts %>% edgeR::cpm() %>% as.data.frame()
  cpm_norm_con <- cpm_norm[, colnames_con]
  cpm_norm_treat <- cpm_norm[, colnames_treat]
  
  cpm_feat <- data.frame(
    rule_cpm_0.75_above_1 = (rowSums(cpm_norm_con > 1, na.rm = T)/ncol(cpm_norm_con) >= 0.75) | 
                            (rowSums(cpm_norm_treat > 1, na.rm = T)/ncol(cpm_norm_treat) >= 0.75),
    row.names = rownames(norm_counts)
  )
  
  # 5. Summary Stats (Mean, SD, Var, Quantiles)
  norm_counts_con <- norm_counts[, colnames_con]
  norm_counts_treat <- norm_counts[, colnames_treat]
  
  # Helper for stats
  get_stats <- function(df, suffix) {
    res_stats <- data.frame(
      mean = apply(df, 1, mean),
      sd = apply(df, 1, sd),
      var = apply(df, 1, var),
      row.names = rownames(df)
    )
    quants <- as.data.frame(t(apply(df, 1, quantile, seq(0, 1, 0.05))))
    colnames(quants) <- paste0('quantile_', colnames(quants))
    res_stats <- cbind(res_stats, quants)
    
    # Subsample median (10 portions)
    num_var <- ncol(df)
    ct_port <- num_var / 10
    for (portion in 1:10) {
      c_i <- max(1, ceiling(ct_port * (portion - 1)))
      c_f <- floor(ct_port * portion)
      if (c_f >= c_i) {
        y <- df[, c_i:c_f, drop=FALSE]
        res_stats[[paste0(portion, 'th_subset_median')]] <- apply(y, 1, median)
      } else {
        res_stats[[paste0(portion, 'th_subset_median')]] <- df[, min(c_i, num_var)]
      }
    }
    
    res_stats$N_nonexpressed_samples <- apply(df, 1, function(x) sum(x == 0))
    res_stats$Proportion_nonexpressed_samples <- res_stats$N_nonexpressed_samples / ncol(df)
    
    colnames(res_stats) <- paste0(colnames(res_stats), "_", suffix)
    return(res_stats)
  }
  
  stats_con <- get_stats(norm_counts_con, "ConDMSO")
  stats_treat <- get_stats(norm_counts_treat, "APA_The")
  
  final_feats <- cbind(stats_con, stats_treat, cpm_feat)
  
  # 6. Fold Changes between features
  feature_names <- colnames(stats_con) %>% gsub('_ConDMSO', '', .)
  for (f_name in feature_names) {
    f_con <- paste0(f_name, "_ConDMSO")
    f_treat <- paste0(f_name, "_APA_The")
    if (f_treat %in% colnames(final_feats)) {
      final_feats[[paste0('foldchange_', f_name)]] <- (final_feats[[f_treat]] + 0.01) / (final_feats[[f_con]] + 0.01)
    }
  }
  
  # 7. Quantile Rules (Simplified version of the complex rules in deriving_features script)
  final_feats$onequartilediff_rule <- (final_feats$`quantile_50%_APA_The` > final_feats$`quantile_75%_ConDMSO`) | 
                                      (final_feats$`quantile_50%_APA_The` < final_feats$`quantile_25%_ConDMSO`)
  
  return(final_feats)
}

#' Derive Features from DESeq2 Results
#' @export
derive_features_from_res <- function(res) {
  if (is.character(res[,1])) {
    res <- res %>% tibble::column_to_rownames(colnames(res)[1])
  }
  
  all_res <- as.data.frame(res)
  all_res$log2FoldChange[is.na(all_res$log2FoldChange)] <- 0
  all_res$lfcSE[is.na(all_res$lfcSE)] <- 1
  all_res$stat[is.na(all_res$stat)] <- 0
  all_res$pvalue[is.na(all_res$pvalue)] <- 1
  all_res$padj[is.na(all_res$padj)] <- 1
  all_res$fdrlowerthan0.01 <- all_res$padj < 0.01
  
  return(all_res)
}

#' Run AutoRel Prioritization
#' @export
run_prioritization <- function(norm_counts, res, coldata, contrast_group, control_level, model_path, output_path) {
  # 0. Validate and Clean Inputs
  cleaned <- validate_inputs(norm_counts, coldata, contrast_group, control_level)
  norm_counts <- cleaned$counts
  coldata <- cleaned$metadata
  
  message("Deriving features...")
  count_feats <- derive_features_from_counts(norm_counts, coldata, contrast_group, control_level)
  stat_feats <- derive_features_from_res(res)
  
  message("Merging datasets...")
  merged_data <- count_feats %>% 
    tibble::rownames_to_column("rowname") %>% 
    merge(tibble::rownames_to_column(stat_feats, "rowname"), by = "rowname")
  
  # Clean column names for model compatibility
  colnames(merged_data) <- make.names(colnames(merged_data)) %>% 
    gsub('abo', 'avo', .) %>% 
    gsub('avove', 'above', .)
  
  # Load model
  message("Loading model and predicting...")
  model <- readRDS(model_path)
  
  # Ensure logical columns are integer for the model
  merged_data <- merged_data %>% mutate(across(where(is.logical), as.integer))
  
  # Check for missing features
  missing_feats <- model$coefnames[!model$coefnames %in% colnames(merged_data)]
  if (length(missing_feats) > 0) {
    warning("Missing features in data: ", paste(missing_feats, collapse=", "))
  }
  
  # Predict
  preds <- predict(model, newdata = merged_data)
  results <- data.frame(
    rowname = merged_data$rowname,
    pred = preds
  )
  
  results$pred <- results$pred %>% 
    gsub('nonsignificant', 'irrelevant', .) %>% 
    gsub('significant', 'relevant', .)
  
  # Save results
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  write.csv(results, file.path(output_path, "all_results.csv"), row.names = FALSE)
  write.csv(results[results$pred == "relevant", ], file.path(output_path, "relevant_genes.csv"), row.names = FALSE)
  
  # Save temp data for reporting
  if (!dir.exists("data/temporary_data")) dir.create("data/temporary_data", recursive = TRUE)
  saveRDS(norm_counts, "data/temporary_data/norm_counts.rds")
  saveRDS(stat_feats, "data/temporary_data/results_dds_deseq2.rds")
  
  return(results)
}
