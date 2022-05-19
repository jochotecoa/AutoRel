library('progress')
library('caret')

# To run this algorithm, you need 2 files: norm_counts and res.

# This is the column name of your 'coldata' dataframe which you specified
# in DESeq2 to split between control and treatment

# contrast_group = NULL



norm_counts = DESeq2::counts(object = dds, normalized = T)


res <- results(dds)


  source('scripts/simulated_dataset/confusion_matrix_population_vs_sampling_pvalues.R')
  
  if (!dir.exists('data/simulated_data/3R')) {
    dir.create('data/simulated_data/3R', recursive = T)
  }
  norm_counts_path = 'data/simulated_data/3R/norm_counts.rds'
  norm_counts_features_path = 'data/simulated_data/3R/norm_counts_features.rds'
  
  
  saveRDS(file = norm_counts_path, norm_counts)
  source('scripts/simulated_dataset/deriving_features_from_normalized_counts.R')
  
  res_dds_path = 'data/simulated_data/3R/results_dds_deseq2.rds'
  all_res_path = 'data/simulated_data/3R/all_res.rds'
  saveRDS(object = res, file = res_dds_path)
  
  source('scripts/pre-processing/deriving_features_from_res.R')
  
  deseq2_dataset_all_path = 'data/simulated_data/3R/deseq2_features_all.rds'
  source('scripts/simulated_dataset/merge_countfeats_with_statfeats.R')
  
  train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21/significant_labels'
  
  model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))
  deseq2_dataset_all = deseq2_dataset_all_path %>% readRDS() %>% as.data.frame()
  
  colnames(deseq2_dataset_all) %<>% make.names() %>% 
    gsub('abo', 'avo', .) %>% 
    gsub('avove', 'above', .)
  
  deseq2_dataset_all[deseq2_dataset_all == T] = 1
  deseq2_dataset_all[deseq2_dataset_all == F] = 0
  
  
  unclass <- function(x) {
    return(class(unlist(x)))
  }
  
  logi_cols = deseq2_dataset_all %>% 
    sapply(class) %>% 
    as.data.frame() %>% 
    .[1,, F] %>% 
    unlist()
  
  deseq2_dataset_all[, logi_cols == 'matrix'] %<>% apply(2, as.integer)
  
  incorrect = model_treebag$coefnames[!model_treebag$coefnames %in% colnames(deseq2_dataset_all)]
  stopifnot(length(incorrect) == 0)
  
  pred <- data.frame(pred = predict(model_treebag, newdata = deseq2_dataset_all), 
                     rowname = deseq2_dataset_all$rowname)
  

if (!dir.exists('output/simulated_data/3R/')) {
  dir.create('output/simulated_data/3R/', recursive = T)
}

saveRDS(conf_matr_all_df, 'output/simulated_data/3R/relevant_performance.rds')
saveRDS(conf_matr_all_rel_2_df, 'output/simulated_data/3R/rel_dub_performance.rds')
saveRDS(conf_matr_all_sign_df, 'output/simulated_data/3R/significant_performance.rds')


rel_sign_ratio = conf_matr_all_df/conf_matr_all_sign_df
rel_2_sign_ratio = conf_matr_all_rel_2_df/conf_matr_all_sign_df

par(mar = c(10, 5, 5, 5))
rel_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2))
abline(1, 0)
dev.off()

par(mar = c(10, 5, 5, 5))
rel_2_sign_ratio[, c(1, 2, 8:11, 18)] %>% boxplot(las=2, cex.axis = 1, ylim = c(0, 2))
abline(1, 0)
dev.off()


conf_matr_all_df_2 = conf_matr_all_df
colnames(conf_matr_all_df_2) %<>% paste0(., '_relevant')
conf_matr_all_rel_2_df_2 = conf_matr_all_rel_2_df
colnames(conf_matr_all_rel_2_df_2) %<>% paste0(., '_relevant_dubious')
conf_matr_all_sign_df_2 = conf_matr_all_sign_df
colnames(conf_matr_all_sign_df_2) %<>% paste0(., '_significant')

conf_matr_comb_df = cbind.data.frame(conf_matr_all_df_2, conf_matr_all_sign_df_2)

