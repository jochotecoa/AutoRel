for (path_model_i in list.files('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9/', 
                                full.names = T, recursive = T, pattern = '.rds')) {
  model_i_name = path_model_i %>% 
    gsub('/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9//', '', .) %>% 
    gsub(pattern = '.rds', replacement = '', x = .) %>% 
    gsub(pattern = '\\/', replacement = '_', x = .) %>% 
    paste0(., '_model')
  
  model_i = path_model_i %>% readRDS()
  assign(x = model_i_name, value = model_i)
  
}

resample_results <- resamples(list(
  # BAM=bam_original_model,
  CSimca=CSimca_original_model,
  KKNN=kknn_original_model,
  lssvmRadial=lssvmRadial_original_model,
  MULTINOM=multinom_original_model,
  NAIVE_BAYES=naive_bayes_original_model,
  ordinalNet=ordinalNet_original_model,
  # KKNN="rf_model_rf_over_model", 
  # KKNN="rf_model_rf_under_model", 
  PAM=pam_original_model,
  RF=rf_original_model,
  RPART2=rpart2_original_model,
  sparseLDA=sparseLDA_original_model,
  TREEBAG=treebag_original_model
  # xgbDART=xgbDART_original_model
  # RPART2=model_rpart2
)
)

resample_results <- resamples(list(KKNN=model_kknn, RPART2=model_rpart2))


densityplot(resample_results , metric = "Accuracy" ,auto.key = list(columns = 3))

# plot all (higher is better)
bwplot(resample_results , metric = c("Kappa","Accuracy"))
