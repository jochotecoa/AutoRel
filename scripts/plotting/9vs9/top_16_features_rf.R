source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'tibble'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
model_rf_all_feats = readRDS(paste0(train_mod_path, '/rf/all_features.rds'))
feats_16 = model_rf_all_feats %>% 
  varImp 
feats_16 = feats_16$importance[order(feats_16$importance$Overall, decreasing=T),,F ] %>% 
  head(16) 
par(mar=c(4,11,4,4))
feats_16 %>% 
  unlist %>% 
  sort %>%
  barplot(., names.arg = rownames(feats_16)[nrow(feats_16):1], horiz=T, las =2)
