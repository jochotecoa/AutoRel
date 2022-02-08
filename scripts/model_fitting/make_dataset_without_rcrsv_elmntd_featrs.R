

rf_rfe = readRDS(rfe_path)


tol_size_acc = pickSizeTolerance(rf_rfe$results, 'Accuracy', maximize = T)
tol_size_kap = pickSizeTolerance(rf_rfe$results, 'Kappa', maximize = T)

tol_size = max(tol_size_acc, tol_size_kap)

min_features = rf_rfe %>% 
  varImp %>% 
  head(tol_size) %>% 
  rownames()

apap_data = apap_dataset_path %>% readRDS()

apap_data_min_feats = apap_data[, min_features]
apap_data_min_feats$significance = apap_data[, 'significance']

apap_data_min_feats_path = apap_data_min_feats_path %>% 
  gsub(pattern = 'X', replacement = tol_size)

apap_data_min_feats %>% saveRDS(apap_data_min_feats_path)

