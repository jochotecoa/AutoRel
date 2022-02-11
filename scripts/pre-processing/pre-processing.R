source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))

forceLibrary('caret')

deseq2_dataset = readRDS(file = deseq2_dataset_path)

deseq2_dataset = deseq2_dataset[deseq2_dataset$baseMean > 0, ]

colnames(deseq2_dataset) = colnames(deseq2_dataset) %>% 
  make.names()

X = X_notproc = deseq2_dataset[, -grep('significance', colnames(deseq2_dataset))] %>% 
  as.data.frame() %>% 
  remove_rownames %>% 
  column_to_rownames('ensembl_gene_id')
Y = deseq2_dataset[, grep('significance', colnames(deseq2_dataset))]


# Convert all logical variables to numerical
X[sapply(X, is.logical)] = X[sapply(X, is.logical)] %>% sapply(as.numeric)

# # Over-sampling
# nonsign_rows = which(Y == 'nonsignificant')
# dub_rows = which(Y == 'dubious')
# sign_rows = which(Y == 'significant')
# 
# dub_rows_over = sample(dub_rows, size = length(nonsign_rows), replace = T)
# sign_rows_over = sample(sign_rows, size = length(nonsign_rows), replace = T)
# 
# X_over = X[c(nonsign_rows, dub_rows_over, sign_rows_over), , F]

# Under-sampling
nonsign_rows = which(Y == 'nonsignificant')
dub_rows = which(Y == 'dubious')
sign_rows = which(Y == 'significant')

nonsign_rows_under = sample(nonsign_rows, size = length(dub_rows))
sign_rows_under = sample(sign_rows, size = length(dub_rows))

X_under = X[c(dub_rows, nonsign_rows_under, sign_rows_under), , F]

# Pre-processing ----------------------------------------------------------
# Zero- and Near Zero-Variance Predictors
i_cols = ncol(X)

# freqCut = sum(Y == 'nonsignificant')/sum(Y != 'nonsignificant')
nzv_m_under <- nearZeroVar(X_under, saveMetrics = T)
nzv_feats = rownames(nzv_m_under[nzv_m_under$nzv, ])


if (length(nzv_feats) > 0) {
  
  print(paste('Predictors with NZV:', nzv_feats, collapse = ' '))
  X <- X[, !(colnames(X) %in% nzv_feats)]
  X_under <- X_under[, !(colnames(X_under) %in% nzv_feats)]
  
}

nzv_cols = ncol(X)
print(paste(i_cols - nzv_cols, 'features filtered due to NZV'))
# Identifying Correlated Predictors ---------------------------------------




descrCor <- cor(na.omit(X_under))
highlyCor <- findCorrelation(descrCor, cutoff = .99)

corrplot::corrplot(descrCor[-highlyCor, highlyCor], tl.cex = .5)
if (length(highlyCor) > 0) {
  X <- X[,-highlyCor]
  X_under <- X_under[,-highlyCor]
}


corr_cols = ncol(X)
print(paste(nzv_cols - corr_cols, 'features filtered due to correlation'))

# Linear Dependencies
ncol(X)

comboInfo <- findLinearCombos(na.omit(X))
if (length(comboInfo$remove) > 0) {
  print(colnames(X)[comboInfo$remove])
  X = X[, -comboInfo$remove]
}

X = X %>% as.data.frame()

lp_cols = ncol(X)
print(paste(corr_cols - lp_cols, 'features filtered due to linear dependencies'))


# Data splitting ----------------------------------------------------------


deseq2_dataset_2 = cbind.data.frame(X, Y) %>% 
  na.omit()
colnames(deseq2_dataset_2)[ncol(deseq2_dataset_2)] = 'significance'
# Y = deseq2_dataset_2['significance']
# X = deseq2_dataset_2[-ncol(deseq2_dataset_2)]
# normalization <- preProcess(X, verbose = T, method = c("center", "scale"))
# X_c_s <- predict(normalization, X) %>%
#   as.data.frame()


# normalization_notproc <- preProcess(X_notproc, verbose = T, method = c("center", "scale"))
# X_notproc_cnt_scl <- predict(normalization_notproc, X_notproc) %>%
#   as.data.frame()
# 
# saveRDS(object = X, file = 'data/apap_hecatos/whole_data_preds_21vs21.rds')
# saveRDS(object = X_c_s, file = 'data/apap_hecatos/whole_data_centered_scaled_preds_21vs21.rds')
# saveRDS(object = X_notproc_cnt_scl, file = 'data/apap_hecatos/whole_data_unpreprocessed_centered_scaled_preds_21vs21.rds')
# saveRDS(object = Y, file = 'data/apap_hecatos/whole_data_target_21vs21.rds')
saveRDS(object = deseq2_dataset_2, deseq2_dataset_2_path)

