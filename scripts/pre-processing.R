forceLibrary('caret')



X = deseq2_dataset[, -grep('significance', colnames(deseq2_dataset))] %>% 
  as.data.frame() %>% 
  column_to_rownames('ensembl_gene_id')
Y = deseq2_dataset[, grep('significance', colnames(deseq2_dataset))]


# Convert all logical variables to numerical
X[sapply(X, is.logical)] = X[sapply(X, is.logical)] %>% sapply(as.numeric)

# Over-sampling
nonsign_rows = which(Y == 'nonsignificant')
dub_rows = which(Y == 'dubious')
sign_rows = which(Y == 'significant')

dub_rows_over = sample(dub_rows, size = length(nonsign_rows), replace = T)
sign_rows_over = sample(sign_rows, size = length(nonsign_rows), replace = T)

X = X[c(nonsign_rows, dub_rows_over, sign_rows_over), , F]

# Under-sampling
nonsign_rows = which(Y == 'nonsignificant')
dub_rows = which(Y == 'dubious')
sign_rows = which(Y == 'significant')

nonsign_rows_under = sample(nonsign_rows, size = length(dub_rows))
sign_rows_under = sample(sign_rows, size = length(dub_rows))

X = X[c(dub_rows, nonsign_rows_under, sign_rows_under), , F]

# Pre-processing ----------------------------------------------------------
# Zero- and Near Zero-Variance Predictors
ncol(X) 
nzv_m <- nearZeroVar(X, saveMetrics = T)
nzv <- nearZeroVar(X)
nzv_m[nzv, ] %>% View()

if (length(nzv) > 0) {
  zv_colnames = colnames(X)[nzv_m$zeroVar]
  nzv_colnames = colnames(X)[as.logical(nzv_m$nzv - nzv_m$zeroVar)]
  nzv_m[nzv, ] %>% print()
  # warning(paste(paste0(zv_colnames, collapse = ', '), 'presented zero variance'))
  # warning(paste(paste0(nzv_colnames, collapse = ', '), 'presented near zero variance'))
  X <- X[, -nzv]
}

# Creating Dummy Variables
ncol(X) 
dummies <- dummyVars(" ~ .", data = X)
X = predict(dummies, newdata = X)
X = X %>% as.data.frame()
ncol(X) 

# Zero- and Near Zero-Variance Predictors
ncol(X) - 1
nzv_m <- nearZeroVar(X, saveMetrics = T)
nzv <- nearZeroVar(X)
nzv_m[nzv, ] %>% View()

if (length(nzv) > 0) {
  zv_colnames = colnames(X)[nzv_m$zeroVar]
  nzv_colnames = colnames(X)[as.logical(nzv_m$nzv - nzv_m$zeroVar)]
  nzv_m[nzv, ] %>% print()
  # warning(paste(paste0(zv_colnames, collapse = ', '), 'presented zero variance'))
  # warning(paste(paste0(nzv_colnames, collapse = ', '), 'presented near zero variance'))
  X <- X[, -nzv]
}




# Identifying Correlated Predictors
ncol(X)
X = X %>% 
  dplyr::select(-circ_score_min) # it only has 2 values: 0 (rows with NAs) and 8


descrCor <- cor(na.omit(X))
highlyCor <- findCorrelation(descrCor, cutoff = .75)
corrplot::corrplot(descrCor[-highlyCor, highlyCor])
if (length(highlyCor) > 0) {
  X <- X[,-highlyCor]
}


# Linear Dependencies
ncol(X)

comboInfo <- findLinearCombos(na.omit(X))
if (length(comboInfo$remove) > 0) {
  X = X[, -comboInfo$remove]
}

X = X %>% as.data.frame()
ncol(X)

# Data splitting ----------------------------------------------------------


X_na_omit = cbind.data.frame(X, Y) %>% 
  na.omit()
colnames(X_na_omit)[ncol(X_na_omit)] = 'proteomics_value'

normalization <- preProcess(X, verbose = T, method = c("center", "scale", "knnImpute"))
X <- predict(normalization, X) %>%
  as.data.frame()

saveRDS(object = X, file = 'data/whole_data_preds_knnimpute.rds')
saveRDS(object = Y, file = 'data/whole_data_target_knnimpute.rds')
