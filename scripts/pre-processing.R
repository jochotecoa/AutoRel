source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))

forceLibrary('caret')

deseq2_dataset = readRDS(file = 'data/apap_hecatos/deseq2_dataset.rds')


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

X_over = X[c(nonsign_rows, dub_rows_over, sign_rows_over), , F]

# Under-sampling
nonsign_rows = which(Y == 'nonsignificant')
dub_rows = which(Y == 'dubious')
sign_rows = which(Y == 'significant')

nonsign_rows_under = sample(nonsign_rows, size = length(dub_rows))
sign_rows_under = sample(sign_rows, size = length(dub_rows))

X_under = X[c(dub_rows, nonsign_rows_under, sign_rows_under), , F]

# Pre-processing ----------------------------------------------------------
# Zero- and Near Zero-Variance Predictors
ncol(X) 
nzv_m <- nearZeroVar(X, saveMetrics = T)
nzv_m_under <- nearZeroVar(X_under, saveMetrics = T)
nzv_m_over <- nearZeroVar(X_over, saveMetrics = T)

nzv_intrsct = intersect(colnames(X)[nzv_m$nzv], intersect(colnames(X)[nzv_m_over$nzv], colnames(X)[nzv_m_under$nzv]))

# nzv_m[nzv_m$nzv, ] %>% View()

if (length(nzv_intrsct) > 0) {
  print(paste('Predictors with NZV:', nzv_intrsct, collapse = ' '))
  # warning(paste(paste0(zv_colnames, collapse = ', '), 'presented zero variance'))
  # warning(paste(paste0(nzv_colnames, collapse = ', '), 'presented near zero variance'))
  X <- X[, !(colnames(X) %in% nzv_intrsct)]
}

# Creating Dummy Variables
ncol(X) 
dummies <- dummyVars(" ~ .", data = X)
X = predict(dummies, newdata = X)
X = X %>% as.data.frame()
ncol(X) 

# Zero- and Near Zero-Variance Predictors
ncol(X) 
nzv_m <- nearZeroVar(X, saveMetrics = T)
nzv_m_under <- nearZeroVar(X_under, saveMetrics = T)
nzv_m_over <- nearZeroVar(X_over, saveMetrics = T)

nzv_intrsct = intersect(colnames(X)[nzv_m$nzv], intersect(colnames(X)[nzv_m_over$nzv], colnames(X)[nzv_m_under$nzv]))

# nzv_m[nzv_m$nzv, ] %>% View()

if (length(nzv_intrsct) > 0) {
  print(paste('Predictors with NZV:', nzv_intrsct, collapse = ' '))
  # warning(paste(paste0(zv_colnames, collapse = ', '), 'presented zero variance'))
  # warning(paste(paste0(nzv_colnames, collapse = ', '), 'presented near zero variance'))
  X <- X[, !(colnames(X) %in% nzv_intrsct)]
}


# Identifying Correlated Predictors ---------------------------------------


ncol(X)

descrCor <- cor(na.omit(X))
highlyCor <- findCorrelation(descrCor, cutoff = .99)

descrCor_under <- cor(na.omit(X_under))
highlyCor_under <- findCorrelation(descrCor_under, cutoff = .99)

descrCor_over <- cor(na.omit(X_over))
highlyCor_over <- findCorrelation(descrCor_over, cutoff = .99)

highlyCor_intrsct = intersect(highlyCor, intersect(highlyCor_over, highlyCor_under))

corrplot::corrplot(descrCor[-highlyCor, highlyCor])
if (length(highlyCor_intrsct) > 0) {
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


X = cbind.data.frame(X, Y) %>% 
  na.omit()
colnames(X)[ncol(X)] = 'significance'
Y = X['significance']
X = X[-ncol(X)]
normalization <- preProcess(X, verbose = T, method = c("center", "scale"))
X_c_s <- predict(normalization, X) %>%
  as.data.frame()

saveRDS(object = X, file = 'data/apap_hecatos/whole_data_preds.rds')
saveRDS(object = X_c_s, file = 'data/apap_hecatos/whole_data_centered_scaled_preds.rds')
saveRDS(object = Y, file = 'data/apap_hecatos/whole_data_target.rds')
