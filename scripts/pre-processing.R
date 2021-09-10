# Pre-processing ----------------------------------------------------------
# Zero- and Near Zero-Variance Predictors
ncol(mrna_prot_df) - 1
nzv_m <- nearZeroVar(mrna_prot_df, saveMetrics = T)
nzv <- nearZeroVar(mrna_prot_df)
nzv_m[nzv, ] %>% View()

if (length(nzv) > 0) {
  zv_colnames = colnames(mrna_prot_df)[nzv_m$zeroVar]
  nzv_colnames = colnames(mrna_prot_df)[as.logical(nzv_m$nzv - nzv_m$zeroVar)]
  nzv_m[nzv, ] %>% print()
  # warning(paste(paste0(zv_colnames, collapse = ', '), 'presented zero variance'))
  # warning(paste(paste0(nzv_colnames, collapse = ', '), 'presented near zero variance'))
  mrna_prot_df <- mrna_prot_df[, -nzv]
}

# Creating Dummy Variables
ncol(mrna_prot_df) - 1
dummies <- dummyVars(" ~ .", data = mrna_prot_df)
mrna_prot_df = predict(dummies, newdata = mrna_prot_df)
mrna_prot_df = mrna_prot_df %>% as.data.frame()
ncol(mrna_prot_df) - 1

# Zero- and Near Zero-Variance Predictors
ncol(mrna_prot_df) - 1
nzv_m <- nearZeroVar(mrna_prot_df, saveMetrics = T)
nzv <- nearZeroVar(mrna_prot_df)
nzv_m[nzv, ] %>% View()

if (length(nzv) > 0) {
  zv_colnames = colnames(mrna_prot_df)[nzv_m$zeroVar]
  nzv_colnames = colnames(mrna_prot_df)[as.logical(nzv_m$nzv - nzv_m$zeroVar)]
  nzv_m[nzv, ] %>% print()
  # warning(paste(paste0(zv_colnames, collapse = ', '), 'presented zero variance'))
  # warning(paste(paste0(nzv_colnames, collapse = ', '), 'presented near zero variance'))
  mrna_prot_df <- mrna_prot_df[, -nzv]
}


X = mrna_prot_df[, -grep('proteomics', colnames(mrna_prot_df))] %>% as.data.frame()
Y = mrna_prot_df[, grep('proteomics', colnames(mrna_prot_df))]


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


mrna_prot_df_na_omit = cbind.data.frame(X, Y) %>% 
  na.omit()
colnames(mrna_prot_df_na_omit)[ncol(mrna_prot_df_na_omit)] = 'proteomics_value'

normalization <- preProcess(X, verbose = T, method = c("center", "scale", "knnImpute"))
X <- predict(normalization, X) %>%
  as.data.frame()

saveRDS(object = X, file = 'data/whole_data_preds_knnimpute.rds')
saveRDS(object = Y, file = 'data/whole_data_target_knnimpute.rds')
