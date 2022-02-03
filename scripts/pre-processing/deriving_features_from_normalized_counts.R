source('scripts/functions/functions_JOA.R')
forceLibrary(c('biomaRt', "tximport", "dplyr", "DESeq2", "grid", "ggplot2", 
               "pheatmap", "BiocParallel", 'tibble', 'edgeR'))


# Functions ---------------------------------------------------------------

removeOutliers <- function(x) {
  otlrs = outlier(x)
  indx_otlrs = x %in% otlrs
  x = x[!indx_otlrs]
  return(x)
}

# Input data --------------------------------------------------------------

norm_counts = norm_counts_path %>% readRDS

colnames_con = subset(x = colnames(norm_counts), 
                      grepl('ConDMSO', colnames(norm_counts)))
colnames_treat = subset(x = colnames(norm_counts), 
                        grepl('APA_The', colnames(norm_counts)))


# CPM rule ----------------------------------------------------------------
cpm_time = Sys.time()

cpm_norm_counts = cpm(norm_counts) %>% as.data.frame()
cpm_norm_counts_con = cpm_norm_counts[, colnames_con]
cpm_norm_counts_treat = cpm_norm_counts[, colnames_treat]

cpm_features_con = data.frame(rule_cpm_0.75_above_1 = rowSums(cpm_norm_counts_con > 1, na.rm = T)/ncol(cpm_norm_counts_con) >= 0.75)
cpm_features_treat = data.frame(rule_cpm_0.75_above_1 = rowSums(cpm_norm_counts_treat > 1, na.rm = T)/ncol(cpm_norm_counts_treat) >= 0.75)


cpm_feature = 
  data.frame(rule_cpm_0.75_above_1 = cpm_features_con$rule_cpm_0.75_above_1 | cpm_features_treat$rule_cpm_0.75_above_1, 
             row.names = rownames(cpm_features_con))

# manual_degs_2 = res %>% 
#   as.data.frame() %>% 
#   rownames_to_column('ensembl_gene_id') %>% 
#   merge.data.frame(x = ., y = manual_degs, by = 'ensembl_gene_id', all.y = T)
# 
# manual_degs_2$padj[is.na(manual_degs_2$padj)] = 1

norm_counts_2 = norm_counts %>% 
  as.data.frame() %>% 
  na.omit()


norm_counts_con = norm_counts_2[, grep('ConDMSO', colnames(norm_counts_2))]
con_length = ncol(norm_counts_con)
norm_counts_treat = norm_counts_2[, grep('APA_The', colnames(norm_counts_2))]
treat_length = ncol(norm_counts_treat)


# Mean, SD, and Var -------------------------------------------------------
meansdvar_time = Sys.time()
print(cpm_time - meansdvar_time)

apply_list_fns <- function(x, fns = c('mean', 'sd', 'var'), quantiles = NULL) {
  for (fns_i in fns) {
    fns_i_chr = fns_i
    fns_i_funct = get('fns_i')
    if (!is.null(quantiles) & grepl('quantile', fns_i_chr)) {
      for (quant in quantiles) {
        x[, paste0(fns_i_chr, '_', quant)] = x %>%
          apply(1, fns_i_funct, quant)
      }
    } else {
      x[, fns_i_chr] = x %>%
        apply(1, fns_i_funct)
      
    }
    
  }
  return(x)
}

norm_counts_con_2 = norm_counts_con %>% 
  apply_list_fns(fns = c('mean', 'sd', 'var')) 

norm_counts_treat_2 = norm_counts_treat %>% 
  apply_list_fns(fns = c('mean', 'sd', 'var'))

# Quantiles ---------------------------------------------------------------
quantile_time = Sys.time()
print(meansdvar_time - quantile_time)


norm_counts_con_quantiles = norm_counts_con %>% 
  apply(1, quantile, seq(0, 1, 0.05)) %>% 
  t()
norm_counts_treat_quantiles = norm_counts_treat %>% 
  apply(1, quantile, seq(0, 1, 0.05)) %>% 
  t()

colnames(norm_counts_con_quantiles) = colnames(norm_counts_con_quantiles) %>% 
  paste0('quantile_', .)
colnames(norm_counts_treat_quantiles) = colnames(norm_counts_treat_quantiles) %>% 
  paste0('quantile_', .)

stopifnot(identical(rownames(norm_counts_con_2), 
                    rownames(norm_counts_con_quantiles)), 
          identical(rownames(norm_counts_treat_quantiles), 
                    rownames(norm_counts_con_quantiles)))

norm_counts_con_2 = norm_counts_con_2 %>% 
  cbind.data.frame(norm_counts_con_quantiles)
norm_counts_treat_2 = norm_counts_treat_2 %>% 
  cbind.data.frame(norm_counts_treat_quantiles)

# Feature the median of each 1/10th of the samples ------------------------
onetenth_time = Sys.time()
print(quantile_time - onetenth_time)


subsample_median <- function(x, num_var, portions) {
  prev_port = 0
  c_i = 1
  ct_port = num_var/portions
  for (portion in 1:portions) {
    y = x[, 1:num_var]
    
    c_f = floor(ct_port*portion)
    if (c_f <= c_i) {
      x[, paste0(portion, 'th_subset_median')] = y[, c_i]
    } else {
      y = y[, c_i:c_f]
      
      x[, paste0(portion, 'th_subset_median')] = apply(y, 1, median)
    }
    
    c_i = ceiling(ct_port*portion)
    # y = y[, :]
  }
  return(x)
}

norm_counts_con_3 = norm_counts_con_2 %>% 
  subsample_median(num_var = con_length, portions = 10)
norm_counts_treat_3 = norm_counts_treat_2 %>% 
  subsample_median(num_var = treat_length, portions = 10)

# Count NA observations ---------------------------------------------------
nacount_time = Sys.time()
print(onetenth_time - nacount_time)


norm_counts_con_4 = norm_counts_con_3
norm_counts_treat_4 = norm_counts_treat_3

zeroCount <- function(x, na.rm = T) {
  if (na.rm) {
    x = na.omit(x)
  }
  y = x == 0
  y = sum(y)
  return(y)
}

norm_counts_con_4$N_nonexpressed_samples = 
  norm_counts_con %>% 
  apply(1, zeroCount)
norm_counts_con_4$Proportion_nonexpressed_samples = 
  norm_counts_con_4$N_nonexpressed_samples / con_length

norm_counts_treat_4$N_nonexpressed_samples = 
  norm_counts_treat %>% 
  apply(1, zeroCount)
norm_counts_treat_4$Proportion_nonexpressed_samples = 
  norm_counts_treat_4$N_nonexpressed_samples / treat_length

# Count outliers ----------------------------------------------------------
outliercount_time = Sys.time()
print(nacount_time - outliercount_time)
# This section takes an hour to run

countOutliers <- function(x, na.rm = T, ...) {
  y = outlier(x, na.rm = na.rm, ...)
  y = length(y)
  return(y)
}

norm_counts_con_4$N_outliers = norm_counts_con_4[, colnames_con] %>% 
  apply(1, countOutliers)
norm_counts_con_4$Proportion_outliers = 
  norm_counts_con_4$N_outliers / con_length
norm_counts_con_4$N_extreme_outliers = norm_counts_con_4[, colnames_con] %>% 
  apply(1, countOutliers, onlyExtreme = T)
norm_counts_con_4$Proportion_extreme_outliers = 
  norm_counts_con_4$N_extreme_outliers / con_length
norm_counts_con_4$N_mild_outliers = 
  norm_counts_con_4$N_outliers - norm_counts_con_4$N_extreme_outliers
norm_counts_con_4$Proportion_mild_outliers = 
  norm_counts_con_4$N_mild_outliers / con_length

norm_counts_treat_4$N_outliers = norm_counts_treat_4[, colnames_treat] %>% 
  apply(1, countOutliers)
norm_counts_treat_4$Proportion_outliers = 
  norm_counts_treat_4$N_outliers / treat_length
norm_counts_treat_4$N_extreme_outliers = norm_counts_treat_4[, colnames_treat] %>% 
  apply(1, countOutliers, onlyExtreme = T)
norm_counts_treat_4$Proportion_extreme_outliers = 
  norm_counts_treat_4$N_extreme_outliers / treat_length
norm_counts_treat_4$N_mild_outliers = 
  norm_counts_treat_4$N_outliers - norm_counts_treat_4$N_extreme_outliers
norm_counts_treat_4$Proportion_mild_outliers = 
  norm_counts_treat_4$N_mild_outliers / con_length


colnames(norm_counts_con_4)[!colnames(norm_counts_con_4) %in% colnames_con] = 
  paste0(colnames(norm_counts_con_4)[!colnames(norm_counts_con_4) %in% colnames_con], '_', 'ConDMSO')
colnames(norm_counts_treat_4)[!colnames(norm_counts_treat_4) %in% colnames_treat] = 
  paste0(colnames(norm_counts_treat_4)[!colnames(norm_counts_treat_4) %in% colnames_treat], '_', 'APA_The')

stopifnot(identical(rownames(norm_counts_con_4), rownames(norm_counts_treat_4)))

norm_counts_4 = norm_counts_con_4 %>% 
  cbind.data.frame(norm_counts_treat_4)


# raw_counts <- DESeq2::counts(object = dds, normalized = F)
# cpm_counts = 
norm_counts_features = norm_counts_4[, !grepl('ConDMSO_|APA_The_', 
                                                colnames(norm_counts_4))]

# dupl_cols = norm_counts_features %>% t %>% duplicated
# norm_counts_features = norm_counts_features[, !dupl_cols]


feature_names = colnames(norm_counts_features) %>% 
  subset(., grepl('_ConDMSO', .)) %>% 
  gsub(pattern = '_ConDMSO', replacement = '')

for (feature_name in feature_names) {
  norm_counts_feature = 
    norm_counts_features[, grepl(pattern = feature_name, 
                                 x = colnames(norm_counts_features))]
  
  stopifnot(ncol(norm_counts_feature) == 2)
  
  pseudocount = min(norm_counts_feature[norm_counts_feature > 0], na.rm = T) * 0.1
  
  norm_counts_feature = norm_counts_feature + pseudocount
  
  fold_change_feature = norm_counts_feature[, 2] / norm_counts_feature[, 1]
  
  norm_counts_features[, paste0('foldchange_', feature_name)] = 
    fold_change_feature
}

# Third Quartile Rule -----------------------------------------------------
quantdiff_time = Sys.time()
print(outliercount_time - quantdiff_time)


# onequartilediff_rule = 
#   norm_counts_features['quantile_50%_APA_The'] > norm_counts_features['quantile_75%_ConDMSO'] | norm_counts_features['quantile_50%_APA_The'] < norm_counts_features['quantile_25%_ConDMSO'] | norm_counts_features['quantile_50%_ConDMSO'] > norm_counts_features['quantile_75%_APA_The'] | norm_counts_features['quantile_50%_ConDMSO'] < norm_counts_features['quantile_25%_APA_The']
# 
# colnames(onequartilediff_rule) = 'onequartilediff_rule'
# 
# norm_counts_features$onequartilediff_rule = onequartilediff_rule

q1belmin = norm_counts_features['quantile_25%_ConDMSO'] < norm_counts_features['quantile_0%_APA_The']
q2belq1 = norm_counts_features['quantile_50%_ConDMSO'] < norm_counts_features['quantile_25%_APA_The']
q3belq2 = norm_counts_features['quantile_75%_ConDMSO'] < norm_counts_features['quantile_50%_APA_The']
maxbelq3 = norm_counts_features['quantile_100%_ConDMSO'] < norm_counts_features['quantile_75%_APA_The']
minavoq1 = norm_counts_features['quantile_0%_ConDMSO'] > norm_counts_features['quantile_25%_APA_The']
q1avoq2 = norm_counts_features['quantile_25%_ConDMSO'] > norm_counts_features['quantile_50%_APA_The']
q2avoq3 = norm_counts_features['quantile_50%_ConDMSO'] > norm_counts_features['quantile_75%_APA_The']
q3avomax = norm_counts_features['quantile_75%_ConDMSO'] > norm_counts_features['quantile_100%_APA_The']

norm_counts_features$q1belmin = q1belmin
norm_counts_features$q2belq1 = q2belq1
norm_counts_features$q3belq2 = q3belq2
norm_counts_features$maxbelq3 = maxbelq3
norm_counts_features$minavoq1 = minavoq1
norm_counts_features$q1avoq2 = q1avoq2
norm_counts_features$q2avoq3 = q2avoq3
norm_counts_features$q3avomax = q3avomax

onequartilediff_rule = as.logical(q1belmin + q2belq1 + q3belq2 + maxbelq3 + 
                                    minavoq1 + q1avoq2 + q2avoq3 + q3avomax)

norm_counts_features$onequartilediff_rule = onequartilediff_rule



q2belmin = norm_counts_features['quantile_50%_ConDMSO'] < norm_counts_features['quantile_0%_APA_The']
q3belq1 = norm_counts_features['quantile_75%_ConDMSO'] < norm_counts_features['quantile_25%_APA_The']
maxbelq2 = norm_counts_features['quantile_100%_ConDMSO'] < norm_counts_features['quantile_50%_APA_The']
minavoq2 = norm_counts_features['quantile_0%_ConDMSO'] > norm_counts_features['quantile_50%_APA_The']
q1avoq3 = norm_counts_features['quantile_25%_ConDMSO'] > norm_counts_features['quantile_75%_APA_The']
q2avomax = norm_counts_features['quantile_50%_ConDMSO'] > norm_counts_features['quantile_100%_APA_The']

norm_counts_features$q2belmin = q2belmin
norm_counts_features$q3belq1 = q3belq1
norm_counts_features$maxbelq2 = maxbelq2
norm_counts_features$minavoq2 = minavoq2
norm_counts_features$q1avoq3 = q1avoq3
norm_counts_features$q2avomax = q2avomax

twoquartilediff_rule = as.logical(q2belmin + q3belq1 + maxbelq2 + 
                                    minavoq2 + q1avoq3 + q2avomax)

norm_counts_features$twoquartilediff_rule = twoquartilediff_rule



q3belmin = norm_counts_features['quantile_75%_ConDMSO'] < norm_counts_features['quantile_0%_APA_The']
maxbelq1 = norm_counts_features['quantile_100%_ConDMSO'] < norm_counts_features['quantile_25%_APA_The']
minavoq3 = norm_counts_features['quantile_0%_ConDMSO'] > norm_counts_features['quantile_75%_APA_The']
q1avomax = norm_counts_features['quantile_25%_ConDMSO'] > norm_counts_features['quantile_100%_APA_The']

norm_counts_features$q3belmin = q3belmin
norm_counts_features$maxbelq1 = maxbelq1
norm_counts_features$minavoq3 = minavoq3
norm_counts_features$q1avomax = q1avomax

threequartilediff_rule = as.logical(q3belmin + maxbelq1 + 
                                      minavoq3 + q1avomax)

norm_counts_features$threequartilediff_rule = threequartilediff_rule



maxbelmin = norm_counts_features['quantile_100%_ConDMSO'] < norm_counts_features['quantile_0%_APA_The']
minavomax = norm_counts_features['quantile_0%_ConDMSO'] > norm_counts_features['quantile_100%_APA_The']

norm_counts_features$maxbelmin = maxbelmin
norm_counts_features$minavomax = minavomax

fourquartilediff_rule = as.logical(maxbelmin +
                                     minavomax)

norm_counts_features$fourquartilediff_rule = fourquartilediff_rule



quartilediff_score = (q1belmin + q2belq1 + q3belq2 + maxbelq3 + 
  q2belmin + q3belq1 + maxbelq2 + 
  q3belmin + maxbelq1 + 
  maxbelmin) -
  (minavoq1 + q1avoq2 + q2avoq3 + q3avomax +
     minavoq2 + q1avoq3 + q2avomax + 
     minavoq3 + q1avomax + 
     minavomax)

norm_counts_features$quartilediff_score = quartilediff_score


# thirdfirstquartile_rule = 
#   norm_counts_features['quantile_25%_APA_The'] > norm_counts_features['quantile_75%_ConDMSO'] | norm_counts_features['quantile_75%_APA_The'] < norm_counts_features['quantile_25%_ConDMSO']
# 
# colnames(thirdfirstquartile_rule) = 'thirdfirstquartile_rule'
# 
# norm_counts_features$thirdfirstquartile_rule = thirdfirstquartile_rule
# 
# maxmin_rule = 
#   norm_counts_features['quantile_0%_APA_The'] > norm_counts_features['quantile_100%_ConDMSO'] | norm_counts_features['quantile_100%_APA_The'] < norm_counts_features['quantile_0%_ConDMSO']
# 
# colnames(maxmin_rule) = 'maxmin_rule'
# 
# norm_counts_features$maxmin_rule = maxmin_rule


stopifnot(identical(rownames(norm_counts_features), rownames(cpm_feature)))

norm_counts_features = cbind.data.frame(norm_counts_features, cpm_feature)

# Spurious Spike Filter ---------------------------------------------------
spuspi_time = Sys.time()
print(quantdiff_time - spuspi_time)


spur_spike_filter = function(x) {
  is.spike = function(x) {
    n = length(x)
    spike_thresh = 1.4*(n)^(-0.66) 
    y = x > (spike_thresh*sum(x))
    return(y)
  }
  y = apply(x, 1, is.spike)
  y = t(y)
  y = apply(y, 1, any)
  return(y)
}

sprs_spks_con = norm_counts_con %>% spur_spike_filter
sprs_spks_treat = norm_counts_treat %>% spur_spike_filter

norm_counts_features$spurious_spikes = sprs_spks_con | sprs_spks_treat

# Save features -----------------------------------------------------------
end_time = Sys.time()
print(spuspi_time - end_time)


norm_counts_features %>% saveRDS(norm_counts_features_path)


# norm_counts_treat_4 = norm_counts_treat_4[, !grepl('nonexpressed', colnames(norm_counts_treat_4))]

# colnames(norm_counts_con_4) =
#   colnames(norm_counts_con_4) %>%
#   gsub('_ConDMSO', '', .)
# colnames(norm_counts_treat_4) =
#   colnames(norm_counts_treat_4) %>%
#   gsub('_APA_The', '', .)

