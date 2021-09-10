manual_degs_2 = res %>% 
  as.data.frame() %>% 
  rownames_to_column('ensembl_gene_id') %>% 
  merge.data.frame(x = ., y = manual_degs, by = 'ensembl_gene_id', all.y = T)

norm_counts_2 = norm_counts %>% 
  as.data.frame() %>% 
  na.omit()

colnames_con = subset(x = colnames(norm_counts), 
                        grepl('ConDMSO', colnames(norm_counts)))
colnames_treat = subset(x = colnames(norm_counts), 
                        grepl('APA_The', colnames(norm_counts)))

norm_counts_con = norm_counts_2[, grep('ConDMSO', colnames(norm_counts_2))]
con_length = ncol(norm_counts_con)
norm_counts_treat = norm_counts_2[, grep('APA_The', colnames(norm_counts_2))]
treat_length = ncol(norm_counts_treat)


apply_list_fns <- function(x, fns = c('max', 'min', 'median', 'mean', 'sd', 'var'), quantiles = NULL) {
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
  apply_list_fns(fns = c('max', 'min', 'median', 'mean', 'sd', 'var', 'quantile'), 
                 quantiles = c(seq(0.1, 0.9, 0.1), 0.25, 0.75))
norm_counts_treat_2 = norm_counts_treat %>% 
  apply_list_fns(fns = c('max', 'min', 'median', 'mean', 'sd', 'var', 'quantile'), 
                 quantiles = c(seq(0.1, 0.9, 0.1), 0.25, 0.75))


subsample_median <- function(x, num_var, portions) {
  prev_port = 0
  c_i = 1
  ct_port = num_var/portions
  for (portion in 1:portions) {
    y = x[, 1:num_var]
    
    c_f = floor(ct_port*portion)
    y = y[, c_i:c_f]
    
    x[, paste0(portion, 'th_subset_median')] = apply(y, 1, median)
    
    c_i = ceiling(ct_port*portion)
    # y = y[, :]
  }
  return(x)
}

norm_counts_con_3 = norm_counts_con_2 %>% 
  subsample_median(num_var = con_length, portions = 10)
norm_counts_treat_3 = norm_counts_treat_2 %>% 
  subsample_median(num_var = con_length, portions = 10)

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
  norm_counts_con_4[, grep('ConDMSO', colnames(norm_counts_con_4))] %>% 
  apply(1, zeroCount)
norm_counts_con_4$Proportion_nonexpressed_samples = 
  norm_counts_con_4$N_nonexpressed_samples / con_length

norm_counts_treat_4$N_nonexpressed_samples = 
  norm_counts_treat_4[, grep('treatDMSO', colnames(norm_counts_treat_4))] %>% 
  apply(1, zeroCount)
norm_counts_treat_4$Proportion_nonexpressed_samples = 
  norm_counts_treat_4$N_nonexpressed_samples / treat_length

countOutliers <- function(x, na.rm = T, ...) {
  y = outlier(x, na.rm = na.rm, ...)
  y = length(y)
  return(y)
}

norm_counts_con_4$N_outliers = norm_counts_con_4[, colnames_con] %>% 
  apply(1, countOutliers)
norm_counts_con_4$Proportion_outliers = 
  norm_counts_con_4$N_outliers / con_length

norm_counts_treat_4$N_outliers = norm_counts_treat_4[, colnames_treat] %>% 
  apply(1, countOutliers)
norm_counts_treat_4$Proportion_outliers = 
  norm_counts_treat_4$N_outliers / treat_length
