source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


dataset_21_path = 'data/apap_hecatos/deseq2_features_all_21vs21.rds' 
dataset_21 = dataset_21_path %>% readRDS()

colnames(dataset_21) = colnames(dataset_21) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_21[sapply(dataset_21, is.logical)] = 
  dataset_21[sapply(dataset_21, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_21))

r_odaf_degs = 'data/r_odaf/Output/R-ODAF_APAP_21_DESeq2_RNA-Seq_APA_vs_ConDMSO_FDR_0.01_DEG_table.txt' %>% 
  read.table()


pred_21 <- data.frame(pred = predict(model_treebag, newdata = dataset_21))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_21 = dataset_21 %>% 
  cbind.data.frame(pred_21)

dataset_21$fdr_0.05 = dataset_21$padj < 0.05
dataset_21$fdr_0.05_l2fc_1.5 = (dataset_21$padj < 0.05) & (abs(dataset_21$log2FoldChange) > 1.5)
dataset_21$fdr_0.01 = dataset_21$padj < 0.01
dataset_21$fdr_0.01_l2fc_1.5 = (dataset_21$padj < 0.01) & (abs(dataset_21$log2FoldChange) > 1.5)


dataset_21$r_odaf = (dataset_21$rule_cpm_0.75_above_1 + 
                       (dataset_21$padj < 0.01) + 
                       abs(dataset_21$spurious_spikes - 1) + 
                       dataset_21$q1avoq2 + dataset_21$q3belq2) == 4

dataset_21$relevant = dataset_21$pred == 'significant'


library(UpSetR)
a = dataset_21[, c('fdr_0.05', 'fdr_0.05_l2fc_1.5', 'fdr_0.01', 'fdr_0.01_l2fc_1.5', 'r_odaf', 'relevant')]
a = a %>% 
  apply(2, as.integer) %>% 
  as.data.frame()

upset(a, 
      nsets = ncol(a),
      order.by='freq', 
      decreasing = T, 
      mb.ratio = c(0.6, 0.4),
      number.angles = 0, 
      text.scale = 2, 
      point.size = 2.8, 
      line.size = 1)

