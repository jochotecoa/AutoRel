source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21'

model_treebag = readRDS(paste0(train_mod_path, '/significant_labels/treebag/original.rds'))


dataset_21_path = 'data/apap_hecatos/deseq2_features_all_21vs21.rds' 
dataset_21 = dataset_21_path %>% readRDS()

colnames(dataset_21) = colnames(dataset_21) %>% 
  make.names()

# Convert all logical variables to numerical
dataset_21[sapply(dataset_21, is.logical)] = 
  dataset_21[sapply(dataset_21, is.logical)] %>% 
  sapply(as.numeric)

table(model_treebag$coefnames %in% colnames(dataset_21))



pred_21 <- data.frame(pred = predict(model_treebag, newdata = dataset_21))

# obsVSpred$pred = obsVSpred[, 2] %>% as.factor()

dataset_21 = dataset_21 %>% 
  cbind.data.frame(pred_21)

dataset_21$fdr_0.05 = dataset_21$padj < 0.05
dataset_21$fdr_0.05_l2fc_1.5 = (dataset_21$padj < 0.05) & (abs(dataset_21$log2FoldChange) > 1.5)
dataset_21$fdr_0.01 = dataset_21$padj < 0.01
dataset_21$fdr_0.01_l2fc_1.5 = (dataset_21$padj < 0.01) & (abs(dataset_21$log2FoldChange) > 1.5)

r_odaf_degs = 'data/r_odaf/Output/R-ODAF_APAP_21_DESeq2_RNA-Seq_APA_vs_ConDMSO_FDR_0.01_DEG_table.txt' %>% 
  read.table() %>% 
  row.names()

dataset_21$r_odaf = dataset_21$ensembl_gene_id %in% r_odaf_degs

dataset_21$relevant = dataset_21$pred == 'significant'


library(UpSetR)
degs_df = dataset_21[, c('fdr_0.05', 'fdr_0.05_l2fc_1.5', 'fdr_0.01', 
                         'fdr_0.01_l2fc_1.5', 'r_odaf', 'relevant')]
rownames(degs_df) = dataset_21$ensembl_gene_id

fdr_0.05 = rownames(degs_df)[degs_df[,'fdr_0.05']]
relevant = rownames(degs_df)[degs_df[,'relevant']]

library(biomaRt)

listMarts()

mart = useMart(biomart = 'ENSEMBL_MART_ENSEMBL', version = 'Ensembl Genes 106')

listDatasets(mart)

mart = useMart(biomart = 'ENSEMBL_MART_ENSEMBL', version = 'Ensembl Genes 106', 
               dataset = 'hsapiens_gene_ensembl')

View(listAttributes(mart)) 
  .[,2] %>% 
  subset(., grepl(pattern = 'ncbi', x = .)) %>% 
  head

fdr_0.05_names = getBM(filters = 'ensembl_gene_id', 
                       attributes = c("entrezgene_id"),
                       values = fdr_0.05, 
                       mart = mart) %>% 
  unique()

relevant_names = getBM(filters = 'ensembl_gene_id', 
                       attributes = c("entrezgene_id"),
                       values = relevant, 
                       mart = mart) %>% 
  unique()

if (!dir.exists('output/gene_lists/21vs21/')) {
  dir.create('output/gene_lists/21vs21/', recursive = T)
}

write.table(x = fdr_0.05, 
            file = 'output/gene_lists/21vs21/FDR_0.05.txt', 
            quote = F, row.names = F, col.names = F)
write.table(x = relevant, 
            file = 'output/gene_lists/21vs21/relevant.txt', 
            quote = F, row.names = F, col.names = F)


write.table(x = fdr_0.05_names, 
            file = 'output/gene_lists/21vs21/FDR_0.05_entrezgene_id.txt', 
            quote = F, row.names = F, col.names = F)
write.table(x = relevant_names, 
            file = 'output/gene_lists/21vs21/relevant_entrezgene_id.txt', 
            quote = F, row.names = F, col.names = F)
