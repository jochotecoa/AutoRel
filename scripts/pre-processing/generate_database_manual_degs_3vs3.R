

# Putting final results toguether -----------------------------------------

deseq2_features_all = readRDS(file = 'data/apap_hecatos/deseq2_features_all_3vs3.rds')




na_non_degs = deseq2_features_all[is.na(deseq2_features_all$padj),] %>% 
  rownames()

# Quantile difference rules -----------------------------------------------


fourqdsigncpm = deseq2_features_all %>% 
  column_to_rownames('ensembl_gene_id') %>%  
  dplyr::filter(
    `fourquartilediff_rule` == T,
    rule_cpm_0.75_above_1 == T,
    padj < 0.05
    ) %>% 
  row.names()

oneqd = deseq2_features_all %>%
  column_to_rownames('ensembl_gene_id') %>%
  dplyr::filter(onequartilediff_rule == F) %>%
  row.names()

scorezero = deseq2_features_all %>%
  column_to_rownames('ensembl_gene_id') %>%
  dplyr::filter(quartilediff_score == 0) %>%
  row.names()

# threeqd_03 = deseq2_features_all %>% 
#   column_to_rownames('ensembl_gene_id') %>%  
#   dplyr::filter(
#     `fourquartilediff_rule` == F,
#     `threequartilediff_rule` == T,
#     padj > 0.3
#   ) %>% 
#   row.names()




# CPM rule ----------------------------------------------------------------

cpmrule = deseq2_features_all %>%
  column_to_rownames('ensembl_gene_id') %>%
  dplyr::filter(rule_cpm_0.75_above_1 == F) %>%
  row.names()


# Combine together --------------------------------------------------------


manual_degs = data.frame(ensembl_gene_id = deseq2_features_all$ensembl_gene_id, 
                         significance = NA)

manual_degs[manual_degs$ensembl_gene_id %in% oneqd, 'significance'] = 'nonsignificant' 
manual_degs[manual_degs$ensembl_gene_id %in% scorezero, 'significance'] = 'nonsignificant'
manual_degs[manual_degs$ensembl_gene_id %in% fourqdsigncpm, 'significance'] = 'significant' 
manual_degs[manual_degs$ensembl_gene_id %in% na_non_degs, 'significance'] = 'nonsignificant'
manual_degs[manual_degs$ensembl_gene_id %in% cpmrule, 'significance'] = 'nonsignificant'



stopifnot(ncol(manual_degs) == 2)
manual_degs = manual_degs %>% 
  na.omit()

manual_annot = 'data/apap_hecatos/predicted_manually_curated_genes_3vs3.csv' %>% 
  read.csv(header = F)

colnames(manual_annot) = colnames(manual_degs)

manual_degs = rbind(manual_annot, manual_degs) 

manual_degs = manual_degs[!duplicated(manual_degs$ensembl_gene_id), ]


saveRDS(object = manual_degs, file = 'data/apap_hecatos/manual_degs_apap_hecatos_3vs3.rds')

