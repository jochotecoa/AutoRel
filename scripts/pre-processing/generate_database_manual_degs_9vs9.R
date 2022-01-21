

# Putting final results toguether -----------------------------------------

deseq2_features_all = readRDS(file = 'data/apap_hecatos/deseq2_features_all_9vs9.rds')


# manual_annot = read.csv('data/apap_hecatos_manual_significant.csv')
# manual_annot$significance = gsub(pattern = 'T', replacement = 'significant', x = manual_annot$significance)
# manual_annot$significance = gsub(pattern = 'F', replacement = 'nonsignificant', x = manual_annot$significance)
# 
# if (ncol(manual_annot) > 2) {
#   manual_annot = manual_annot[, 1:2]
# }
# 
# manual_annotv2 = read.csv('data/apap_hecatos/predicted_manually_curated_genes_13012022.csv', header = F)
# names(manual_annotv2) = names(manual_annot)
# manual_annot = rbind(manual_annot, manual_annotv2)
# 
# # man_ann_res = subset.data.frame(x = res2, subset = rownames(res2) %in% manual_annot$ensembl_gene_id) %>% 
# #   as.data.frame()
# 
# manual_annot_2 = deseq2_features_all %>% 
#   merge.data.frame(x = manual_annot, y = ., by = 'ensembl_gene_id') %>% 
#   column_to_rownames('ensembl_gene_id')
# 
# manual_annot_2 = manual_annot_2[manual_annot$ensembl_gene_id, ]

# # Padj > 0.2 --------------------------------------------------------------
# 
# 
# big_padj_nondegs = deseq2_features_all %>% 
#   column_to_rownames('ensembl_gene_id') %>% 
#   dplyr::filter(padj >= 0.2) %>% 
#   rownames()
# 
# # 0.2 < Padj >= 0.1; pvalue > 0.05 ----------------------------------------
# 
# non_degs_0.1 = deseq2_features_all %>% 
#   column_to_rownames('ensembl_gene_id') %>% 
#   dplyr::filter(padj >= 0.1, padj < 0.2, pvalue > 0.05)
# 
# non_degs_0.1 = non_degs_0.1[order(non_degs_0.1$padj), ]
# 
# which(rownames(non_degs_0.1) %in% rownames(manual_annot_2))
# non_degs_0.1 = non_degs_0.1[1:grep("ENSG00000004487", rownames(non_degs_0.1)), ] %>% 
#   row.names()
# 
# # (0.2 < padj >= 0.5) & pvalue < 0.05 -------------------------------------
# 
# 
# non_degs_0.1_0.05 = deseq2_features_all %>% 
#   column_to_rownames('ensembl_gene_id') %>% 
#   dplyr::filter(padj >= 0.1, padj < 0.2, pvalue < 0.05) 
# non_degs_0.1_0.05 = non_degs_0.1_0.05[order(non_degs_0.1_0.05$padj), ]
# 
# which(rownames(non_degs_0.1_0.05) %in% rownames(manual_annot_2))
# non_degs_0.1_0.05 = non_degs_0.1_0.05 %>% 
#   row.names()
# 
# # padj >= 0.05, padj < 0.1 ------------------------------------------------
# 
# 
# non_degs_0.05 = deseq2_features_all %>% 
#   column_to_rownames('ensembl_gene_id') %>% 
#   dplyr::filter(padj >= 0.05, padj < 0.1) 
# non_degs_0.05 = non_degs_0.05[order(non_degs_0.05$padj), ]
# 
# # which(rownames(non_degs_0.05) %in% rownames(manual_annot_2))
# non_degs_0.05 = non_degs_0.05[1:grep("ENSG00000138606", rownames(non_degs_0.05)), ] %>% 
#   row.names()
# 
# # padj < 0.05 -------------------------------------------------------------
# 
# 
# degs_0.05 = deseq2_features_all %>% 
#   column_to_rownames('ensembl_gene_id') %>%  
#   dplyr::filter(padj < 0.05) 
# degs_0.05 = degs_0.05[order(degs_0.05$padj), ]
# 
# # which(rownames(degs_0.05) %in% rownames(manual_annot_2))
# degs_0.05 = degs_0.05[1:grep("ENSG00000134115", rownames(degs_0.05)), ] %>% 
#   row.names()
# 
# padj == NA --------------------------------------------------------------


na_non_degs = deseq2_features_all[is.na(deseq2_features_all$padj),] %>% 
  rownames()

# Quantile difference rules -----------------------------------------------


fourqd = deseq2_features_all %>% 
  column_to_rownames('ensembl_gene_id') %>%  
  dplyr::filter(fourquartilediff_rule == T) %>% 
  row.names()

oneqd = deseq2_features_all %>% 
  column_to_rownames('ensembl_gene_id') %>%  
  dplyr::filter(onequartilediff_rule == F) %>% 
  row.names()



# CPM rule ----------------------------------------------------------------

cpmrule = deseq2_features_all %>% 
  column_to_rownames('ensembl_gene_id') %>%  
  dplyr::filter(rule_cpm_0.75_above_1 == F) %>% 
  row.names()


# Combine together --------------------------------------------------------


manual_degs = data.frame(ensembl_gene_id = deseq2_features_all$ensembl_gene_id, 
                         significance = NA)

manual_degs[manual_degs$ensembl_gene_id %in% oneqd, 'significance'] = 'nonsignificant' 
manual_degs[manual_degs$ensembl_gene_id %in% fourqd, 'significance'] = 'significant' 
manual_degs[manual_degs$ensembl_gene_id %in% na_non_degs, 'significance'] = 'nonsignificant'
manual_degs[manual_degs$ensembl_gene_id %in% cpmrule, 'significance'] = 'nonsignificant'


# old_nrow = nrow(manual_degs)
# manual_degs = manual_degs %>% 
#   merge.data.frame(y = manual_annot, by = 'ensembl_gene_id', all = T)
# stopifnot(old_nrow == nrow(manual_degs))

# manual_degs[!is.na(manual_degs$significance.y), 'significance.x'] = 
#   manual_degs[!is.na(manual_degs$significance.y), 'significance.y']
# 
# manual_degs[is.na(manual_degs$significance.y), 'significance.y'] = 
#   manual_degs[is.na(manual_degs$significance.y), 'significance.x']
# 
# stopifnot(all.equal(manual_degs[, 'significance.x'], 
#                     manual_degs[, 'significance.y']))
# 
# colnames(manual_degs)[grep('significance.x', colnames(manual_degs))] = 
#   'significance'
# 
# manual_degs = manual_degs[, -grep('significance.y', colnames(manual_degs))] %>% 
#   na.omit()

stopifnot(ncol(manual_degs) == 2)
manual_degs = manual_degs %>% 
  na.omit()
# manual_degs2 = read.csv('data/apap_hecatos/predicted_manually_curated_genes_12102021.csv')
# saveRDS(object = manual_annot, file = 'data/apap_hecatos/manual_annot_apap_hecatos.rds')
saveRDS(object = manual_degs, file = 'data/apap_hecatos/manual_degs_apap_hecatos_9vs9.rds')

