norm_counts = 'data/apap_hecatos/norm_counts_deseq2_apap_hecatos.rds' %>% readRDS
manual_res = readRDS(file = 'data/apap_hecatos/manual_res_apap_hecatos.rds')
deseq_counts = norm_counts %>% as.data.frame %>%
  rownames_to_column('ensembl_gene_id') %>% 
  merge.data.frame(y = manual_res, by = 'ensembl_gene_id', all.y = T) %>% 
  column_to_rownames('ensembl_gene_id')

deseq_counts = deseq_counts[, -c(43:48, 50)]

deseq_counts_non = deseq_counts[deseq_counts$significance == 'nonsignificant', ]
deseq_counts_sig = deseq_counts[deseq_counts$significance == 'significant', ]
deseq_counts_dub = deseq_counts[deseq_counts$significance == 'dubious', ]

my_colrs = c(
  rep('gray', 21),
  rep('pink', 21)
)

for (row_i in rownames(deseq_counts_dub)) {
  names.args = colnames(deseq_counts_non)[-ncol(deseq_counts_non)]
  deseq_counts_dub[row_i, -ncol(deseq_counts_non), F] %>% 
    as.numeric() %>% 
    barplot(las  =2, 
            col = my_colrs, 
            main = row_i, 
            names.arg = names.args)
  readline(prompt = "Press [enter] to continue")
  
}

print(deg)
readline(prompt = "Press [enter] to continue")
