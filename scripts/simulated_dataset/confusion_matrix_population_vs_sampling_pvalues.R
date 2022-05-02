res %<>% as.data.frame() %>% 
  rownames_to_column()
rowdata %<>% rownames_to_column()

res_rowdata = merge.data.frame(res, rowdata, 'rowname')

res_rowdata$significant = res_rowdata$padj < 0.05 
res_rowdata$significant %<>% as.factor()
res_rowdata$DE.ind %<>% as.factor()

if (levels(res_rowdata$DE.ind) != levels(res_rowdata$significant)) {
  warning('No significant genes in iteration ', i)
  levels(res_rowdata$significant) = levels(res_rowdata$DE.ind)
}


conf_matr_sign = confusionMatrix(res_rowdata$DE.ind, res_rowdata$significant)
