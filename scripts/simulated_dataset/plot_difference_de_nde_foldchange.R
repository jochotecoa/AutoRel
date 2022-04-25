source('scripts/simulated_dataset/spsimseq.R')

counts_de = counts[rowdata$DE.ind, ]

fcs_de = rowMeans(counts_de[, 1:50]) / rowMeans(counts_de[, 51:100]) 
fcs_de[is.infinite(fcs_de)] = fcs_de[!is.infinite(fcs_de)] %>% max(na.rm = T)
fcs_de[is.na(fcs_de)] = 1

counts_nde = counts[!rowdata$DE.ind, ]

fcs_nde = rowMeans(counts_nde[, 1:50]) / rowMeans(counts_nde[, 51:100])
fcs_nde[is.infinite(fcs_nde)] = fcs_nde[!is.infinite(fcs_nde)] %>% max(na.rm = T)
fcs_nde[is.na(fcs_nde)] = 1

fcs_bxplot = data.frame(fold_change = fcs_de, de = 'yes') %>% 
  rbind(data.frame(fold_change = fcs_nde, de = 'no'))

boxplot(log2(fold_change) ~ de, fcs_bxplot)
dev.off()

table(abs(log2(fcs_de)) < 0.5)


boxplot(abs(log2(fold_change)) ~ de, fcs_bxplot)
abline(a = 0.5, b = 0)
dev.off()

