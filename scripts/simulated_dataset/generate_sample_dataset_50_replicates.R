source('scripts/functions/functions_JOA.R')
forceLibrary(c('tidyverse', 'magrittr', 'tibble'))


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if (!require("SPsimSeq", quietly = TRUE))
  BiocManager::install("SPsimSeq")

library(SPsimSeq)
library(tidyverse)

# load the Zhang bulk RNA-seq data (availabl with the package) 
data("zhang.data.sub") 
# filter genes with sufficient expression (important step to avoid bugs) 
zhang.counts <- zhang.data.sub$counts 
MYCN.status  <- zhang.data.sub$MYCN.status #The grouping variable

# set.seed(6452) #Set seed for reproducibility
# simulate data
sim.data.bulk <- SPsimSeq(n.sim = 1, s.data = zhang.counts,
                          group = MYCN.status, n.genes = 3000, batch.config = 1,
                          group.config = c(0.5, 0.5), tot.samples = ncol(zhang.counts), 
                          pDE = 0.1, lfc.thrld = 0.5, result.format = "list", return.details = TRUE)


sim.data.bulk1 <- sim.data.bulk$sim.data.list[[1]]
head(sim.data.bulk1$counts[, seq_len(5)])  # count data
head(sim.data.bulk1$colData)        # sample info
head(sim.data.bulk1$rowData)        # gene info

counts = sim.data.bulk1$counts
coldata = sim.data.bulk1$colData        # sample info
rowdata = sim.data.bulk1$rowData

coldata$Group = coldata$Group %>% 
  as.integer %>% 
  as.logical()
coldata_group_logi = coldata
# coldata_group_logi = coldata_group_logi[c(which(!coldata_group_logi$Group),
#                                           which(coldata_group_logi$Group)), ]
coldata_group = coldata_group_logi
coldata_group$Group[coldata_group_logi$Group] = 'MYCN_amplified'
coldata_group$Group[!coldata_group_logi$Group] = 'MYCN_non-amplified'

coldata = coldata_group 

# Note: In order to benefit from the default settings of the package, you should 
# put the variable of interest at the end of the formula and make sure the 
# control level is the first level.
coldata$Group = factor(coldata$Group, levels = unique(coldata$Group)[2:1])
coldata$Group %>% levels() %>% .[1]

if (!dir.exists('data/example_files')) {
  dir.create('data/example_files', recursive = T)
}

counts %>% write.csv('data/example_files/counts.csv')
coldata %>% write.csv('data/example_files/coldata.csv')


source('scripts/simulated_dataset/deseq2.R')

norm_counts = DESeq2::counts(object = dds, normalized = T)
res <- results(dds)

norm_counts %>% write.csv('data/example_files/norm_counts.csv')
res %>% write.csv('data/example_files/res.csv')

