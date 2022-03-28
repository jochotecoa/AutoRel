source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'magrittr'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3vs3'
conf_matr_path = 'output/confusion_matrices/apap_3vs3'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))

plot_varimp_3 = model_treebag %>% 
  varImp %>% 
  plot()

# Above is written with a 'b', not 'avove' 
plot_varimp_3$y.limits %<>% 
  gsub(pattern = 'avo', replacement = 'abo')

if (!dir.exists('output/plots/variable_importance/')) {
  dir.create('output/plots/variable_importance/', recursive = T)
}


tiff("output/plots/variable_importance/treebag_3vs3.tiff", units="in", width=10, height=10, res=300)
par(mar=c(4,14,4,8))

plot_varimp_3 %>% plot()

dev.off()
