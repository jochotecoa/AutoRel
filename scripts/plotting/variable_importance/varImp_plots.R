
# 3vs3 --------------------------------------------------------------------


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



# 9vs9 --------------------------------------------------------------------

source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_9vs9'
conf_matr_path = 'output/confusion_matrices/apap_9vs9'

model_sparseLDA = readRDS(paste0(train_mod_path, '/sparseLDA/original.rds'))


plot_varimp_9 = model_sparseLDA %>% 
  varImp %>% 
  plot()

# Above is written with a 'b', not 'avove' 
plot_varimp_9$y.limits %<>% 
  gsub(pattern = 'avo', replacement = 'abo')

if (!dir.exists('output/plots/variable_importance/')) {
  dir.create('output/plots/variable_importance/', recursive = T)
}


tiff("output/plots/variable_importance/sparseLDA_9vs9.tiff", units="in", width=10, height=20, res=300)
par(mar=c(4,14,4,8))

plot_varimp_9 %>% plot()

dev.off()



# 21vs21 ------------------------------------------------------------------

source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'magrittr'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_21vs21'
conf_matr_path = 'output/confusion_matrices/apap_21vs21'

model_rf = readRDS(paste0(train_mod_path, '/rf/original.rds'))

plot_varimp_21 = model_rf %>% 
  varImp %>% 
  plot()

# Above is written with a 'b', not 'avove' 
plot_varimp_21$y.limits %<>% 
  gsub(pattern = 'avo', replacement = 'abo')

if (!dir.exists('output/plots/variable_importance/')) {
  dir.create('output/plots/variable_importance/', recursive = T)
}


tiff("output/plots/variable_importance/sparseLDA_21vs21.tiff", units="in", width=10, height=10, res=300)
par(mar=c(4,14,4,8))

plot_varimp_21 %>% plot()

dev.off()

