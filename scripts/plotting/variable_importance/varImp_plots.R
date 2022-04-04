
# 3vs3 --------------------------------------------------------------------


source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'magrittr'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3vs3'
conf_matr_path = 'output/confusion_matrices/apap_3vs3'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))

varimp_3 = model_treebag %>% 
  varImp 
varimp_3$importance %<>% .[order(varimp_3$importance$Overall, decreasing = T),, F] %>% 
  .[1:20,,F]


plot_varimp_3 = varimp_3 %>%  
  plot()

# Above is written with a 'b', not 'avove' 
plot_varimp_3$y.limits %<>% 
  gsub(pattern = 'avo', replacement = 'abo')
y.limits = plot_varimp_3$y.limits %>% 
  .[(length(.)-19):length(.)]
plot_varimp_3$y.limits %<>%  .[1:20]

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


varimp_9 = model_sparseLDA %>% 
  varImp 
varimp_9$importance %<>% .[order(apply(varimp_9$importance, 1, max), decreasing = T),, F] %>% 
  .[1:20,,F]


plot_varimp_9 = varimp_9 %>%  
  plot()

# Above is written with a 'b', not 'avove' 
plot_varimp_9$y.limits %<>% 
  gsub(pattern = 'avo', replacement = 'abo')
y.limits = plot_varimp_9$y.limits %>% 
  .[(length(.)-19):length(.)]
plot_varimp_9$y.limits %<>%  .[1:20]

if (!dir.exists('output/plots/variable_importance/')) {
  dir.create('output/plots/variable_importance/', recursive = T)
}


tiff("output/plots/variable_importance/sparseLDA_9vs9.tiff", units="in", width=10, height=10, res=300)#width=10, height=20, res=300)
par(mar=c(4,14,4,8))

plot_varimp_9 %>% plot()

dev.off()



# 21vs21 ------------------------------------------------------------------

source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN', 'magrittr'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_21vs21'
conf_matr_path = 'output/confusion_matrices/apap_21vs21'

model_rf = readRDS(paste0(train_mod_path, '/rf/original.rds'))


varimp_21 = model_rf %>% 
  varImp 
varimp_21$importance %<>% .[order(varimp_21$importance$Overall, decreasing = T),, F] %>% 
  .[1:20,,F]


plot_varimp_21 = varimp_21 %>% 
  plot()

# Above is written with a 'b', not 'avove' 
plot_varimp_21$y.limits %<>% 
  gsub(pattern = 'avo', replacement = 'abo')
y.limits = plot_varimp_21$y.limits %>% 
  .[(length(.)-19):length(.)]
plot_varimp_21$y.limits %<>%  .[1:20]

if (!dir.exists('output/plots/variable_importance/')) {
  dir.create('output/plots/variable_importance/', recursive = T)
}


tiff("output/plots/variable_importance/sparseLDA_21vs21.tiff", units="in", width=10, height=10, res=300)
par(mar=c(4,14,4,8))

plot_varimp_21 %>% plot()

dev.off()



# 3_9_21 ------------------------------------------------------------------

source('scripts/functions/functions_JOA.R')
forceLibrary(c('mlbench', 'caret', 'doMC', 'dplyr', 'RANN'))

train_mod_path = '/ngs-data-2/analysis/juan/autosign/trained_models/apap_3_9_21'
conf_matr_path = 'output/confusion_matrices/apap_3_9_21'

model_treebag = readRDS(paste0(train_mod_path, '/treebag/original.rds'))


varimp_3_9_21 = model_treebag %>% 
  varImp 
varimp_3_9_21$importance %<>% .[order(varimp_3_9_21$importance$Overall, decreasing = T),, F] %>% 
  .[1:20,,F]


plot_varimp_3_9_21 = varimp_3_9_21 %>% 
  plot()

# Above is written with a 'b', not 'avove' 
plot_varimp_3_9_21$y.limits %<>% 
  gsub(pattern = 'avo', replacement = 'abo')
y.limits = plot_varimp_3_9_21$y.limits %>% 
  .[(length(.)-19):length(.)]
plot_varimp_3_9_21$y.limits %<>%  .[1:20]

if (!dir.exists('output/plots/variable_importance/')) {
  dir.create('output/plots/variable_importance/', recursive = T)
}


tiff("output/plots/variable_importance/sparseLDA_3_9_21vs3_9_21.tiff", units="in", width=10, height=10, res=300)
par(mar=c(4,14,4,8))

plot_varimp_3_9_21 %>% plot()

dev.off()
