library(dplyr)
library(tibble)
SampleDataFile <- "data/raw_count_hecatos_apap.txt" #This comma delimited file contains the merged RSEM.genes.results files

SampleData = read.table(SampleDataFile, header = T) %>% 
  column_to_rownames('gene_id')

SampleData = SampleData %>% 
  select(matches('APA_The|ConDMSO'))
SampleKey = data.frame(NAME = colnames(SampleData),
                       Compound = gsub('_.*', '', colnames(SampleData)))
dir.create("data/r_odaf/")
sampleDataFile = "data/r_odaf/sampleData_21.csv"
write.csv(x = SampleData, file = sampleDataFile)
SampleKeyFile <- "data/r_odaf/samplekeyTEST_21.csv" #This comma delimited file contains at least 2 columns: NAME (sample names identical to the column names of sampleData) and Compound (needs to identify to which group the sample belongs -> ExperimentalGroup & ControlGroup)
write.csv(x = SampleKey, file = SampleKeyFile)

SampleData = SampleData %>% 
  select(matches('APA_The|ConDMSO')) %>%  
  select(matches('002|008|024'))
SampleKey = data.frame(NAME = colnames(SampleData),
                       Compound = gsub('_.*', '', colnames(SampleData)))
dir.create("data/r_odaf/")
sampleDataFile = "data/r_odaf/sampleData_9.csv"
write.csv(x = SampleData, file = sampleDataFile)
SampleKeyFile <- "data/r_odaf/samplekeyTEST_9.csv" #This comma delimited file contains at least 2 columns: NAME (sample names identical to the column names of sampleData) and Compound (needs to identify to which group the sample belongs -> ExperimentalGroup & ControlGroup)
write.csv(x = SampleKey, file = SampleKeyFile)

SampleData = SampleData %>% 
  select(matches('APA_The|ConDMSO')) %>%  
  select(matches('024'))
SampleKey = data.frame(NAME = colnames(SampleData),
                       Compound = gsub('_.*', '', colnames(SampleData)))
dir.create("data/r_odaf/")
sampleDataFile = "data/r_odaf/sampleData_3.csv"
write.csv(x = SampleData, file = sampleDataFile)
SampleKeyFile <- "data/r_odaf/samplekeyTEST_3.csv" #This comma delimited file contains at least 2 columns: NAME (sample names identical to the column names of sampleData) and Compound (needs to identify to which group the sample belongs -> ExperimentalGroup & ControlGroup)
write.csv(x = SampleKey, file = SampleKeyFile)
