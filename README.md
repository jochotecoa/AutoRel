# auto_relevant

Welcome to the auto_relevant (or autorrel) model.

To be run, it requires the following installations:
- R language
- DESeq2 R package

One can access "AutoRel/scripts/autorrel/run_autorrel.R" to introduce the data and run the model.

After running DESeq2, 4 files/variables that can be recovered from the DESeq2 output are needed:
- **norm_counts**: These are the normalized counts. 
  - They can be obtained with the following line:
  ```
  norm_counts = DESeq2::counts(object = dds, normalized = T)
  ```
- **res**: this is the results object of DESeq2, where the statistical results are stored.
  - res can be obtained with the following line:
  ```
  res <- results(dds)
  ```
- **coldata**:  dataframe which you specified  in DESeq2 to split between control and treatment
  For example, if your DESeq2 code contained the following line:

  dds = DESeqDataSetFromMatrix(countData = cts, 
                               colData = **coldata**, 
                               design = ~ Group)

- **contrast_group**: One last string character you need to specify
 This is the column name of your 'coldata' data.frame
  Using the prior example: 
  
    dds = DESeqDataSetFromMatrix(countData = cts, 
                               colData = coldata, 
                               design = ~ **Group**)
    ```
    contrast_group = 'Group'
    ```
- **output_path**: Path where the output of the autorrel model will be saved
```
output_path = 'output/example_files/'
```
