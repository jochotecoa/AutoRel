# auto_relevant

Welcome to the auto_relevant (or autorrel) model.

To be run, it requires the following installations:
- R language
- DESeq2 R package

After running DESeq2, 2 files that can be recovered from the output are needed:
- norm_counts: These are the normalized counts. 
  - They can be obtained with the following line:
  norm_counts = DESeq2::counts(object = dds, normalized = T)

- res: this is the results object of DESeq2, where the statistical results are stored.
  - res can be obtained with the following line:
  res <- results(dds)
  
 One last string character you need to save is the contrast_group:
 This is the column name of your 'coldata' dataframe which you specified
  in DESeq2 to split between control and treatment
  For example, if your DESeq2 code contained the following line:
  dds = DESeqDataSetFromMatrix(countData = cts, 
                             colData = coldata, 
                             design = ~ Group)

  then you would write in your R session:
    contrast_group = 'Group'
