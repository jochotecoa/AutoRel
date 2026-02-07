# AutoRel (auto_significant)

**AutoRel** (Automated Relevancy) is a framework designed to identify biologically relevant genes from differential expression analysis, going beyond simple statistical significance (p-values).

## Project Overview
The core of this project is the **autorrel** model, which can be used to process DESeq2 outputs to prioritize genes that show consistent and high-magnitude changes across different modeling strategies.

## Directory Structure
- **`scripts/`**: 
    - **`autorrel/`**: Core model implementation (`run_autorrel.R`).
    - **`analysis/`**: Pipeline and model comparison scripts.
    - **`case_studies/`**: Specific applications (APAP, Colorectal Cancer, etc.).
    - **`functions/`**: Utility functions used across the project.
    - **`pre-processing/`**: Data cleaning and preparation.
    - **`plotting/`**: Visualization scripts.
- **`data/`**: Input datasets for the model.
- **`output/`**: Results, figures, and model exports.

## Usage
To use the model, refer to `scripts/autorrel/run_autorrel.R`. You will need:
1.  **Normalized Counts**: From `DESeq2::counts(dds, normalized = T)`.
2.  **Results Object**: From `DESeq2::results(dds)`.
3.  **ColData**: Metadata describing your samples.
4.  **Contrast Group**: The column name defining your experimental groups.

## Requirements
- R
- DESeq2
- Tidyverse (recommended)

---
*Developed by Juan Ochoteco Asensio*