# AutoRel (Auto-Relevant)

**AutoRel** is a tool designed to identify biologically relevant genes from RNA-Seq differential expression analysis, going beyond simple statistical significance.

## 🚀 Quick Start (CLI)

1.  **Install dependencies and model:**
    ```bash
    Rscript install_AutoRel.R
    ```

2.  **Run the analysis:**
    ```bash
    Rscript run_AutoRel.R --counts counts.csv --results res.csv --metadata meta.csv --contrast Group --control Control --output results/
    ```

3.  **Verify installation (Optional):**
    ```bash
    Rscript test_installation.R
    ```

## CLI Arguments
| Argument | Description |
| :--- | :--- |
| `-c, --counts` | Path to normalized counts CSV (gene names in 1st column) |
| `-r, --results` | Path to DESeq2 results CSV (gene names in 1st column) |
| `-m, --metadata` | Path to coldata/metadata CSV (sample names in 1st column) |
| `-g, --contrast` | Column name in metadata used for grouping |
| `-l, --control` | (Optional) Name of the control level |
| `-o, --output` | (Optional) Output directory (default: output/autorrel_results) |

## Project Structure
- **`run_AutoRel.R`**: Main Command Line Interface.
- **`install_AutoRel.R`**: Script to install dependencies and download the model.
- **`scripts/`**: 
    - **`autorrel/`**: Core pipeline and model logic.
    - **`analysis/`**: Pipeline and model comparison scripts.
    - **`case_studies/`**: Specific applications (APAP, Colorectal Cancer, etc.).
- **`models/`**: Storage for the trained `autorrel.rds` model.
- **`data/`**: Input datasets and example files.
- **`output/`**: Results, figures, and model exports.

## Requirements
- R (>= 4.0)
- DESeq2
- Tidyverse, Caret, Magrittr, Optparse

---
*Developed by Juan Ochoteco Asensio*
