# AutoRel (Auto-Relevant)

**AutoRel** is a tool designed to identify biologically relevant genes from RNA-Seq differential expression analysis, going beyond simple statistical significance.

## 🚀 Quick Start (CLI)

1.  **Install as an R Package (Directly from GitHub):**
    ```R
    # In R:
    devtools::install_github("jochotecoa/AutoRel")
    ```

2.  **Run via CLI:**
    ```bash
    Rscript run_AutoRel.R --counts counts.csv --results res.csv --metadata meta.csv --contrast Group --control Control --report
    ```

3.  **Verify installation (Optional):**
    ```bash
    Rscript test_installation.R
    ```

## 🛠️ Developer Usage
You can use AutoRel directly inside your R scripts:

```R
library(AutoRel)

results <- run_prioritization(
  norm_counts = my_counts,
  res = my_deseq2_results,
  coldata = my_metadata,
  contrast_group = "Group",
  control_level = "Control",
  model_path = system.file("extdata", "autorrel.rds", package="AutoRel"),
  output_path = "results/"
)
```

## 📦 Model Versioning
This tool uses **GitHub Releases** to manage model versions. This ensures that the code version you use is always paired with the correct trained model.

### For Maintainers: How to Release a New Model
To update the model hosted on GitHub:
1.  **Tag the commit:** `git tag v0.1.0`
2.  **Push the tag:** `git push origin v0.1.0`
3.  **Create a Release** on GitHub and upload the `autorrel.rds` file as a binary asset.

The `install_AutoRel.R` script will then automatically pull the model from that release.

## 🐳 Docker Usage
Docker allows you to run AutoRel without worrying about R dependencies.

1.  **Build the image:**
    ```bash
    docker build -t autorell .
    ```

2.  **Run the analysis:**
    (Mount your local data folder to `/app/data` in the container)
    ```bash
    docker run -v /path/to/your/data:/app/data autorell --counts data/counts.csv --results data/res.csv --metadata data/meta.csv --contrast Group --control Control
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
| `--report`     | (Optional) Generate an automated HTML analysis report |

## 📊 Visual Reporting
When using the `--report` flag, AutoRel generates a professional HTML report containing:
- **Summary findings:** Total genes prioritized as relevant.
- **Interactive Tables:** Filterable list of relevant genes.
- **Relevance vs. Significance Plot:** A volcano-style visualization highlighting prioritized genes.
- **Top Genes Heatmap:** Scaled expression profiles of the top 20 prioritized genes.

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
