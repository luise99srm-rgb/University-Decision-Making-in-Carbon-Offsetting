# 📘 Master’s Thesis Repository – University Decision-Making in Carbon Offsetting: A Conjoint Study on Offsetting Preferences at the Universities of St.Gallen and Konstanz

This repository contains all scripts, data, and outputs for the Master’s thesis project analyzing survey and choice-based conjoint (CBC) data using **descriptive statistics, count data, logit modeling, and hierarchical Bayesian estimation**.

---

## 🧭 Repository Structure

```
repo/
│
├── code/                     # All R scripts for data processing & analysis
│   ├── descriptives/         # Respondent-level descriptives and distributions
│   ├── count/                # Count data analyses (frequencies, response rates)
│   ├── logit/                # Logit model estimation and visualization
│   └── hb/                   # Hierarchical Bayesian (HB) estimation & diagnostics
│
├── data/                     # Input data and fonts
│   ├── cm-unicode/           # CMU Serif fonts used for thesis figures
│   ├── anonymized_responses  # Raw respondent-level survey data (CSV)
│   └── sawtooth outputs/     # Original Sawtooth CBC exports (Excel + CSV)
│       ├── count/            # Count data exports from Sawtooth
│       ├── logit/            # Logit model exports from Sawtooth
│       └── hb/               # Hierarchical Bayes output (draws, utilities)
│
├── results/                  # All generated outputs
│   ├── figures/              # Saved PDF/PNG visualizations
│   │   ├── descriptives/     # Distribution plots, bar charts
│   │   ├── count/            # Count-level plots
│   │   ├── logit/            # Logit model coefficient visualizations
│   │   └── hb/               # HB utility plots, posterior distributions, convergence
│   │       └── convergence/  # MCMC diagnostics and trace plots
│   │
│   └── tables/               # All exported CSV and LaTeX tables
│       ├── descriptives/     # Summary statistics by group/university
│       ├── count/            # Count-level summaries
│       ├── logit/            # Logit model coefficients, importances
│       └── hb/               # HB coefficients, utilities, importances, draws
│
└── README.md                 # This documentation file
```

---

## ⚙️ Execution Order

Run the scripts in the following order to reproduce the full analysis.  
Each step writes its own tables and figures to the `results/` directory.

### 1️⃣ **Descriptives**
**Folder:** `code/descriptives/`  
**Main file:** `descriptives_main.R`

- Reads `CsvAllFields.csv` (change to anonymized_responses.csv) from `data/`.
- Maps categorical variables (gender, education, stakeholder group, etc.).
- Produces:
  - Age histograms (overall and by university)
  - Categorical facet plots
  - LaTeX + CSV tables with descriptive statistics  
- **Outputs:**  
  - `results/figures/descriptives/`  
  - `results/tables/descriptives/`

---

### 2️⃣ **Count Data**
**Folder:** `code/count/`  
**Purpose:**  
Analyzes frequencies and response patterns (if applicable).  
Generates summary tables and bar charts.  
- **Outputs:**  
  - `results/figures/count/`  
  - `results/tables/count/`

---

### 3️⃣ **Logit Model**
**Folder:** `code/logit/`  
**Main file:** `logit_analysis.R`

- Reads aggregated logit coefficients and importances.
- Creates forest plots, utility and importance charts, and pie charts.
- Exports both LaTeX and CSV tables for reporting.  
- **Outputs:**  
  - `results/figures/logit/`  
  - `results/tables/logit/`

---

### 4️⃣ **Hierarchical Bayes (HB / Bayesian)**
**Folder:** `code/hb/`  
**Main file:** `hb_analysis.R`

- Reads individual-level draws from `data/sawtooth outputs/hb/`.
- Processes zero-centered and raw utilities, importances, and posterior draws.
- Generates:
  - Violin + boxplots (individual utilities)
  - Forest plots (average HB utilities)
  - Density and credible interval plots
  - Convergence diagnostics for MCMC chains  
- Exports all intermediate and final tables as CSV (and optionally `.tex`).
- **Outputs:**  
  - `results/figures/hb/`  
  - `results/tables/hb/`

---

## 📊 Output Overview

| Analysis Stage | Key Figures | Key Tables |
|----------------|-------------|-------------|
| **Descriptives** | Age histograms, categorical bar charts | Summary statistics by university |
| **Count** | Count-level bar plots | Frequency counts and shares |
| **Logit** | Forest plots, pie chart of importances | Coefficients, utilities, importances |
| **HB** | Posterior densities, credible intervals, convergence plots | Individual utilities (raw & ZC), importances, draws summary |

---

## 🖋 Fonts

All figures use **CMU Serif**, located in:
```
data/cm-unicode/
```
Font paths are loaded at the beginning of each R script using `showtext`.

---

## 🧩 Dependencies

Install required packages once via:

```r
install.packages(c(
  "ggplot2", "dplyr", "tibble", "forcats", "showtext",
  "readxl", "tidyr", "stringr", "purrr", "kableExtra",
  "posterior", "scales"
))
```

---

## ▶️ Reproducibility Notes

1. Set the working directory to the repository root (`masterarbeit_luise/repo/`).
2. Each script automatically creates required subfolders inside `results/`.
3. To re-run only specific modules, make sure corresponding data (e.g., HB draws) exist in `data/sawtooth outputs/`.
4. Figures are saved as both `.pdf` (for thesis inclusion) and `.png` (for quick preview).

---

## 📅 Execution Summary

| Step | Script Folder | Description | Output Location |
|------|----------------|--------------|----------------|
| 1 | `code/descriptives` | Demographics & categorical summaries | `results/figures/descriptives/` / `results/tables/descriptives/` |
| 2 | `code/count` | Frequency analysis | `results/figures/count/` / `results/tables/count/` |
| 3 | `code/logit` | Logit model visualization | `results/figures/logit/` / `results/tables/logit/` |
| 4 | `code/hb` | Bayesian hierarchical model | `results/figures/hb/` / `results/tables/hb/` |

---

## 🏁 Final Notes

- All code is modular — each script can be run independently after its data is prepared.  
- The **results/tables** directory is designed for direct import into LaTeX (`\input{}` or `\include{}`).  
- The repository follows a **fully reproducible analysis workflow** suitable for academic publication.

---

## 📬 Contact

**Luise Schorm**  
Student ID: 23-600-646  
📧 [luise.schorm@student.unisg.ch](mailto:luise.schorm@student.unisg.ch)

