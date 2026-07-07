# future-cognition-collective-action-integrative-review
Data, materials, and R scripts for an integrative review and theoretical framework of future cognition and long-term collective action

# Looking Ahead to Act Together:
## An Integrative Review and Theoretical Framework of Future Cognition and Long-Term Collective Action

This repository contains the data, materials, and analysis scripts for an integrative review examining how theories of future cognition shape decision-making in long-term collective action problems.

The project synthesises theoretical and empirical research across domains including climate change, public health, and social justice, and introduces a framework for understanding how future-oriented cognition supports collective action over extended time horizons.

---

## Preregistration

The study was preregistered on OSF:  
https://doi.org/10.17605/OSF.IO/CMWTE

---

## Repository Structure

The repository is organised as follows:

- **`data/`**
  - `raw/` – original datasets and search outputs  
  - `processed/` – cleaned datasets used in analysis  
  - `screening/` – inclusion/exclusion decisions and disagreement resolution  

- **`materials/`**
  - Codebook (variable definitions and coding scheme)  
  - LLM prompts used for screening, summarisation, and data extraction  

- **`scripts/`**
  - R scripts for data processing, analysis, and visualisation  

  - Supplementary materials  
  - figures and tables

---

## Methods Overview

This study is an integrative review of theories of future cognition in the context of long-term collective action problems.

### Literature Search
- Databases: PubMed, Scopus, Web of Science  
- Search period: March–April 2025  

### Inclusion Criteria
Studies were included if they:
- examined future-oriented cognition (e.g., temporal discounting, construal level, episodic future thinking)  
- addressed collective or societal challenges requiring cooperation  
- measured or theorised decision-making, intentions, or behaviours linked to collective outcomes  

### Screening and Validation
- Dual screening approach combining human judgement and large language model (LLM) evaluation  
- Disagreements resolved through structured adjudication  

### Data Extraction
- Study characteristics, theoretical frameworks, and outcome measures  
- Predictors, mediators, moderators, and dependent variables  
- Outcome classification (supportive, non-supportive, mixed)  

### Analytical Approach
- Pathway-based analysis of significant statistical relationships  
- Binary representation of features within pathways  
- Hierarchical clustering using Jaccard distance for heatmap construction (used for ease of visualisation)

---

## Use of AI Tools

Large language models were used to support:

- abstract screening  
- summarisation of study results  
- extraction of statistical relationships (predictors, mediators, moderators, and outcomes)
- writing/editing the manuscript 

All prompts used in these processes are provided in the `materials/` directory for transparency and reproducibility.

---

## Reproducibility

All analyses can be reproduced using the R scripts in the `scripts/` directory.

Figures and tables in the manuscript and supplementary materials are generated from the processed datasets included in this repository. Example outputs are available in the `results/` folder.

---

## Supplementary Materials

Full supplementary materials, including detailed tables, heatmaps, and methodological documentation, are provided in the `docs/` directory.

---

## Citation

If you use this repository, please cite:

Syme, K. L. (2026).  
*Looking Ahead to Act Together: An Integrative Review and Theoretical Framework of Future Cognition and Long-Term Collective Action.*

---

## License

This repository is licensed under the MIT License.  
If you use the data or materials, please cite the associated publication.

---

## Author

Kristen L. Syme
