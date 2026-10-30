# Machine Learning Prediction of Skilled Birth Attendance in Mozambique

This repository is a publication-ready scaffold for an academic R research project using **Mozambique DHS 2022–2023** data to model skilled birth attendance (SBA).

## Research Overview

Skilled birth attendance remains a core indicator for maternal and newborn health monitoring. This project is structured for reproducible model development, evaluation, and reporting of SBA prediction in Mozambique.

> **Important:** Methodological details in this scaffold are documented as planned workflow items and **must be confirmed against the original analysis scripts** before publication.

## Study Objectives

1. Develop and compare seven predictive algorithms for SBA.
2. Evaluate discrimination, calibration, and subgroup performance.
3. Provide transparent, reproducible documentation suitable for peer-reviewed reporting.

## Planned Algorithms

- XGBoost
- Support Vector Machine (SVM)
- Logistic Regression
- Artificial Neural Network (ANN)
- Random Forest
- k-Nearest Neighbors (k-NN)
- Decision Tree

## Planned Analytical Workflow *(requires confirmation against original R scripts)*

- Define outcome and predictors using DHS recode documentation and analysis plan.
- Perform train/test splitting.
- Apply missing-data handling strategy.
- Run Boruta feature selection.
- Use five-fold cross-validation for model development.
- Conduct hyperparameter tuning for eligible algorithms.
- Evaluate locked test-set performance.
- Compare AUROC values across models with Holm adjustment for multiple comparisons.
- Assess calibration (for example, calibration plots and summary metrics).
- Perform model explainability with SHAP and LIME.
- Evaluate model performance in predefined subgroups.

## Repository Structure

```text
.
├── config/             # Configuration templates and project settings
├── docs/               # Documentation (data access, reproducibility notes, protocol support)
├── figures/            # Manuscript-ready figures
├── scripts/            # R analysis scripts and pipeline entry points
├── supplementary/      # Supplementary files for publication
├── tables/             # Manuscript-ready tables
├── CITATION.cff        # Citation metadata template
└── README.md
```

## Reproducibility (R + renv)

A sample dependency-management workflow is provided in `docs/renv-workflow.md`.

This scaffold intentionally does **not** pin package versions or include analytical code in order to avoid unverified assumptions.

## Data Access and Restrictions

This repository **does not** include DHS microdata. DHS datasets are controlled-access.

- Access instructions: `docs/data-access-guide.md`
- Do not commit participant-level DHS records, extracts, or derivatives that violate DHS terms.
- Do not create or upload simulated participant-level records that could be misrepresented as DHS microdata.

## How to Use This Scaffold

1. Add approved analysis scripts to `scripts/`.
2. Add non-sensitive configuration templates to `config/`.
3. Keep figures/tables/supplementary outputs publication-ready and traceable.
4. Confirm all method statements in this README against implemented R scripts before submission.

## Citation

Please use `CITATION.cff` for citation metadata.

- DOI: `TBD`
- Version: `TBD`
- Authors: `TBD`
- Journal citation details: `TBD`

## License

This repository is released under the MIT License (see `LICENSE`).
