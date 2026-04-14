# Predicting Diabetes Status with Statistical Models (R)

## Overview
This project builds predictive models for **diabetes status (0/1)** using a dataset of **100,000 observations**. The workflow includes exploratory data analysis (EDA), preprocessing, model training, cross-validation, and model comparison in **R**.

## Objective
The goal is to identify important risk factors associated with diabetes and compare multiple statistical / machine learning models for classification.

## Methods
The project evaluates three models:

- **k-Nearest Neighbors (k-NN)**
- **Decision Tree (DT)**
- **Logistic Regression (LR)**

A shared **5-fold cross-validation** setup is used for model tuning and comparison.

## Main variables
Predictors used in the analysis include:

- `gender`
- `smoking_history`
- `hypertension`
- `heart_disease`
- `age`
- `bmi`
- `HbA1c_level`
- `blood_glucose_level`

Response variable:

- `diabetes` (0 = non-diabetic, 1 = diabetic)

## Key findings
- Strong numerical predictors include **HbA1c_level** and **blood_glucose_level**
- **hypertension** and **heart_disease** are the strongest categorical signals
- The best reported ROC-AUC in the study is **0.9691** for the Decision Tree model
- **Logistic Regression** is recommended as the final model because it offers a strong balance between performance, interpretability, calibration, and deployment simplicity

## Repository structure
```text
.
├── analysis.R
├── README.md
├── .gitignore
└── Statistical_Report_Vo_Ngoc_Gia_Bao_AY2526.pdf
```

## Required R packages
Install the required packages before running the script:

```r
install.packages(c(
  "ggplot2",
  "scales",
  "dplyr",
  "tidyr",
  "class",
  "gridExtra",
  "gtable",
  "rpart",
  "rpart.plot",
  "pROC",
  "ROCR",
  "tibble"
))
```

## How to run
1. Place the dataset file in your working directory.
2. Update the file path inside `analysis.R` if needed.
3. Run the script in R or RStudio.

## Notes
- The current script uses a local working directory via `setwd(...)`. You may want to replace that with a relative project path before publishing.
- Generated figures are saved into the `figs_report/` folder created by the script.
- If the dataset is private or too large, do not upload it to GitHub. Instead, describe its expected filename and format in this README.

## Author
Vo Ngoc Gia Bao  
Bachelor Student in Data Science and Analytics  
National University of Singapore
