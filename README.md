# Predicting Diabetes Status with Statistical Models (R)

## Overview
This project develops predictive models for diabetes status (0/1) using a dataset of 100,000 observations. The workflow includes exploratory data analysis (EDA), preprocessing, model training, cross-validation, and model comparison in R.

## Objective
The aim is to identify the key risk factors associated with diabetes and compare multiple classification models.

## Methods
The project evaluates three models:
- k-Nearest Neighbors (k-NN)
- Decision Tree (DT)
- Logistic Regression (LR)

All models are compared using 5-fold cross-validation.

## Variables
Predictors used in the analysis include:
- gender
- smoking_history
- hypertension
- heart_disease
- age
- bmi
- HbA1c_level
- blood_glucose_level

Response variable:
- diabetes (0 = non-diabetic, 1 = diabetic)

## Key Results
- Strong numerical predictors include HbA1c_level and blood_glucose_level.
- Hypertension and heart_disease show strong categorical associations with diabetes.
- The highest ROC-AUC achieved is 0.9691.
- Logistic Regression is recommended as the final model because it provides a strong balance between predictive performance and interpretability.

## Repository Structure
- `analysis.R` — main R script for EDA, model training, and evaluation
- `Statistical Report_Vo Ngoc Gia Bao_AY2526.pdf` — final statistical report
- `README.md` — project overview

## Tools and Packages
This project is implemented in R using packages such as:
- ggplot2
- dplyr
- tidyr
- class
- rpart
- rpart.plot
- pROC
- ROCR

## Author
Vo Ngoc Gia Bao  
Bachelor Student in Data Science and Analytics  
National University of Singapore
