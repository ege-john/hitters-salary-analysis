# Hitters Salary Prediction ⚾

This project applies multinomial logistic regression to analyze and predict the salary levels of baseball players using the classic Hitters dataset. It includes full R code (R Markdown), data, and an HTML report showing the process step by step, from data cleaning and visualization to model training, cross-validation, and result interpretation.

## 📄 Project Overview

- **Dataset:** [Hitters.csv] — Contains individual statistics and salary information of MLB players from the 1986–1987 seasons.
- **Goal:** Predict the salary category (Low, Medium, High) of players based on their performance metrics.
- **Methodology:** Multinomial logistic regression, with model evaluation via cross-validation.

## ✨ Features

- Clean and preprocess real-world data.
- Visualize key relationships and distributions in the data.
- Create categorical salary levels using data-driven quantiles.
- Build and interpret a multinomial logistic regression model.
- Assess model performance using cross-validation (K-fold, LOO, or validation set).
- Document all code, results, and reasoning in a reproducible format.

## 🚀 How to Run

1. **Requirements:**  
   - R and RStudio  
   - Required packages: `tidyverse`, `nnet`, `caret`, `ggplot2` (see code chunks for details)

2. **Files:**
   - `analysis.Rmd` — Main R Markdown analysis
   - `analysis.html` — Output report
   - `Hitters.csv` — Dataset

3. **Usage:**
   - Open the R Markdown file in RStudio.
   - Install missing packages if prompted.
   - Knit the file to HTML to reproduce all results and plots.

## 📊 Project Structure

- **Data Exploration:** Summary stats, missing values, and visualizations.
- **Feature Engineering:** Salary level creation and exploratory plots.
- **Modeling:** Multinomial regression fit and interpretation.
- **Validation:** Cross-validation using a randomly chosen method and a second method for robustness.
- **Conclusion:** Insights and model evaluation.

## 👥 Authors

- Group E: Ege John Işık, Muhammet Emin Albayram

---

*For details and results, see the HTML report or re-run the R Markdown file. For questions, contact the project authors.*

