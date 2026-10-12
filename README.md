# Student Sleep Duration Analysis: R Shiny Dashboard

Survey-based exploration of which daily habits (phone use, study time, hours on campus, exercise, stress, caffeine, and others) relate to sleep duration among 2022-cohort Data Science students at Universitas Airlangga, presented as an interactive R Shiny dashboard.

**Live dashboard:** https://andrehendra19.shinyapps.io/rshinyappkelompok4/

**Tools:** R, Shiny, Google Colab (R runtime)

**Data:** Primary questionnaire (Google Form), 87 respondents from a cohort of 111 students (minimum sample from Slovin's formula, e = 0.05). 17 predictors (5 categorical, 12 numeric) and sleep duration as the target. The raw responses are not included in this repo to protect respondents' privacy.

**Approach:** Outliers replaced with the median → variable transformation and z-score normalisation → label encoding → PCA (7 components with eigenvalue > 1) → multiple linear regression on the components, checked for residual normality (Shapiro–Wilk), autocorrelation (Durbin–Watson), heteroscedasticity (Breusch–Pagan), and multicollinearity (VIF).

## Result
| Model | R² | Adj. R² | F-test p-value |
|---|---|---|---|
| Raw data, 17 predictors (before preprocessing) | 0.799 | 0.749 | < 2.2e-16 |
| 7 principal components (after preprocessing) | 0.044 | −0.040 | 0.815 |

After preprocessing, the model explained about 4% of the variance in sleep duration and was not significant; none of the seven components had a significant coefficient (all p > 0.3). The surveyed habits did not explain sleep duration in this sample.

**Note:** The high R² on the raw data was obtained before outlier handling, while the phone-use and sleep-duration columns still contained extreme values, so it should not be read as a real effect.

## Report
- [docs/report.pdf](docs/paper.pdf)
- [docs/poster.pdf](docs/poster.pdf)

---
Course project — Data Exploration and Visualization, Semester 3 (2023/2024). Team of 5. My contribution:
- Built and deployed the R Shiny dashboard.
- Helped collect the survey data.
- Helped with the analysis notebook.