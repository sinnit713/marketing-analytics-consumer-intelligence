
# Marketing Analytics & Consumer Intelligence

An end-to-end marketing analytics project using R to investigate customer behaviour, consumer preferences and competitive positioning through statistical modelling and multivariate analysis.

## Project Overview

This project addresses two distinct business scenarios through three analytical workstreams, applying marketing analytics techniques to generate insights that support customer targeting, product design and strategic decision-making.

## Project Structure

This repository is organised into three analytical workstreams:

| Workstream | Focus | Details |
|---|---|---|
| Part A | Customer Segmentation & Targeting | [Explore Part A](./Part_A_Customer_Segmentation/) |
| Part B | Consumer Preferences & Conjoint Analysis | [Explore Part B](./Part_B_Consumer_Preferences/) |
| Part C | Competitive Positioning & PCA | [Explore Part C](./Part_C_Competitive_Positioning/) |

### Additional Resources

- [Complete R Script](./R/Marketing_Analytics_Complete.R)
- [Part A: Outputs](./Part_A_Customer_Segmentation/outputs/)
- [Part A: Visualisations](./Part_A_Customer_Segmentation/visualisations/)
- [Part B: Outputs](./Part_B_Consumer_Preferences/outputs/)
- [Part B: Visualisations](./Part_B_Consumer_Preferences/visualisations/)
- [Part C: Outputs](./Part_C_Competitive_Positioning/outputs/)
- [Part C: Visualisations](./Part_C_Competitive_Positioning/visualisations/)

### 1. Customer Segmentation and Targeting

A UK-based online retailer scenario involving customer transaction and demographic data.

**Analytical techniques:**
- Exploratory Data Analysis (EDA) and data preprocessing
- Recency, Frequency and Monetary (RFM) analysis
- Sequential and independent RFM segmentation
- Hierarchical clustering and K-means clustering
- Customer profiling and segment evaluation using ANOVA
- Linear Discriminant Analysis (LDA) for prospective customer targeting
- Confusion matrix-based model evaluation

### 2. Consumer Preference Analytics

A sports-car manufacturer scenario investigating consumer preferences across product attributes.

**Analytical techniques:**
- Conjoint analysis
- Part-worth utility estimation
- Attribute importance measurement
- Willingness-to-Pay (WTP) estimation
- First-choice market share simulation

### 3. Competitive Positioning

An automotive market scenario examining relationships between vehicle characteristics.

**Analytical techniques:**
- Principal Component Analysis (PCA)
- Singular value decomposition
- Principal component loadings
- Proportion of variance explained (PVE)
- Scree plot and PCA biplot
- Automotive perceptual mapping

## Technology Stack

- **Programming:** R
- **Data Analysis:** dplyr, tidyverse
- **Data Import:** readxl
- **Statistical Modelling:** MASS
- **Visualisation:** ggplot2, factoextra
- **Data Processing:** lubridate

## Repository Structure

```text
marketing-analytics-consumer-intelligence/
│
├── Part_A_Customer_Segmentation/
│   ├── data/
│   ├── outputs/
│   └── visualisations/
│
├── Part_B_Consumer_Preferences/
│   ├── R/
│   ├── data/
│   ├── outputs/
│   └── visualisations/
│
├── Part_C_Competitive_Positioning/
│   ├── R/
│   ├── data/
│   ├── outputs/
│   └── visualisations/
│
├── | R/
├── README.md
└── .gitignore
```

## Business Applications

The analytical approaches explored in this project have practical applications in:

- Customer retention and value-based segmentation
- Targeted marketing and prospect prioritisation
- Consumer preference measurement
- Product feature optimisation
- Pricing and willingness-to-pay analysis
- Market-share estimation
- Competitive positioning and strategic decision-making

## Project Context

This project originated from an MSc Business Analytics academic assessment at Queen's University Belfast. The analysis applies statistical and analytical methods to simulated business decision-making scenarios defined in the assessment brief.

## Responsible Use of AI

Generative AI was used in a limited supportive capacity for code refinement, debugging and improving the clarity of interpretation. Analytical decisions, implementation, methodology and conclusions were independently reviewed, validated and adapted.

---

**Author:** Nitish Kumar Singh

**Field:** Business Analytics | Marketing Analytics | Consumer Intelligence
