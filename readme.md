


# Project tittle: GENETIC VARIABILITY OF EARLY MATURING INBRED MAIZE UNDER ARTIFICIAL AND NATURAL INFESTATION OF FALL ARMYWORM (Spodoptera frugiperda J.E. Smith)

This repository contains R scripts and data to evaluate genetic variability for resistance to Fall Armyworm (FAW) among early-maturing white maize inbred lines under infested (natural and artificial infestations) and control environment. It also identifies promising hybrids that combine FAW resistance with grain yield and other desirable agronomic characteristics, and assesses relationships between agronomic traits and FAW resistance.

## Table of Contents
1. Overview 
2. Data Description
3. Prerequisites
4. Installation
5. Usage
6. Analysis Steps
7. Results
8. Contributing


objectives:

This project aims to:
1. Evaluate genetic variability for FAW resistance across maize hybrids and environments.
2. Identify promising hybrids that combine FAW resistance with high grain yield and desirable agronomic traits.
3. Assess relationships between agronomic traits and FAW resistance.

The analysis uses statistical methods such as ANOVA, AMMI, correlation analysis, and regression modeling. Visualizations include violin plots, scatter plots, bar charts, and lollipop charts.



## Data Description

The dataset (`MAIZE DATA.csv`) contains the following variables:

- `env`: Environment [infested (natural or artificial infestation) and control environment]  .
- `set`: Experimental set.
- `rep`: Replication.
- `blk`: Block.
- `male`, `female`: Parental lines.
- `hybrid`: Hybrid identifier.
- `DA`, `DS`: Days to anthesis and silking.
- `ASI`: Anthesis-silking interval.
- `PHT`, `EHT`: Plant height and ear height.
- `EPP`: Ears per plant.
- `EASP`: Ear Aspect Score (lower values indicate better FAW resistance).
- `GY`: Grain yield.

---

## Prerequisites

To run this project, you need the following software and libraries installed:

- **R**: Version 4.0 or higher.
- **R Packages**:
  - `dplyr`
  - `tidyverse`
  - `agricolae`
  - `ggplot2`

Install required packages using:
```R
install.packages(c("dplyr", "tidyverse", "agricolae", "ggplot2"))
```


### Running the Script

To reproduce the analysis:

1. Open the `analysis_script.R` file in RStudio or another R editor.
2. Run the script line by line or execute the entire script.

### Generating Outputs

- Figures and tables will be generated in the `outputs/` folder.
- Summary statistics and ANOVA results will be saved as `.txt` files in the `results/` folder.

---

## Analysis Steps

### Question 1: Evaluate Genetic Variability for FAW Resistance
1. Load and preprocess the data.
2. Group data by environment and hybrid to calculate summary statistics (`mean_EASP`, `sd_EASP`, `var_EASP`).
3. Visualize variability using violin plots and boxplots.
4. Perform ANOVA to test for significant differences in `EASP` across hybrids and environments.
5. Conduct AMMI analysis to assess stability across environments.

### Question 2: Identify Promising Hybrids
1. Aggregate data by hybrid to calculate mean values for `EASP`, `GY`, and other agronomic traits.
2. Filter hybrids based on thresholds for `EASP` (bottom 25%) and `GY` (top 25%).
3. Add a combined score to rank hybrids.
4. Visualize top-performing hybrids using scatter plots and bar charts.

### Question 3: Assess Relationships Between Agronomic Traits and FAW Resistance
1. Compute correlations between `EASP` and agronomic traits.
2. Fit a multiple linear regression model to predict `EASP` from agronomic traits.
3. Visualize relationships using scatter plots and lollipop charts.

---

## Results

The outputs of the analysis include:

1. **Figures**:
   - Violin plots showing FAW resistance distributions.
   - Scatter plots of FAW resistance vs. grain yield.
   - Lollipop charts of correlation coefficients.

2. **Tables**:
   - Summary statistics for `EASP` across hybrids and environments.
   - ANOVA results.
   - Ranked list of promising hybrids.

3. **Files**:
   - Saved figures in `outputs/`.
   - Summary statistics and ANOVA results in `results/`.

---

## Contributing

If you would like to contribute to this project:
1. Fork the repository.
2. Create a new branch for your changes.
3. Submit a pull request with a detailed description of your contribution.

---


### Contact Information

For questions or feedback, please contact:
- Name: Omolola thompson
- Email: thompsonomolola@gmail.com



