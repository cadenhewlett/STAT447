# STAT447 - Coursework and Final Project

This repository contains coursework and the final project for STAT 447. The final project focuses on leveraging conjugacy in Dirichlet Process Poisson Mixture Models.

## Directory Structure

### [data](https://github.com/cadenhewlett/STAT447/tree/main/data)
Contains the raw data files used for analysis, across homework and projects.

### [final_project](https://github.com/cadenhewlett/STAT447/tree/main/final_project)
This folder includes all files related to the final project:
- **cleaned_crash_data.csv**: Processed and cleaned crash data used in the analysis.
- **dirch_appx.png**: Visualization of the finite approximation of the Dirichlet process.
- **post_box.png**: Boxplots of posterior rate parameters.
- **post_comp.png**: Posterior predictive versus observed data comparison.
- **posterior_results.RDS**: Saved RDS file of the posterior results.
- **posterior_sampleframe.RDS**: Sample frame from the posterior.
- **posterior_sims.RDS**: Simulated posterior draws.
- **results.tex**: LaTeX file of the posterior parameters and weights.

### [finished_homework_pdfs](https://github.com/cadenhewlett/STAT447/tree/main/finished_homework_pdfs)
Completed homework assignments in PDF format.

### [homework_rmds](https://github.com/cadenhewlett/STAT447/tree/main/homework_rmds)
R Markdown files for the homework assignments.

### [images](https://github.com/cadenhewlett/STAT447/tree/main/images)
Images used in the reports and visualizations.

### [notes_and_utils](https://github.com/cadenhewlett/STAT447/tree/main/notes_and_utils)
Notes and utility scripts used throughout the coursework.


## Getting Started

1. **Necessary R packages**:
    ```r
    install.packages(c("dirichletprocess", "ggplot2", "latex2exp", "dplyr", "lubridate", "readxl", "tidyr", "pbapply", "hexbin", "scales", "knitr", "kableExtra"))
    ```

2. **Run the R scripts**:
    Navigate to the `final_project` directory and run the R scripts to reproduce the analysis and visualizations.

## Copyright and Licensing

© 2024 Caden Hewlett. All rights reserved.

This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License. You may not use this work for commercial purposes. You may share this work with proper attribution, but you may not alter, transform, or build upon this work without express permission from the author.

For more details about the license, see the [Creative Commons License](https://creativecommons.org/licenses/by-nc-nd/4.0/).

If you wish to cite this work for any purpose, please use the following `bibtex` citation style:
```bibtex
@misc{hewlett2024stat447,
  author = {Caden Hewlett},
  title = {Leveraging Conjugacy in Dirichlet Process Poisson Mixture Models},
  year = {2024},
  howpublished = {\url{https://github.com/cadenhewlett/STAT447}},
  note = {STAT 447 Final Project, University of British Columbia}
}
```
## Project Description

### Introduction

This project presents a non-parametric approach to the Gamma-Poisson (GP) model framework using weekly aggregation of daily crash count data from the Chicago Police Department. The main goal is to integrate the GP Bayesian Model with the `dirichletprocess` R package, which does not have an out-of-the-box solution for this setup.

### Methods

The project utilizes the stick-breaking process to define the Dirichlet Process Mixture Model (DPMM) and implements a custom mixing distribution for the Gamma-Poisson conjugate pair.

### Results

The results include posterior parameter estimates, cluster assignments, and predictive densities. The analysis demonstrates the effectiveness of the DPMM in identifying varying rates of crash counts.

### Discussion

The findings highlight the potential and limitations of non-parametric Bayesian approaches in modeling crash data. Suggestions for future work include extending the model to other conjugate pairs and improving cluster assignment methods.

## Sources

- [Chicago Traffic Crashes - Chicago Police Dept.](https://doi.org/10.34740/KAGGLE/DSV/7339559)
- [An Introduction to Mathematical Proof: MATH 220](https://personal.math.ubc.ca/~PLP/)
- [Bayesian Analysis of Some Nonparametric Problems](https://doi.org/10.1214/aos/1176342360)
- [Dirichlet Process Mixtures of Generalized Linear Models](https://www.jmlr.org/papers/volume12/hannah11a/hannah11a.pdf)
- [Markov Chain Sampling Methods for Dirichlet Process Mixture Models](https://doi.org/10.1080/10618600.2000.10474879)
- [Hierarchical Dirichlet Processes](https://www.cs.cmu.edu/~epxing/Class/10708-14/scribe_notes/scribe_note_lecture20.pdf)
- [Research on Dirichlet Process Mixture Model for Clustering](https://doi.org/10.18280/isi.240209)
