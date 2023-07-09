# Multi-layer modeling for CVD risk prediction - UK BioBank Study
Translational Data Science Project (Group 1)

## Project description -  
- Cardiovascular Disease (CVD) represents a significant and prevalent health challenge worldwide.
- Various environmental factors havebeen recognised as risk factors for CVD but **causal links** (direct or indirect, mediated through biological variables) between such factors and CVD outcomes are difficult to establish in conventional study formats.
- Data was collected from the UK Biobank, a large prospective cohort study. This study was conducted on 240,103 participants without
a history of CVD.
- Structural Causal Modeling with five defined hierarchical layers was utilised within a LASSO stability selection framework to identify
potential causative pathways between environmental, socio-economic, behavioural, QRISK3 factors and CVD risk - [Figure 1](https://github.com/rictoo/tds-proj/assets/70545953/90e04a98-5a6d-4eca-b17d-04180ab86c22)
- The model was refitted and predictive performance was evaluated on a hold-out test set - [Figure 2](https://github.com/rictoo/tds-proj/assets/70545953/da28cb67-9417-4b41-bc7b-d48305f2fa85)

![Figure 1](https://github.com/rictoo/tds-proj/assets/70545953/90e04a98-5a6d-4eca-b17d-04180ab86c22)
**Figure 1: Proposed Directed Acyclic Graph (DAG) modeling CVD risk across layers**

![Figure 2](https://github.com/rictoo/tds-proj/assets/70545953/da28cb67-9417-4b41-bc7b-d48305f2fa85)
**Figure 2: Cox Model Performance with Successive Variable Inclusion**

## Code and analytical pipeline

This project repository contains the analytical pipeline to generate the statistical inferences in the research project detailed above.

Pre-processing is performed in the preprocessing.Rmd file followed by analysis in analysis.Rmd. 

The full details of the methods employed are discussed in the final report document.

Included in this repository are:
1. Two .Rmd files in the root folder:
- preprocessing.Rmd: Fill pre-processing workflow, with the final block outputting a dataset to-be-imputed. 
- analysis.Rmd: Statistical analyses on complete data (following imputation).

2. A .py file within the `imputation` folder (and accompanying .sh file):
- This Python script executes k-NN imputation on the specified dataset.

3. Supplementary images and a .bib citation file.
