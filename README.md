Group 1: **Environmental Determinants of Cardiovascular Disease**

This project contains the analytical pipeline to generate the statistical inferences in the research project.

Pre-processing is performed in the preprocessing.Rmd file followed by analysis in analysis.Rmd. 

The full details of the methods employed are discussed in the final report document.

Included in this repository are:
1. Two .Rmd files in the root folder:
- preprocessing.Rmd: Fill pre-processing workflow, with the final block outputting a dataset to-be-imputed. 
- analysis.Rmd: Statistical analyses on complete data (following imputation).

2. A .py file within the `imputation` folder (and accompanying .sh file):
- This Python script executes k-NN imputation on the specified dataset.

3. Supplementary images and a .bib citation file.
