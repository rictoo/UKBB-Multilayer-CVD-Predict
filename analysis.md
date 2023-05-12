CVD Project: Statistical analysis
================
Group 1

# Setup

    ## Loading required package: Matrix

    ## Loaded glmnet 4.1-7

    ## Loading required package: fake

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: ggplot2

    ## Loading required package: lattice

    ## Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
    ## had status 1

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

``` r
iter9 <- readRDS("imputed_iter9_cc_plswork_onehot_outcomes.rds")
iter12 <- readRDS("imputed_iter9_cc_plswork_nothot_outcomes_rownames.rds")
iter12 <- cbind(iter12, outcome_incident_case = iter9$outcome_incident_case, outcome_tte = iter9$outcome_tte)
str(iter12)
head(iter12)
names(iter12)
str(iter12)

# removing the outcomes and all factor variables before normalizing the numeric predictors 

df_norm <- iter12 %>% select(-c(outcome_incident_case, outcome_tte)) %>% select_if(is.numeric) 
df_non_numeric <- iter12[, !(names(iter12) %in% names(iter12 %>% select_if(is.numeric)))]
df_scaled <- data.frame(scale(df_norm))
df_use <- cbind(df_non_numeric, df_scaled, outcome_incident_case = iter12$outcome_incident_case, outcome_tte = iter12$outcome_tte )


df_use$be_smoking_status.0.0_cat <- factor(df_use$be_smoking_status.0.0_cat, labels = c("Never", "Former", "Current"))
df_use$se_highest_quali.0.0 <- case_when(
  df_use$se_highest_quali.0.0_cat_Low == 1 ~ 1,
  df_use$se_highest_quali.0.0_cat_Intermediate == 1 ~ 2,
  df_use$se_highest_quali.0.0_cat_Other.professional == 1 ~ 3,
  df_use$se_highest_quali.0.0_cat_High == 1 ~ 4)
df_use$se_highest_quali.0.0 <- factor(df_use$se_highest_quali.0.0, labels = c("Low", "Intermediate", "Moderate", "High"))
df_use$be_alcohol_freq.0.0_cat <- factor(df_use$be_alcohol_freq.0.0_cat, labels = c("never", "social", "moderate", "daily"))
df_use$be_days_weeks_walking.0.0_cat <- factor(df_use$be_days_weeks_walking.0.0_cat, labels = c("0-2 days", "3-5 days", "6-7 days"))
df_use$be_days_weeks_mod_phys.0.0_cat <- factor(df_use$be_days_weeks_mod_phys.0.0_cat, labels = c("0-2 days", "3-5 days", "6-7 days"))
df_use$be_days_weeks_vig_phys.0.0_cat <- factor(df_use$be_days_weeks_vig_phys.0.0_cat, labels = c("0-2 days", "3-5 days", "6-7 days"))
df_use$bl_ethnic_bckgd.0.0_cat <- factor(df_use$bl_ethnic_bckgd.0.0_cat, labels = c("White", "Other"))

df_use <- subset(df_use, select = -c(se_highest_quali.0.0_cat_Low, se_highest_quali.0.0_cat_Intermediate,
                                   se_highest_quali.0.0_cat_Other.professional, se_highest_quali.0.0_cat_High))
# Cox ##########

cox_example <- coxph(Surv(outcome_tte, outcome_incident_case) ~ bl_BMI.0.0 + bl_sex.0.0 + bl_age_at_recr.0.0, 
                     data = df_use)
summary(cox_example)

# Testing this code
betas <- c()
se = c()
se1 = c()
se2 = c()
pvals = c()
model <- coxph(Surv(outcome_tte, outcome_incident_case) ~ be_alcohol_freq.0.0_cat + bl_sex.0.0 + bl_age_at_recr.0.0, 
               data = df_use)
n.levels <- length(levels(df_use$se_highest_quali.0.0))-1
exp_betas=c(betas, exp(summary(model)$coefficients)[seq(1,by = 1,length = n.levels),1])
HR = as.numeric(exp_betas)
variables <- names(exp_betas)
betas_df <- data.frame(exp_betas)
HR_CI_lower=as.numeric(c(se, exp(confint(model))[seq(1,by = 1,length = n.levels),1]))
HR_CI_upper=as.numeric(c(se1, exp(confint(model))[seq(1,by = 1,length = n.levels),2]))
se_HR=as.numeric(c(se2, exp(summary(model)$coefficients)[seq(1,by = 1,length = n.levels),3]))
p_value=as.numeric(c(pvals, summary(model)$coefficients[seq(1,by = 1,length = n.levels),5]))
results_df <- data.frame(cbind(variables, p_value, HR,HR_CI_lower, HR_CI_upper, se_HR))
results_df[, 2:6] <- sapply(results_df[, 2:6], as.numeric)

# Define variables
outcome_var <- "outcome_incident_case"
censoring_var <- "outcome_tte"
age_var <- "bl_age_at_recr.0.0"
sex_var <- "bl_sex.0.0"
# Create empty dataframe to store results
cox_results <- data.frame(variables = character(),
                          p_value = numeric(),
                          HR = numeric(),
                          HR_CI_lower = numeric(),
                          HR_CI_upper = numeric(),
                          se_HR = numeric(),
                          stringsAsFactors = FALSE)

## Use this 

# Loop through each variable in the dataframe (excluding outcome, censoring, age, and sex)
for (variable in colnames(df_use)[!colnames(df_use) %in% c(outcome_var, censoring_var, age_var, sex_var)]) {
  
  print(variable)
  betas <- c()
  se = c()
  se1 = c()
  se2 = c()
  pvals = c()
  
  # Fit univariate Cox regression model
  model <- coxph(as.formula(paste("Surv(", censoring_var, ", ", outcome_var, ") ~ ", variable, " + ", age_var, " + ", sex_var)), data = df_use)
  
  if(is.factor(df_use[,variable])){ 
    n.levels = length(levels(df_use[,variable]))-1
    if(n.levels != 1){
      exp_betas=c(betas, exp(summary(model)$coefficients)[seq(1,by = 1,length = n.levels),1])
      HR = as.numeric(exp_betas)
      variables <- names(exp_betas)
      betas_df <- data.frame(exp_betas)
      HR_CI_lower=as.numeric(c(se, exp(confint(model))[seq(1,by = 1,length = n.levels),1]))
      HR_CI_upper=as.numeric(c(se1, exp(confint(model))[seq(1,by = 1,length = n.levels),2]))
      se_HR=as.numeric(c(se2, exp(summary(model)$coefficients)[seq(1,by = 1,length = n.levels),3]))
      p_value=as.numeric(c(pvals, summary(model)$coefficients[seq(1,by = 1,length = n.levels),5]))
      results_df <- data.frame(cbind(variables, p_value, HR,HR_CI_lower, HR_CI_upper, se_HR))
      print(results_df)
      #results_df[, 2:6] <- sapply(results_df[, 2:6], as.numeric)
      
      cox_results <- rbind(cox_results, results_df)
    } else {
      
      # Extract HR and confidence intervals
      HR <- exp(coef(model))[1]
      HR_CI <- exp(confint(model))[1,]
      
      # Extract p-value for variable of interest
      p_value <- summary(model)$coefficients[1, 5]
      
      # Extract se of HR
      se_HR <- exp(summary(model)$coefficients[1, 3])
      
      # Add results to dataframe
      cox_results <- rbind(cox_results, data.frame(variables = variable,
                                                   p_value = p_value,
                                                   HR = HR,
                                                   HR_CI_lower = HR_CI[1],
                                                   HR_CI_upper = HR_CI[2],
                                                   se_HR = se_HR))}
    } else {
      
      # Extract HR and confidence intervals
      HR <- exp(coef(model))[1]
      HR_CI <- exp(confint(model))[1,]
      
      # Extract p-value for variable of interest
      p_value <- summary(model)$coefficients[1, 5]
      
      # Extract se of HR
      se_HR <- exp(summary(model)$coefficients[1, 3])
      
      # Add results to dataframe
      cox_results <- rbind(cox_results, data.frame(variables = variable,
                                                   p_value = p_value,
                                                   HR = HR,
                                                   HR_CI_lower = HR_CI[1],
                                                   HR_CI_upper = HR_CI[2],
                                                   se_HR = se_HR))
    } 
}  

saveRDS(cox_results, "univar_output/univar_cox_new.rds")
```

# Stability selection models

### Layer 1

#### Stability selection

``` r
## Running Model 1 - everything to CVD and using the output for the second model 


rm(list=ls())
VAL = FALSE
SENSITIVITY = FALSE
setwd("/rds/general/user/dba22/home/tds/Group1/tds-proj/final/")

if(SENSITIVITY == FALSE) {
  iter12_ <- readRDS("data/dataset_primary_imputed_encoded.Rds")
} else {
  iter12 <- readRDS("data/dataset_sensitivity_imputed_encoded.Rds")
}
X <- iter12 %>% dplyr::select(-c('outcome_tte', 'outcome_incident_case'))#, 'split'))
DLBCL.surv <- Surv(iter12$outcome_tte, iter12$outcome_incident_case)

set.seed(234)
if(VAL) {
  idx_train <- sample(1:nrow(X), 0.4*nrow(X))
} else {
  idx_train <- sample(1:nrow(X), 0.8*nrow(X))
}

X_train <- X[idx_train, ] 
Y_train <- DLBCL.surv[idx_train, ] 
X_test <- X[-idx_train, ] 
Y_test <- DLBCL.surv[-idx_train, ]

if(VAL) {
  idx_val <- sample(1:nrow(X_test), 0.5*nrow(X_test))
  X_val <- X_test[idx_val, ]
  Y_val <- Y_test[idx_val, ]
  X_test <- X_test[-idx_val, ]
  Y_test <- Y_test[-idx_val, ]
}

# Don't penalise age and sex
sex_index <- grep("bl_sex.0.0", colnames(X))
age_index <- grep("bl_age_at_recr.0.0", colnames(X))
penalty_factor <- c(replace(rep(1,ncol(X)), c(sex_index, age_index), 0))
penalty_factor
t0 <- Sys.time()

X_train[] <- lapply(X_train, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x)


out <- VariableSelection(
  xdata = X_train,
  ydata = Y_train,
  verbose = TRUE, penalty.factor = penalty_factor,
  family = "cox", pi_list = seq(0.6, 0.99, by = 0.01)
)

t1 <- Sys.time() 
print(t1 - t0) 
par(mar = c(7, 7, 7, 7))
calplot <- CalibrationPlot(out) 
calplot
selprop <- SelectionProportions(out) 
print(selprop) 
hat_params <- Argmax(out) 
print(hat_params)

model1_out <- t(data.frame(SelectionProportions(out)))
rownames(model1_out) <- "CVD"
model1_out <- data.frame(model1_out)
model1_out$pi <- hat_params[2]

saveRDS(model1_out, "scm_results/model1_fulloutput_80traindata.rds")
```

#### Plotting

``` r
test <- readRDS("scm_results/model1_fulloutput_80traindata.rds")
#test <- test %>% dplyr::rename(PRS = gen_PRS)

get_stability_plot("scm_results/model1_fulloutput_80traindata.rds", "CVD Variables", flip = TRUE) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
```

    ## Using outcomes as id variables

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## ℹ Results may be unexpected or may change in future versions of ggplot2.

![](analysis_files/figure-gfm/l1_plotting-1.png)<!-- -->

### Layer 2

#### Stability selection

``` r
VAL = TRUE

#iter9 <- readRDS("data/dataset_primary_imputed_encoded.Rds")
iter9 <- readRDS("data/dataset_sensitivity_imputed_encoded.Rds")

#iter9 <- iter9[1:50000,]

#model1_output <- readRDS("scm_results/model1_fulloutput_80traindata.rds")
model1_pi <- model1_output$pi


# Load the selected bio variables from model 1 
#model1_bioselected <- readRDS("scm_results/model1_biooutput_80traindata.rds")

selected_outcomes <- colnames(model1_output[,grep("^bio", colnames(model1_output), value = TRUE)] )[colSums(abs(model1_output[,grep("^bio", colnames(model1_output), value = TRUE)]) >= model1_pi) >= 1]

# Run stability stab against 1 outcome and try to feed that object into pred/explanatory performance/incremental
# removing all outcome and bio data 
X <- iter9 %>% dplyr::select(-c("outcome_incident_case", "outcome_tte", 
                          grep("^bio", names(iter9), value = TRUE)))


Y <- iter9 %>% dplyr::select(selected_outcomes)

# Splitting the data into 80% train, 20% test
set.seed(234)
if(VAL) {
  idx_train <- sample(1:nrow(X), 0.4*nrow(X))
} else {
  idx_train <- sample(1:nrow(X), 0.8*nrow(X))

}

X_train <- X[idx_train, ] 
Y_train <- Y[idx_train, ]
X_test <- X[-idx_train, ] 
Y_test <- Y[-idx_train, ]

if(VAL) {
  idx_val <- sample(1:nrow(X_test), 0.5*nrow(X_test))
  X_val <- X_test[idx_val, ]
  Y_val <- Y_test[idx_val, ]
  X_test <- X_test[-idx_val, ]
  Y_test <- Y_test[-idx_val, ]
}

# Setting penalty vector to adjust for age and sex
sex_index <- grep("bl_sex.0.0", colnames(X))
age_index <- grep("bl_age_at_recr.0.0", colnames(X))
penalty_factor <- c(replace(rep(1,ncol(X)), c(sex_index, age_index), 0))
penalty_factor

x_names <- names(X)
x_names <- setdiff(x_names, c("bl_sex.0.0", "bl_age_at_recr.0.0"))

# Running stability for all outcomes
df <- data.frame(matrix(nrow=8, ncol=47))
colnames(df) <- x_names
rownames(df) <- names(Y)

df_param <- data.frame(matrix(nrow=8, ncol=2))
colnames(df_param) <- c("lambda", "pi")
rownames(df_param) <- names(Y)

X_train[] <- lapply(X_train, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x)

out <- VariableSelection(
    xdata = as.matrix(X_train),
    ydata = Y_train["bio_sys_auto"],
    verbose = TRUE,
    penalty.factor = penalty_factor,
    family = "gaussian",
    pi_list = seq(0.6, 0.99, by = 0.01))

selprop <- SelectionProportions(out)
  print(selprop)
  hat_params <- Argmax(out)
  print(hat_params)

# LASSO stability selection
t0 <- Sys.time()
rowcount <- 1
for(bio in names(Y_train)) {
  print(bio)
  index <- 1
  t0 <- Sys.time()
  out <- VariableSelection(
    xdata = as.matrix(X_train),
    ydata = Y_train[bio],
    verbose = TRUE,
    penalty.factor = penalty_factor,
    family = "gaussian",
    pi_list = seq(0.6, 0.99, by = 0.01))

  selprop <- SelectionProportions(out)
  print(selprop)
  hat_params <- Argmax(out)
  print(hat_params)
  print(rowcount)
  
  # refit_formula <- GetFormula(X_train, bio, selected=names(SelectedVariables(out))[SelectedVariables(out)==1])
  # refit_data <- cbind(X_train, ydata=Y_train[bio])
  # model <- do.call(stats::lm, args = c(
  #  list(
  #    formula = refit_formula,
  #    data = as.data.frame(refit_data)
  #  )))
  # 
  # for(coef in names(model$coefficients)[2:(length(model$coefficients)-2)]) {
  #   if(model$coefficients[coef] < 0) {
  #     selprop[coef] <- -1*selprop[coef]
  #   }
  # }

  df[rowcount, ] <- selprop
  df_param[rowcount, ] <- hat_params

  rowcount <- rowcount + 1
  t1 <- Sys.time()
  print(t1 - t0)
}

CalibrationPlot(out)

df_combined <- cbind(df, df_param)
saveRDS(df_combined, "scm_results/model2_fulloutput_80traindata.rds")
```

#### Plotting

``` r
library(cowplot)

# To create the legend:
# names <- c('Genetic', 'Environmental', 'Socioeconomic', 'Behavioural', 'Co-morbidities', 'Medications', 'Biological')
# clrs <- c('deeppink3', 'purple', 'darkorange', 'darkblue', 'darkgreen', 'red', 'royalblue')
# #par()
# #plot.new()
# # Create an empty plot
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# 
# # Add a legend with filled squares
# legend("topleft", horiz=FALSE, legend = names,
#        pch = 15, # Use filled squares
#        col = clrs, # Fill squares with colors specified in 'clrs'
#        pt.cex = 6, # Size of the squares
#        bty='n', cex=2)
# 
# #p1 <- recordPlot()

get_stability_plot("scm_results/model2_fulloutput_80traindata.rds", "Biological Variables", flip = TRUE)
```

    ## Using outcomes as id variables

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## ℹ Results may be unexpected or may change in future versions of ggplot2.

![](analysis_files/figure-gfm/l2_plotting-1.png)<!-- -->

``` r
#plot_grid(layer2_plot, p1,nrow = 2)
```

### Layer 3

#### Stability selection

``` r
VAL = TRUE

model2_output <- readRDS("scm_results/model2_fulloutput_80traindata.rds")
model2_pi <- model2_output$pi
selected_outcomes <- colnames(model2_output[,grep("^med", colnames(model2_output), value = TRUE)] )[colSums(abs(model2_output[,grep("^med", colnames(model2_output), value = TRUE)]) >= model2_pi) >= 1]
selected_outcomes <- setdiff(selected_outcomes, c("med_antipsych"))

# Everything against the medications selected
iter12 <- readRDS("data/dataset_primary_imputed_encoded.Rds")

# removing all outcome and bio data 
X <- iter12 %>% dplyr::select(-c("outcome_incident_case", "outcome_tte", 
                          grep("^bio", names(iter12), value = TRUE),
                          grep("^med", names(iter12), value = TRUE)))

# Only select those outcomes that were selected in the preceding stability selection runs
Y <- subset(iter12, select = selected_outcomes)

# Splitting the data into 80% train, 20% test
set.seed(234)
if(VAL) {
  idx_train <- sample(1:nrow(X), 0.4*nrow(X))
} else {
  idx_train <- sample(1:nrow(X), 0.8*nrow(X))

}

X_train <- X[idx_train, ] 
Y_train <- Y[idx_train, ]
X_test <- X[-idx_train, ] 
Y_test <- Y[-idx_train, ]

if(VAL) {
  idx_val <- sample(1:nrow(X_test), 0.5*nrow(X_test))
  X_val <- X_test[idx_val, ]
  Y_val <- Y_test[idx_val, ]
  X_test <- X_test[-idx_val, ]
  Y_test <- Y_test[-idx_val, ]
}

# Setting penalty vector to adjust for age and sex
sex_index <- grep("bl_sex.0.0", colnames(X))
age_index <- grep("bl_age_at_recr.0.0", colnames(X))
penalty_factor <- c(replace(rep(1,ncol(X)), c(sex_index, age_index), 0))
penalty_factor

x_names <- names(X)
x_names <- setdiff(x_names, c("bl_sex.0.0", "bl_age_at_recr.0.0"))

df <- data.frame(matrix(nrow=dim(Y)[2], ncol=(dim(X)[2]-2)+1))
colnames(df) <- c(x_names, "pithreshold")
rownames(df) <- names(Y)

# df_param <- data.frame(matrix(nrow=2, ncol=2))
# colnames(df_param) <- c("lambda", "pi")
# rownames(df_param) <- names(Y)

X_train[] <- lapply(X_train, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x)

# Lasso model
t0 <- Sys.time()
rowcount <- 1
for(med in names(Y_train)) {
  print(med)
  index <- 1
  t0 <- Sys.time()
  out <- VariableSelection(
    xdata = as.matrix(X_train),
    ydata = Y_train[med],
    verbose = TRUE,
    penalty.factor = penalty_factor,
    family = "binomial",
    pi_list = seq(0.6, 0.99, by = 0.01))#, maxit=1000000) # Maximum iterations increased to attempt to fix issue with antipsychotic medication
  CalibrationPlot(out)
  selprop <- SelectionProportions(out)
  print(selprop)
  hat_params <- Argmax(out)
  print(hat_params)
  print(rowcount)
  
  # refit_formula <- GetFormula(X_train, med, selected=names(SelectedVariables(out))[SelectedVariables(out)==1])
  # refit_data <- cbind(X_train, ydata=Y_train[med])
  # model <- do.call(stats::glm, args = c(
  #    list(
  #      formula = refit_formula,
  #      data = as.data.frame(refit_data),
  #      family = stats::binomial(link = "logit")
  #    )))
  # 
  # for(coef in names(model$coefficients)[2:(length(model$coefficients)-2)]) {
  #   if(model$coefficients[coef] < 0) {
  #     selprop[coef] <- -1*selprop[coef]
  #   }
  # }

  df[rowcount, ] <- c(selprop, pithreshold=hat_params[2])

  rowcount <- rowcount + 1
  t1 <- Sys.time()
  print(t1 - t0)
}

saveRDS(df, "scm_results/model3_fulloutput_80traindata.rds")
```

#### Plotting

``` r
get_stability_plot("scm_results/model3_fulloutput_80traindata.rds", "Medication variables", flip = TRUE)
```

    ## Using outcomes as id variables

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## ℹ Results may be unexpected or may change in future versions of ggplot2.

![](analysis_files/figure-gfm/l3_plotting-1.png)<!-- -->

### Layer 4

#### Stability selection

``` r
model3_output <- readRDS("scm_results/model3_fulloutput_80traindata.rds")
model3_pi <- model3_output$pithreshold
selected_outcomes <- colnames(model3_output[,grep("^comorb|bl_BMI", colnames(model3_output), value = TRUE)] )[colSums(abs(model3_output[,grep("^comorb|bl_BMI", colnames(model3_output), value = TRUE)] >= model3_pi)) >= 1]

data <-readRDS("../files/imputed_iter9_cc_plswork_onehot_outcomes.rds")

# Deselect further layers; predictors and outcomes
X <- data %>%
  dplyr::select(-c("bl_BMI.0.0", starts_with(c("comorb", "med" ,"bio")), "outcome_incident_case", "outcome_tte"))

# Select relevant outcomes
Y <- data %>% dplyr::select(selected_outcomes)

# Splitting the data into 80% train, 20% test
set.seed(234)
if(VAL) {
  idx_train <- sample(1:nrow(X), 0.4*nrow(X))
} else {
  idx_train <- sample(1:nrow(X), 0.8*nrow(X))

}

X_train <- X[idx_train, ] 
Y_train <- Y[idx_train, ]
X_test <- X[-idx_train, ] 
Y_test <- Y[-idx_train, ]

if(VAL) {
  idx_val <- sample(1:nrow(X_test), 0.5*nrow(X_test))
  X_val <- X_test[idx_val, ]
  Y_val <- Y_test[idx_val, ]
  X_test <- X_test[-idx_val, ]
  Y_test <- Y_test[-idx_val, ]
}

# Setting penalty vector to adjust for age and sex
sex_index <- grep("bl_sex.0.0", colnames(X))
age_index <- grep("bl_age_at_recr.0.0", colnames(X))
penalty_factor <- c(replace(rep(1,ncol(X)), c(sex_index, age_index), 0))
penalty_factor

x_names <- names(X)
x_names <- setdiff(x_names, c("bl_sex.0.0", "bl_age_at_recr.0.0"))

# create df to store results
df <- data.frame(matrix(nrow=dim(Y)[2], ncol=(dim(X)[2]-2)+1))
colnames(df) <- c(x_names, "pithreshold")
rownames(df) <- names(Y)

X_train[] <- lapply(X_train, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x)

# Lasso model
t0 <- Sys.time()
rowcount <- 1
for(i in names(Y_train)) {
  print(i)
  index <- 1
  t0 <- Sys.time()
  if (i == "bl_BMI.0.0") { # gaussian for continuous BMI (the only numeric variable in this layer)
    out <- VariableSelection(
      xdata = as.matrix(X_train),
      ydata = Y_train[i],
      verbose = TRUE,
      penalty.factor = penalty_factor,
      family = "gaussian", 
      pi_list = seq(0.6, 0.99, by = 0.01))
    selprop <- SelectionProportions(out)
    
    # refit_formula <- GetFormula(X_train, i, selected=names(SelectedVariables(out))[SelectedVariables(out)==1])
    # refit_data <- cbind(X_train, ydata=Y_train[i])
    # model <- do.call(stats::lm, args = c(
    #    list(
    #      formula = refit_formula,
    #      data = as.data.frame(refit_data)
    #    )))
    # 
    # for(coef in names(model$coefficients)[2:(length(model$coefficients)-2)]) {
    #   if(model$coefficients[coef] < 0) {
    #     selprop[coef] <- -1*selprop[coef]
    #   }
    # }
  
  } else {
    out <- VariableSelection(
      xdata = as.matrix(X_train),
      ydata = Y_train[i],
      verbose = TRUE,
      penalty.factor = penalty_factor,
      family = "binomial", 
      pi_list = seq(0.6, 0.99, by = 0.01))
    selprop <- SelectionProportions(out)

    # refit_formula <- GetFormula(X_train, i, selected=names(SelectedVariables(out))[SelectedVariables(out)==1])
    #     refit_data <- cbind(X_train, ydata=Y_train[i])
    #     model <- do.call(stats::glm, args = c(
    #        list(
    #          formula = refit_formula,
    #          data = as.data.frame(refit_data),
    #          family = stats::binomial(link = "logit")
    #        )))
    #       
    # for(coef in names(model$coefficients)[2:(length(model$coefficients)-2)]) {
    #   if(model$coefficients[coef] < 0) {
    #     selprop[coef] <- -1*selprop[coef]
    #   }
    # }
  }
  CalibrationPlot(out)
  print(selprop)
  hat_params <- Argmax(out)
  print(hat_params)
  print(rowcount)
  df[rowcount, ] <- c(selprop, pithreshold=hat_params[2])

  rowcount <- rowcount + 1
  t1 <- Sys.time()
  print(t1 - t0)
}

saveRDS(df, "scm_results/model4_fulloutput_80traindata.rds")
```

#### Plotting

``` r
get_stability_plot("scm_results/model4_fulloutput.rds", "Comorbidities", flip = TRUE)
```

    ## Using outcomes as id variables

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## ℹ Results may be unexpected or may change in future versions of ggplot2.

![](analysis_files/figure-gfm/l4_plotting-1.png)<!-- -->

### Layer 5

#### Stability selection

``` r
model4_output <- readRDS("scm_results/model4_fulloutput.rds") # This is the model4_fulloutput_80traindata.rds
model4_pi <- model4_output$pi # This is the pithreshold
selected_outcomes <- colnames(model4_output[,grep("^be", colnames(model4_output), value = TRUE)] )[colSums(abs(model4_output[,grep("^be", colnames(model4_output), value = TRUE)]) >= model4_pi) >= 1]

#data <-readRDS("../files/imputed_iter9_cc_plswork_onehot_outcomes.rds")
data <- readRDS("data/dataset_sensitivity_imputed_encoded.Rds")
#data <- data[1:5000,] # For testing

# Deselecting deeper layers
X <- data %>% dplyr::select(-c("outcome_incident_case", "outcome_tte", "bl_BMI.0.0", 
                        grep("^bio", names(data), value = TRUE),
                        grep("^med", names(data), value = TRUE),
                        grep("^comorb_", names(data), value = TRUE),
                        grep("^be_", names(data), value = TRUE)))

# Select relevant outcomes
Y <- data %>% dplyr::select(selected_outcomes)

# Splitting the data into 80% train, 20% test
set.seed(234)
if(VAL) {
  idx_train <- sample(1:nrow(X), 0.4*nrow(X))
} else {
  idx_train <- sample(1:nrow(X), 0.8*nrow(X))

}
X_train <- X[idx_train, ] 
Y_train <- Y[idx_train, ]
X_test <- X[-idx_train, ] 
Y_test <- Y[-idx_train, ]

if(VAL) {
  idx_val <- sample(1:nrow(X_test), 0.5*nrow(X_test))
  X_val <- X_test[idx_val, ]
  Y_val <- Y_test[idx_val, ]
  X_test <- X_test[-idx_val, ]
  Y_test <- Y_test[-idx_val, ]
}

# Setting penalty vector to adjust for age and sex
sex_index <- grep("bl_sex.0.0", colnames(X))
age_index <- grep("bl_age_at_recr.0.0", colnames(X))
penalty_factor <- c(replace(rep(1,ncol(X)), c(sex_index, age_index), 0))
penalty_factor

x_names <- names(X)
x_names <- setdiff(x_names, c("bl_sex.0.0", "bl_age_at_recr.0.0"))

# create df to store results
df <- data.frame(matrix(nrow=dim(Y)[2], ncol=(dim(X)[2]-2)+1))
colnames(df) <- c(x_names, "pithreshold")
rownames(df) <- names(Y)

X_train[] <- lapply(X_train, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x)

testing <- VariableSelection(
  xdata = as.matrix(X_train),
  ydata = Y_train['be_sleep_score.0.0'],
  verbose = TRUE,
  penalty.factor = penalty_factor,
  family = "gaussian",
  pi_list = seq(0.6, 0.99, by = 0.01))

for(i in names(Y_train)) {
  print(i)
  index <- 1
  t0 <- Sys.time()
  if (nrow(unique(Y_train[i])) > 2) { # Gaussian for continuous variables
    out <- VariableSelection(
      xdata = as.matrix(X_train),
      ydata = Y_train[i],
      verbose = TRUE,
      penalty.factor = penalty_factor,
      family = "gaussian", 
      pi_list = seq(0.6, 0.99, by = 0.01))
    selprop <- SelectionProportions(out)

    # refit_formula <- GetFormula(X_train, i, selected=names(SelectedVariables(out))[SelectedVariables(out)==1])
    # refit_data <- cbind(X_train, ydata=Y_train[i])
    # model <- do.call(stats::lm, args = c(
    #    list(
    #      formula = refit_formula,
    #      data = as.data.frame(refit_data)
    #    )))
    # 
    # for(coef in names(model$coefficients)[2:(length(model$coefficients)-2)]) {
    #   if(model$coefficients[coef] < 0) {
    #     selprop[coef] <- -1*selprop[coef]
    #   }
    # }
  
  } else {
    out <- VariableSelection(
      xdata = as.matrix(X_train),
      ydata = Y_train[i],
      verbose = TRUE,
      penalty.factor = penalty_factor,
      family = "binomial", 
      pi_list = seq(0.6, 0.99, by = 0.01))
    selprop <- SelectionProportions(out)

        refit_formula <- GetFormula(X_train, i, selected=names(SelectedVariables(out))[SelectedVariables(out)==1])
    refit_data <- cbind(X_train, ydata=Y_train[i])
    model <- do.call(stats::glm, args = c(
       list(
         formula = refit_formula,
         data = as.data.frame(refit_data),
         family = stats::binomial(link = "logit")
       )))
    
    for(coef in names(model$coefficients)[2:(length(model$coefficients)-2)]) {
      if(model$coefficients[coef] < 0) {
        selprop[coef] <- -1*selprop[coef]
      }
    }
    
  }
  CalibrationPlot(out)
  print(selprop)
  hat_params <- Argmax(out)
  print(hat_params)
  print(rowcount)
  df[rowcount, ] <- c(selprop, pithreshold=hat_params[2])

  rowcount <- rowcount + 1
  t1 <- Sys.time()
  print(t1 - t0)
}

saveRDS(df, "scm_results/model5_fulloutput_80traindata.rds")

#test <- readRDS("scm_results/model5_fulloutput.rds")
```

#### Plotting

``` r
get_stability_plot("scm_results/model5_fulloutput.rds", "Behavioural variables", flip = TRUE)
```

    ## Using outcomes as id variables

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## ℹ Results may be unexpected or may change in future versions of ggplot2.

![](analysis_files/figure-gfm/l5_plotting-1.png)<!-- -->
