
<!-- README.md is generated from README.Rmd. Please edit that file -->

# langAssessR

<!-- badges: start -->
<!-- badges: end -->

R package for bias-aware, explainable, and generalizable language-based
assessment with built-in contamination testing, cross-context
validation, and fairness auditing.

## Installation

You can install the development version of langAssessR from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("kimberlyjg/langAssessR")
```

## Example

This is a basic example showing the core functionality:

``` r
library(langAssessR)

# Simulate data
sim <- simulate_lang_data(n = 200, n_sites = 3)

# Extract features from transcripts
features <- la_features(sim$transcripts$narrative)
head(features)
#>     id char_len word_count type_token_ratio mean_word_len
#> 1 P001       33          3                1        10.333
#> 2 P002       32          3                1        10.000
#> 3 P003       32          3                1        10.000
#> 4 P004       32          3                1        10.000
#> 5 P005       32          3                1        10.000
#> 6 P006       32          3                1        10.000

# Fit a model
model <- la_fit(features[,-1], sim$participants$y_bin)

# Make predictions and evaluate
pred <- la_predict(model, features[,-1])
eval <- la_eval(pred, sim$participants$y_bin)
eval$summary
#>      metric value
#> 1     AUROC 0.471
#> 2  Accuracy 0.725
#> 3 Precision    NA
#> 4    Recall 0.000
#> 5        F1    NA

# Check cross-site generalization
cv <- cross_context_cv(features[,-1], sim$participants$y_bin, sim$participants$site)
cv$site_estimates
#>    site  estimate       lcl       ucl
#> 1 Site3 0.4776596 0.4276596 0.5276596
#> 2 Site2 0.5571581 0.5071581 0.6071581
#> 3 Site1 0.4168798 0.3668798 0.4668798
```
