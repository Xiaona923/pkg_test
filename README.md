    remotes::install_github("Xiaona923/pkg_test", force = TRUE)

    ## generics (0.1.3 -> 0.1.4) [CRAN]

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/xf/l79029316mxg61sybqflzb_40000gn/T//RtmpteU9pd/downloaded_packages
    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##   ✔  checking for file ‘/private/var/folders/xf/l79029316mxg61sybqflzb_40000gn/T/RtmpteU9pd/remotes1454025a807fd/Xiaona923-pkg_test-3cbbb18/DESCRIPTION’
    ##   ─  preparing ‘lsurvROC’:
    ##    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##   ─  building ‘lsurvROC_0.0.1.tar.gz’
    ##      
    ## 

    library(lsurvROC)

## Introduction

We propose a flexible regression framework to evaluate the prognostic
capability of longitudinal biomarker with survival outcome accounting
for the covariate and biomaker measurement time. Here is an example
using a simulated dataset to estimate the biomarker threshold at a
specified specificity level along with the corresponding sensitivity,
and to construct a covariate- and measurement time-specific ROC curve.

## Read in example data

    data("example_data")

## Estimate time-varying coefficients

-   dat.long: long format data, each subject may have multiple biomarker
    records
-   dat.short: short format data, each subject only has one record,
    including observed event time, event indicator, covariates (eg. Z,
    Zcont in this example)
-   cutoff.type.basis: type of basis function to estimate biomarker
    threshold, (FP: Fractional polynomial, BS: B Splines, linear: linear
    basis)
-   sens.type.basis: type of basis function to estimate sensitivity
    level, (FP: Fractional polynomial, BS: B Splines, linear: linear
    basis)
-   covariate1: a vector of covariate names to estimate the biomarker
    threshold
-   covariate2: a vector of covariate names to estimate the sensitivity
    level
-   tau: target specificity level, could be a single value or a set of
    values between 0 and 1
-   time.window: a time window for biomarker evaluation
-   nResap: number of perturbation resampling

<!-- -->

    par(mfrow=c(1,3))
    res = lsurvROC(dat.long = example_data$data.long, 
                  dat.short = example_data$data.short,
                  cutoff.type.basis = "FP",
                  sens.type.basis = "FP", 
                  covariate1 = c("Z", "Zcont"), 
                  covariate2 = c("Z", "Zcont"), 
                  tau = c(0.7, 0.8, 0.9), 
                  time.window = 1, 
                  nResap = 50, 
                  show_plots = TRUE
                  )

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)![](README_files/figure-markdown_strict/unnamed-chunk-3-2.png)

## Conditional ROC curve

    #estimate thresholds and sensitivity levels
    model.results <- lsurvROC(dat.long = example_data$data.long, 
                              dat.short = example_data$data.short,
                              cutoff.type.basis = "FP",
                              sens.type.basis = "FP", 
                              covariate1 = c("Z", "Zcont"), 
                              covariate2 = c("Z", "Zcont"), 
                              tau = seq(0.1, 0.9, 0.05), 
                              time.window = 1, 
                              nResap = 50, 
                              show_plots = FALSE
                              )
    #get monotoned ROC curve and AUC value
    ROC.results <- plot_lsurvROC( model = model.results$models, 
                                  my.newdat = data.frame(vtime = 0.5, Z = 1, Zcont = 0.25), 
                                  tau = seq(0.1, 0.9, 0.05),
                                  basis = "FP", 
                                  tol = 1e3, 
                                  add = FALSE,
                                  col = "black", 
                                  lty = 1)

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    ROC.results$AUC

    ## [1] 0.847306
