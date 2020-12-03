
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mifa - Multiple Imputation for Exploratory Factor Analysis

<!-- badges: start -->
<!-- badges: end -->

This package uses multiple imputation to estimate the covariance matrix
of incomplete data. An exploratory factor analysis then can be applied
on this estimated covariance matrix. It also provides Fieller and
bootstrap confidence intervals for the proportion of explained variance
using different number of factors.

#### For more information, see:

Nassiri, V., Lovik, A., Molenberghs, G. *et al.* On using multiple
imputation for exploratory factor analysis of incomplete data. *Behav
Res* 50, 501–517 (2018). <https://doi.org/10.3758/s13428-017-1013-4>

## Installation

You can install the the release version of mifa from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vahidnassiri/mifa")
```

## Usage

``` r
library(mifa)
```
