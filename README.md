
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mifa - Multiple Imputation for Exploratory Factor Analysis

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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

### Generate Data Set

Genarate a data set with 5% of the data missing completely at random
(MCAR)

``` r
library(eigeninv)
#> Warning: package 'eigeninv' was built under R version 4.0.3

e.vals <- c(50, 48, 45, 25, 20, 10, 5, 5, 1, 1, 0.5, 0.5, 0.5, 0.1, 0.1) # eigenvalues
P      <- length(e.vals)  # number of items
N      <- 100             # sample size

# generate a covariance matrix with the eigenvalues in e.vals
cov.mat  <- eigeninv::eiginv(evals = e.vals, symmetric = TRUE) 
chol.cov <- t(chol(cov.mat)) # Cholesky decomposition of the covariance matrix

# Generate centered independent normal data
data.ini1     <- matrix(rnorm(N * P), N, P)
mean.data.ini <- apply(data.ini1, 2, mean)
data.ini      <- t(t(data.ini1) - mean.data.ini)

# generate multivariate normal data with the given covariance matrix.
data <- matrix(0, N, P)
for (i in 1:N) {
  data[i, ] <- chol.cov %*% data.ini[i, ]
}

# Create 5-percent missing data with missing completely at random
data.miss <- data
mcar.n.miss <- 0.05

for (i in 1:P) {
  for (j in 1:N) {
    rand.u <- runif(1)
    if (rand.u <= mcar.n.miss) {
      data.miss[j, i] <- NA
    }
  }
}
```

### Run mifa

Run mifa:

``` r
library(mifa)

result.mi <- mifa.cov(data.miss,
  n.factor = 1:10, M = 10, maxit.mi = 5, method.mi = "pmm",
  alpha = 0.05, rep.boot = 50, ci = TRUE
)
#> Warning: Number of logged events: 3
#> Warning: Number of logged events: 5

summary(result.mi)
#>                   Length Class  Mode   
#> cov.mice          225    -none- numeric
#> cov.mice.imp       10    -none- list   
#> exp.var.mice       10    -none- numeric
#> ci.mice.fieller    30    -none- numeric
#> ci.mice.bootstrap  30    -none- numeric
```

The Fieller confidence intervals indicate that 4 factors are enough to
explain around 80% of the variance.

``` r
round(result.mi$ci.mice.fieller, 2)
#>       n.factor Lower Upper
#>  [1,]        1  0.23  0.36
#>  [2,]        2  0.46  0.58
#>  [3,]        3  0.66  0.75
#>  [4,]        4  0.79  0.85
#>  [5,]        5  0.90  0.93
#>  [6,]        6  0.94  0.96
#>  [7,]        7  0.97  0.98
#>  [8,]        8  0.98  0.99
#>  [9,]        9  0.99  0.99
#> [10,]       10  0.99  0.99
```

The estimated covariance matrix based on imputed data is in
`result.mi$cov.mice`, and we can use it to perform exploratory factor
analysis:

``` r
library(psych)
#> Warning: package 'psych' was built under R version 4.0.3
fit <- fa(r = result.mi$cov.mice, n.obs = N, nfactors = 4, rotate = "varimax")

fit
#> Factor Analysis using method =  minres
#> Call: fa(r = result.mi$cov.mice, nfactors = 4, n.obs = N, rotate = "varimax")
#> Standardized loadings (pattern matrix) based upon correlation matrix
#>       MR1   MR3   MR4   MR2   h2    u2 com
#> V1   0.87  0.01 -0.11  0.01 0.76 0.238 1.0
#> V2   0.98  0.15  0.06 -0.01 0.98 0.024 1.1
#> V3   0.97  0.15  0.04 -0.02 0.97 0.032 1.1
#> V4   0.91  0.26  0.15 -0.07 0.93 0.074 1.2
#> V5   0.94  0.24  0.08 -0.05 0.95 0.050 1.1
#> V6   0.93  0.20  0.13 -0.03 0.92 0.077 1.1
#> V7  -0.03  0.13  0.06  0.63 0.42 0.579 1.1
#> V8   0.60 -0.07  0.51  0.01 0.63 0.373 2.0
#> V9   0.71  0.24  0.07 -0.06 0.57 0.426 1.3
#> V10  0.50  0.60  0.15  0.09 0.64 0.356 2.1
#> V11  0.60  0.16  0.09 -0.27 0.47 0.531 1.6
#> V12  0.11  0.27  0.00 -0.29 0.18 0.825 2.3
#> V13  0.07  0.38 -0.07  0.04 0.16 0.841 1.2
#> V14  0.06 -0.01 -0.44  0.01 0.20 0.800 1.0
#> V15  0.21 -0.08  0.27  0.12 0.13 0.865 2.5
#> 
#>                        MR1  MR3  MR4  MR2
#> SS loadings           6.76 0.91 0.64 0.60
#> Proportion Var        0.45 0.06 0.04 0.04
#> Cumulative Var        0.45 0.51 0.55 0.59
#> Proportion Explained  0.76 0.10 0.07 0.07
#> Cumulative Proportion 0.76 0.86 0.93 1.00
#> 
#> Mean item complexity =  1.5
#> Test of the hypothesis that 4 factors are sufficient.
#> 
#> The degrees of freedom for the null model are  105  and the objective function was  14.97 with Chi Square of  1394.85
#> The degrees of freedom for the model are 51  and the objective function was  0.51 
#> 
#> The root mean square of the residuals (RMSR) is  0.02 
#> The df corrected root mean square of the residuals is  0.03 
#> 
#> The harmonic number of observations is  100 with the empirical chi square  8.3  with prob <  1 
#> The total number of observations was  100  with Likelihood Chi Square =  46.11  with prob <  0.67 
#> 
#> Tucker Lewis Index of factoring reliability =  1.008
#> RMSEA index =  0  and the 90 % confidence intervals are  0 0.054
#> BIC =  -188.75
#> Fit based upon off diagonal values = 1
#> Measures of factor score adequacy             
#>                                                    MR1  MR3  MR4  MR2
#> Correlation of (regression) scores with factors   0.99 0.78 0.75 0.71
#> Multiple R square of scores with factors          0.97 0.61 0.56 0.51
#> Minimum correlation of possible factor scores     0.95 0.21 0.11 0.01

fa.diagram(fit)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
`mifa.cov()` provides the confidence intervals when `ci == TRUE`. CIs
can also be calculated separately if desired:

``` r
# get CIs using Fieller's method
result.mi <- mifa.cov(data.miss,
  n.factor = 1:10, M = 10, maxit.mi = 5, method.mi = "pmm",
  alpha = 0.05, rep.boot = 500, ci = FALSE
)

ci.mifa.fieller(result.mi$cov.mice.imp, 1:10, 0.05, N = 100)
#>       n.factor     Lower     Upper
#>  [1,]        1 0.2273153 0.3636066
#>  [2,]        2 0.4594949 0.5835279
#>  [3,]        3 0.6571766 0.7523962
#>  [4,]        4 0.7916119 0.8573226
#>  [5,]        5 0.9040544 0.9337448
#>  [6,]        6 0.9426481 0.9608676
#>  [7,]        7 0.9657265 0.9770542
#>  [8,]        8 0.9826180 0.9879300
#>  [9,]        9 0.9872813 0.9913813
#> [10,]       10 0.9912664 0.9942816


# Get CIs using bootstrapping
ci.mifa.bootstrap(data.miss, 
  n.factor = 1:10, rep.boot = 100, maxit.mi = 5, method.mi = "pmm", 
  alpha = 0.05
)
#> Warning: Number of logged events: 1
#>       n.factor      2.5%     97.5%
#>  [1,]        1 0.2667657 0.3775205
#>  [2,]        2 0.4976026 0.5989957
#>  [3,]        3 0.6930701 0.7681802
#>  [4,]        4 0.8174491 0.8647566
#>  [5,]        5 0.9112542 0.9369631
#>  [6,]        6 0.9479430 0.9633479
#>  [7,]        7 0.9689222 0.9790495
#>  [8,]        8 0.9846687 0.9890757
#>  [9,]        9 0.9892576 0.9926238
#> [10,]       10 0.9931133 0.9952030
```
