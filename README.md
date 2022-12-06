
<!-- README.md is generated from README.Rmd. Please edit that file -->

# causirf

<!-- badges: start -->
<!-- badges: end -->

The goal of causirf is to implement the method proposed in:

- [Revisiting causality using stochastics: 1.
  Theory](https://royalsocietypublishing.org/doi/10.1098/rspa.2021.0835)
- [Revisiting causality using stochastics: 2.
  Applications](https://royalsocietypublishing.org/doi/10.1098/rspa.2021.0836)

for causal analysis.

## Installation

You can install the development version of causirf from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("miguel-conde/causirf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(causirf)
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.1.2
data(temp_co2_data)

obj_causal_temp_co2 <- est_irf(temp_co2_data, "temp", "co2", J = 20, lambda = 10)

autoplot(obj_causal_temp_co2)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
evr(obj_causal_temp_co2)
#>           co2
#> co2 0.1223941
```
