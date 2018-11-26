
<!-- README.md is generated from README.Rmd. Please edit that file -->
secretsauce
===========

Herein lies a motley collection of helper functions that I use in my day-to-day as an analyst in the insurance industry. If you analyse data concerning people and money, you may get some use out of this package too.

**Why *secret sauce*?**

As R packages are often loaded at the start of R scripts, calling `library(secretsauce)` is like garneshing your code with that little something extra... \#spicycode

Installation
------------

As secretsauce is primarily for my personal use it is not on CRAN (and probably never will be). Fear not however, you can install it directly from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("liam-c-smith/secretsauce")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub!
