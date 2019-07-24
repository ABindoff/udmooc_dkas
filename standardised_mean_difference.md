standardised mean difference
================
Bindoff, A.
24/07/2019

``` r
library(dplyr)
load("dkas_2016_2017")

library(tableone)
```

    ## Warning: package 'tableone' was built under R version 3.6.1

``` r
smd <- CreateTableOne(vars = c('score'), strata = 'time', data = df0)
print(smd, smd = TRUE)
```

    ##                    Stratified by time
    ##                     pre          post         p      test SMD   
    ##   n                  4894         4894                          
    ##   score (mean (SD)) 33.56 (9.23) 43.44 (6.09) <0.001       1.263
