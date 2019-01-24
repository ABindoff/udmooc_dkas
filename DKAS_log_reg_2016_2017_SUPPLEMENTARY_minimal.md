DKAS, multilevel logistic regression - 2016 & 2017 UD MOOCs
================
Bindoff, A., Eccleston, C.

2019-01-24

### Logistic regression

A dummy variable was code denoting scores \>=45, being the 90th
percentile score at pre-test.

Inter-quartile range of scores at **pre-test**

``` r
quantile(filter(df0, time == "pre")$raw_score, c(0.25, 0.5, 0.75, 0.90))
```

    ##  25%  50%  75%  90% 
    ## 27.0 34.5 41.0 45.0

Fit the logistic regression model, obtain p-values using a
likelihood-ratio test.

``` r
library(optimx)
df0$score <- df0$raw_score >= 45 
m2 <- glmer(score ~ time + education + exposure + exposure:time +
              (1|subjectid) + (1|year), data = df0,
             family = binomial(link = logit), 
          control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))

drop1(m2, test = "Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## score ~ time + education + exposure + exposure:time + (1 | subjectid) + 
    ##     (1 | year)
    ##               Df    AIC     LRT   Pr(Chi)    
    ## <none>           9534.2                      
    ## education      3 9758.5 230.221 < 2.2e-16 ***
    ## time:exposure  7 9598.6  78.342 2.999e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Pairwise differences estimated using Tukey contrasts to find the minimum
set of pairwise differences, p-values adjusted using Tukey method to
correct for multiple
comparisons.

``` r
summary(lsmeans(m2, list(revpairwise ~ education|time)), type = "response")
```

    ## $`lsmeans of education | time`
    ## time = pre:
    ##  education                        prob          SE df  asymp.LCL  asymp.UCL
    ##  High school and below      0.02023997 0.005371742 NA 0.01200239 0.03393701
    ##  Pre-tertiary qualification 0.03463249 0.008291708 NA 0.02158774 0.05511571
    ##  University degree          0.06075310 0.013834737 NA 0.03866255 0.09422832
    ##  Honours & postgraduate     0.09524745 0.020801530 NA 0.06155511 0.14454095
    ## 
    ## time = post:
    ##  education                        prob          SE df  asymp.LCL  asymp.UCL
    ##  High school and below      0.32541845 0.053363403 NA 0.23051100 0.43720008
    ##  Pre-tertiary qualification 0.45585303 0.055584199 NA 0.35063596 0.56516402
    ##  University degree          0.60166501 0.053794490 NA 0.49312003 0.70105706
    ##  Honours & postgraduate     0.71084356 0.047065085 NA 0.61080512 0.79384686
    ## 
    ## Results are averaged over the levels of: exposure 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the logit scale 
    ## 
    ## $`pairwise differences of contrast, time | time`
    ## time = pre:
    ##  contrast                                            odds.ratio        SE df z.ratio p.value
    ##  Pre-tertiary qualification - High school and below    1.736604 0.2195331 NA   4.366  0.0001
    ##  University degree - High school and below             3.131112 0.4074985 NA   8.770  <.0001
    ##  University degree - Pre-tertiary qualification        1.803008 0.1513332 NA   7.023  <.0001
    ##  Honours & postgraduate - High school and below        5.096047 0.7079319 NA  11.723  <.0001
    ##  Honours & postgraduate - Pre-tertiary qualification   2.934489 0.2782695 NA  11.353  <.0001
    ##  Honours & postgraduate - University degree            1.627552 0.1502547 NA   5.276  <.0001
    ## 
    ## time = post:
    ##  contrast                                            odds.ratio        SE df z.ratio p.value
    ##  Pre-tertiary qualification - High school and below    1.736604 0.2195331 NA   4.366  0.0001
    ##  University degree - High school and below             3.131112 0.4074985 NA   8.770  <.0001
    ##  University degree - Pre-tertiary qualification        1.803008 0.1513332 NA   7.023  <.0001
    ##  Honours & postgraduate - High school and below        5.096047 0.7079319 NA  11.723  <.0001
    ##  Honours & postgraduate - Pre-tertiary qualification   2.934489 0.2782695 NA  11.353  <.0001
    ##  Honours & postgraduate - University degree            1.627552 0.1502547 NA   5.276  <.0001
    ## 
    ## Results are averaged over the levels of: exposure 
    ## P value adjustment: tukey method for comparing a family of 4 estimates 
    ## Tests are performed on the log odds ratio scale

``` r
summary(lsmeans(m2, list(revpairwise ~ time|exposure)), type = "response")
```

    ## $`lsmeans of time | exposure`
    ## exposure = No exposure:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.01298992 0.005065785 NA 0.006030303 0.02775737
    ##  post 0.35322260 0.058320022 NA 0.248759268 0.47388207
    ## 
    ## exposure = Educ only:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.03453014 0.017399108 NA 0.012695969 0.09047319
    ##  post 0.45248083 0.087130638 NA 0.293184027 0.62214571
    ## 
    ## exposure = Family only:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.01555761 0.005097115 NA 0.008164079 0.02944804
    ##  post 0.49062105 0.059446986 NA 0.376662148 0.60556312
    ## 
    ## exposure = Work only:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.04230531 0.010615456 NA 0.025752210 0.06874766
    ##  post 0.42640329 0.056529738 NA 0.320921404 0.53903507
    ## 
    ## exposure = Educ and family:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.07258827 0.021051675 NA 0.040679423 0.12623285
    ##  post 0.67695368 0.059264760 NA 0.551974731 0.78090732
    ## 
    ## exposure = Educ and work:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.11992642 0.026590303 NA 0.076780830 0.18252360
    ##  post 0.57274998 0.058001626 NA 0.457234918 0.68084095
    ## 
    ## exposure = Family and work:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.03807469 0.010000510 NA 0.022652812 0.06331551
    ##  post 0.53652129 0.058047588 NA 0.422826760 0.64654155
    ## 
    ## exposure = Educ, family and work:
    ##  time       prob          SE df   asymp.LCL  asymp.UCL
    ##  pre  0.15510919 0.031473841 NA 0.102864445 0.22716986
    ##  post 0.68181141 0.050368263 NA 0.576167946 0.77156264
    ## 
    ## Results are averaged over the levels of: education 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the logit scale 
    ## 
    ## $`pairwise differences of contrast, exposure | exposure`
    ## exposure = No exposure:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre  41.496247 14.320844 NA  10.795  <.0001
    ## 
    ## exposure = Educ only:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre  23.106873 12.051887 NA   6.021  <.0001
    ## 
    ## exposure = Family only:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre  60.947056 16.362252 NA  15.309  <.0001
    ## 
    ## exposure = Work only:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre  16.828527  2.723317 NA  17.445  <.0001
    ## 
    ## exposure = Educ and family:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre  26.773201  7.184281 NA  12.251  <.0001
    ## 
    ## exposure = Educ and work:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre   9.837553  1.554454 NA  14.469  <.0001
    ## 
    ## exposure = Family and work:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre  29.245708  5.482072 NA  18.009  <.0001
    ## 
    ## exposure = Educ, family and work:
    ##  contrast   odds.ratio        SE df z.ratio p.value
    ##  post - pre  11.671931  1.560067 NA  18.384  <.0001
    ## 
    ## Results are averaged over the levels of: education 
    ## Tests are performed on the log odds ratio scale

``` r
summary(lsmeans(m2, list(revpairwise ~ exposure|time)), type = "response")
```

    ## $`lsmeans of exposure | time`
    ## time = pre:
    ##  exposure                    prob          SE df   asymp.LCL  asymp.UCL
    ##  No exposure           0.01298992 0.005065785 NA 0.006030303 0.02775737
    ##  Educ only             0.03453014 0.017399108 NA 0.012695969 0.09047319
    ##  Family only           0.01555761 0.005097115 NA 0.008164079 0.02944804
    ##  Work only             0.04230531 0.010615456 NA 0.025752210 0.06874766
    ##  Educ and family       0.07258827 0.021051675 NA 0.040679423 0.12623285
    ##  Educ and work         0.11992642 0.026590303 NA 0.076780830 0.18252360
    ##  Family and work       0.03807469 0.010000510 NA 0.022652812 0.06331551
    ##  Educ, family and work 0.15510919 0.031473841 NA 0.102864445 0.22716986
    ## 
    ## time = post:
    ##  exposure                    prob          SE df   asymp.LCL  asymp.UCL
    ##  No exposure           0.35322260 0.058320022 NA 0.248759268 0.47388207
    ##  Educ only             0.45248083 0.087130638 NA 0.293184027 0.62214571
    ##  Family only           0.49062105 0.059446986 NA 0.376662148 0.60556312
    ##  Work only             0.42640329 0.056529738 NA 0.320921404 0.53903507
    ##  Educ and family       0.67695368 0.059264760 NA 0.551974731 0.78090732
    ##  Educ and work         0.57274998 0.058001626 NA 0.457234918 0.68084095
    ##  Family and work       0.53652129 0.058047588 NA 0.422826760 0.64654155
    ##  Educ, family and work 0.68181141 0.050368263 NA 0.576167946 0.77156264
    ## 
    ## Results are averaged over the levels of: education 
    ## Confidence level used: 0.95 
    ## Intervals are back-transformed from the logit scale 
    ## 
    ## $`pairwise differences of contrast, time | time`
    ## time = pre:
    ##  contrast                                odds.ratio         SE df z.ratio p.value
    ##  Educ only - No exposure                  2.7175331 1.55102641 NA   1.752  0.6530
    ##  Family only - No exposure                1.2007916 0.48453374 NA   0.453  0.9998
    ##  Family only - Educ only                  0.4418682 0.23354928 NA  -1.545  0.7827
    ##  Work only - No exposure                  3.3564715 1.16634628 NA   3.485  0.0116
    ##  Work only - Educ only                    1.2351171 0.60307370 NA   0.432  0.9999
    ##  Work only - Family only                  2.7952158 0.77013527 NA   3.731  0.0047
    ##  Educ and family - No exposure            5.9471519 2.32892940 NA   4.553  0.0001
    ##  Educ and family - Educ only              2.1884377 1.13389212 NA   1.512  0.8015
    ##  Educ and family - Family only            4.9526929 1.61654031 NA   4.902  <.0001
    ##  Educ and family - Work only              1.7718464 0.45565386 NA   2.224  0.3369
    ##  Educ and work - No exposure             10.3540694 3.58908406 NA   6.743  <.0001
    ##  Educ and work - Educ only                3.8100987 1.85252451 NA   2.751  0.1077
    ##  Educ and work - Family only              8.6227033 2.35338779 NA   7.894  <.0001
    ##  Educ and work - Work only                3.0848078 0.55869213 NA   6.220  <.0001
    ##  Educ and work - Educ and family          1.7410131 0.43687055 NA   2.210  0.3457
    ##  Family and work - No exposure            3.0075316 1.07391092 NA   3.084  0.0427
    ##  Family and work - Educ only              1.1067139 0.54665436 NA   0.205  1.0000
    ##  Family and work - Family only            2.5046242 0.71645474 NA   3.210  0.0290
    ##  Family and work - Work only              0.8960397 0.17995012 NA  -0.547  0.9994
    ##  Family and work - Educ and family        0.5057096 0.13488819 NA  -2.556  0.1723
    ##  Family and work - Educ and work          0.2904686 0.05705706 NA  -6.294  <.0001
    ##  Educ, family and work - No exposure     13.9492902 4.75268124 NA   7.735  <.0001
    ##  Educ, family and work - Educ only        5.1330709 2.46696778 NA   3.403  0.0153
    ##  Educ, family and work - Family only     11.6167458 3.06001136 NA   9.310  <.0001
    ##  Educ, family and work - Work only        4.1559388 0.70336288 NA   8.417  <.0001
    ##  Educ, family and work - Educ and family  2.3455413 0.55875768 NA   3.579  0.0083
    ##  Educ, family and work - Educ and work    1.3472278 0.21013845 NA   1.911  0.5433
    ##  Educ, family and work - Family and work  4.6381192 0.84878155 NA   8.384  <.0001
    ## 
    ## time = post:
    ##  contrast                                odds.ratio         SE df z.ratio p.value
    ##  Educ only - No exposure                  1.5132379 0.47488900 NA   1.320  0.8916
    ##  Family only - No exposure                1.7636465 0.31330228 NA   3.194  0.0305
    ##  Family only - Educ only                  1.1654787 0.34811527 NA   0.513  0.9996
    ##  Work only - No exposure                  1.3611947 0.22549482 NA   1.861  0.5777
    ##  Work only - Educ only                    0.8995246 0.26462590 NA  -0.360  1.0000
    ##  Work only - Family only                  0.7718070 0.10809993 NA  -1.849  0.5860
    ##  Educ and family - No exposure            3.8370769 0.85244428 NA   6.053  <.0001
    ##  Educ and family - Educ only              2.5356734 0.82362083 NA   2.865  0.0799
    ##  Educ and family - Family only            2.1756497 0.42692150 NA   3.961  0.0019
    ##  Educ and family - Work only              2.8189039 0.54120546 NA   5.398  <.0001
    ##  Educ and work - No exposure              2.4546487 0.43312919 NA   5.089  <.0001
    ##  Educ and work - Educ only                1.6221169 0.48308842 NA   1.624  0.7357
    ##  Educ and work - Family only              1.3918031 0.20513309 NA   2.243  0.3258
    ##  Educ and work - Work only                1.8033047 0.24717575 NA   4.302  0.0005
    ##  Educ and work - Educ and family          0.6397184 0.12407579 NA  -2.303  0.2916
    ##  Family and work - No exposure            2.1196469 0.36370530 NA   4.378  0.0003
    ##  Family and work - Educ only              1.4007361 0.41304117 NA   1.143  0.9473
    ##  Family and work - Family only            1.2018548 0.17002318 NA   1.300  0.8993
    ##  Family and work - Work only              1.5571960 0.20422266 NA   3.377  0.0168
    ##  Family and work - Educ and family        0.5524119 0.10476119 NA  -3.129  0.0372
    ##  Family and work - Educ and work          0.8635235 0.11941657 NA  -1.061  0.9647
    ##  Educ, family and work - No exposure      3.9236116 0.67614217 NA   7.933  <.0001
    ##  Educ, family and work - Educ only        2.5928585 0.75940841 NA   3.253  0.0253
    ##  Educ, family and work - Family only      2.2247155 0.30520121 NA   5.829  <.0001
    ##  Educ, family and work - Work only        2.8824765 0.37660088 NA   8.103  <.0001
    ##  Educ, family and work - Educ and family  1.0225523 0.18697422 NA   0.122  1.0000
    ##  Educ, family and work - Educ and work    1.5984412 0.21212768 NA   3.534  0.0097
    ##  Educ, family and work - Family and work  1.8510685 0.23494154 NA   4.852  <.0001
    ## 
    ## Results are averaged over the levels of: education 
    ## P value adjustment: tukey method for comparing a family of 8 estimates 
    ## Tests are performed on the log odds ratio scale
