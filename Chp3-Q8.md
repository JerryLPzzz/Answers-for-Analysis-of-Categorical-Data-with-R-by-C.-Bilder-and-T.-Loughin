Chp3 Q8
================

``` r
#Entering data into program
c.table<-array(data = c(5, 2200, 17, 6190, 3, 530, 2, 1236, 3, 468),
               dim=c(2,5), 
               dimnames = list(Hepatitis = c("presence", "absence"),
                               occupation = c("Exposure Prone", 
                                              "Fluid contact", 
                                              "Lab staff", 
                                              "Patient contact", 
                                              "No patient contact")))

#load necessary package for independent test
library(package = vcd)
```

    ## Loading required package: grid

``` r
assocstats(x = c.table)
```

    ##                     X^2 df P(> X^2)
    ## Likelihood Ratio 3.7350  4  0.44305
    ## Pearson          4.5043  4  0.34204
    ## 
    ## Phi-Coefficient   : NA 
    ## Contingency Coeff.: 0.021 
    ## Cramer's V        : 0.021

``` r
#p-values from both Pearson chi-square and LR tests are very large.
#Thus, we conclude that there is no evidence against independence.
#The result found here are similar to the data analysis in Chapter 2.
#Because it is the same as tests for the equality of two binomial 
# success probabilities, which is equivalent to a test for independence 
# in a product multinomial model with I=J=2.
```
