Chp2 Q4
================

a). The potential problem: scientists may learn something new from each
launch and make improvement accordingly. This debunks the assumption of
independence. This assumption is necessary, because if the potential
problem is true, it means that the data collected does not capture
everything and inferences based on such data would be misleading.

b). The estimated logistic regression model is: logit(pi.hat) = 2.5202 -
0.0983Temp + 0.0085Pressure

``` r
challenger <- read.table(file = "challenger.csv", 
                          header = TRUE, sep = ",")

challenger.fit <- glm(formula = O.ring/Number ~ Temp + Pressure, 
                      family = binomial, 
                      data = challenger) 
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

``` r
#there is nothing wrong with this warning message, glm() is just picky when it comes to specifying binomial models

summary(challenger.fit)
```

    ## 
    ## Call:
    ## glm(formula = O.ring/Number ~ Temp + Pressure, family = binomial, 
    ##     data = challenger)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.42299  -0.26267  -0.21671  -0.06634   0.95601  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  2.520195   8.540918   0.295    0.768
    ## Temp        -0.098297   0.109959  -0.894    0.371
    ## Pressure     0.008484   0.018806   0.451    0.652
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4.0384  on 22  degrees of freedom
    ## Residual deviance: 2.7576  on 20  degrees of freedom
    ## AIC: 9.2286
    ## 
    ## Number of Fisher Scoring iterations: 6

c). According to LRTs, both explanatory variables are not important

``` r
library(car)
```

    ## Loading required package: carData

``` r
Anova(challenger.fit, test = "LR") #The Anova() function produces LRTs using the test = "LR" argument value
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: O.ring/Number
    ##          LR Chisq Df Pr(>Chisq)
    ## Temp      0.86397  1     0.3526
    ## Pressure  0.25678  1     0.6123

``` r
#there is nothing wrong with this warning message
```

d). Remove variable Pressure from the model was done because it has a
p-value of 0.6123. In this case, we fail to reject null hypothesis,
cannot conclude alternative hypothesis that variable Pressure is
significant. The potential problem with removing variable Pressure is
that there will be only one variable Temp left in this model and this
variable may not capture everything that is influencial to the response
variable.
