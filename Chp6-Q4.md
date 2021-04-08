Chp6 Q4
================

a).

``` r
set1 <- read.csv(file = "C:\\Users\\Moon\\Desktop\\New folder\\HIVKenya.csv")
is.factor(set1$marital.status)#check if marital.status is considered a categorical variable
```

    ## [1] FALSE

``` r
set1$marital.status = as.factor(set1$marital.status)#set marital.status as a categorical variable
is.factor(set1$marital.status)#check again if marital.status is considered a categorical variable
```

    ## [1] TRUE

``` r
mod.fit <- glm(formula = hiv ~ ., data = set1, 
               family = binomial(link = logit))#estimate according to the question
summary(mod.fit)
```

    ## 
    ## Call:
    ## glm(formula = hiv ~ ., family = binomial(link = logit), data = set1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6547  -0.4223  -0.3542  -0.2716   2.6641  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)     -2.755933   0.950464  -2.900  0.00374 **
    ## parity          -0.175151   0.157301  -1.113  0.26551   
    ## age             -0.003685   0.045665  -0.081  0.93568   
    ## marital.status2 -0.680614   0.602100  -1.130  0.25831   
    ## marital.status3 -0.268751   0.408700  -0.658  0.51081   
    ## marital.status4  1.156981   1.214855   0.952  0.34091   
    ## marital.status5  3.027842   1.337184   2.264  0.02355 * 
    ## education        0.359447   0.209167   1.718  0.08571 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 355.39  on 667  degrees of freedom
    ## Residual deviance: 335.16  on 660  degrees of freedom
    ## AIC: 351.16
    ## 
    ## Number of Fisher Scoring iterations: 6

b).

``` r
library(car)
```

    ## Loading required package: carData

``` r
Anova(mod.fit)#LRTs on each explanatory variables
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: hiv
    ##                LR Chisq Df Pr(>Chisq)  
    ## parity           1.2943  1    0.25526  
    ## age              0.0065  1    0.93564  
    ## marital.status   9.2309  4    0.05558 .
    ## education        2.9535  1    0.08569 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

It seems like there is a marginal evidence that marital status and
education are significant.

c).

``` r
mod.empty <- glm(formula = hiv ~ 1, data = set1, 
               family = binomial(link = logit))
step.sel <- step(object = mod.empty, scope = list(upper = mod.fit), 
                 k = log(nrow(set1)), trace = TRUE)
```

    ## Start:  AIC=361.89
    ## hiv ~ 1
    ## 
    ##                  Df Deviance    AIC
    ## + education       1   347.27 360.28
    ## <none>                355.39 361.89
    ## + parity          1   349.91 362.92
    ## + age             1   354.45 367.46
    ## + marital.status  4   343.11 375.63
    ## 
    ## Step:  AIC=360.28
    ## hiv ~ education
    ## 
    ##                  Df Deviance    AIC
    ## <none>                347.27 360.28
    ## - education       1   355.39 361.89
    ## + parity          1   344.41 363.92
    ## + age             1   345.97 365.48
    ## + marital.status  4   338.07 377.10

``` r
#the best fitting model for the data only includes education as its 
#explanatory variable according to the stepwise selection method

best.fit <- glm(formula = hiv ~ education, data = set1, 
               family = binomial(link = logit)) #fit the best fitting model

exp(best.fit$coefficients[2]) #odds ratio
```

    ## education 
    ##  1.653851

to interpret the given odds ratio, one unit increase in education (Ex.
from none(1) to primary(2)) are 65% more likely to have hiv
