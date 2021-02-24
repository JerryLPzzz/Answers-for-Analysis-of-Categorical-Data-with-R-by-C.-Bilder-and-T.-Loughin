Chp2 Q19
================

Compute Wald test for hypothesis test

``` r
#load provided dataframe
HCW <- read.table(file = "healthcare_worker.csv",
                        header = TRUE, 
                        sep = ",")

#fit a logistic regression model
logistic.fit <- glm(formula = Hepatitis/Size ~ Occup.group, 
                    weights = Size,
                    family = binomial,
                    data = HCW)
#compute summary table
summary(logistic.fit)$coefficients
```

    ##                                 Estimate Std. Error     z value     Pr(>|z|)
    ## (Intercept)                   -6.0867747  0.4477215 -13.5950020 4.287492e-42
    ## Occup.groupFluid contact       0.1892977  0.5093522   0.3716441 7.101579e-01
    ## Occup.groupLab staff           0.9125100  0.7318980   1.2467721 2.124811e-01
    ## Occup.groupNo patient contact  1.0369187  0.7320687   1.4164226 1.566518e-01
    ## Occup.groupPatient contact    -0.3397137  0.8374148  -0.4056696 6.849854e-01

``` r
#According to p-value based on Wald test (0.710, 0.212, 0.157, 0.685),
# there is no evidence of an occupational group effect on hepatitis status.
#We may explore Odds Ratio and their Confidence Interval for more explanation.
```

Compute Odds Ratios and their Confidence Intervals

``` r
exp(logistic.fit$coefficients[2:5]) #calculate OR
```

    ##      Occup.groupFluid contact          Occup.groupLab staff 
    ##                     1.2084006                     2.4905660 
    ## Occup.groupNo patient contact    Occup.groupPatient contact 
    ##                     2.8205128                     0.7119741

``` r
exp(confint.default(object = logistic.fit, 
                    parm = c(2:5), 
                    level = 0.95)) #Wald CI for OR
```

    ##                                   2.5 %    97.5 %
    ## Occup.groupFluid contact      0.4452968  3.279233
    ## Occup.groupLab staff          0.5933424 10.454199
    ## Occup.groupNo patient contact 0.6717227 11.843120
    ## Occup.groupPatient contact    0.1379289  3.675132

``` r
#All confidence intervals for odds ratios have 1 within the interval, 
# which suggests that there is insufficient evidence to indicate an 
# actual evidence of an occupational group effect on hepatitis status.
#It is preferable to say that there is not sufficient evidence of an effect.
```
