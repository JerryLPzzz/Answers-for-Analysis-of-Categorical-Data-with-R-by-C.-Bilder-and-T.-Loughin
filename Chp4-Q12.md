Chp 4 Q12
================

a).

``` r
#create data frame
alldata <- read.table("C:\\Users\\Moon\\Desktop\\New folder\\PolIdeolData.csv", 
                      sep = ",", header = TRUE)

lin.score1 <- c(rep(x = c(1,2,3,4,5), times = 4))
lin.score2 <- c(rep(x = c(0,2,3,4,6), times = 4))
extrm.score <- c(rep(x = c(2,1,0,1,2), times = 4))
alldata <- data.frame(alldata, lin.score1, lin.score2, extrm.score)

#try score set 0-2-3-4-6
mod.homo.lin2.PI <- glm(formula = count ~ gender*party + gender*ideol + party*lin.score2, 
                        family = poisson(link = "log"), data = alldata)
summary(mod.homo.lin2.PI)
```

    ## 
    ## Call:
    ## glm(formula = count ~ gender * party + gender * ideol + party * 
    ##     lin.score2, family = poisson(link = "log"), data = alldata)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.63195  -0.53691  -0.00133   0.53254   1.24421  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        4.72991    0.08161  57.961  < 2e-16 ***
    ## genderM           -0.71435    0.13646  -5.235 1.65e-07 ***
    ## partyR            -1.09888    0.16418  -6.693 2.19e-11 ***
    ## ideolSC           -1.33155    0.14679  -9.071  < 2e-16 ***
    ## ideolSL           -0.88108    0.13609  -6.474 9.53e-11 ***
    ## ideolVC           -1.42090    0.15711  -9.044  < 2e-16 ***
    ## ideolVL           -0.89039    0.14938  -5.961 2.51e-09 ***
    ## lin.score2              NA         NA      NA       NA    
    ## genderM:partyR     0.29337    0.14510   2.022  0.04320 *  
    ## genderM:ideolSC    0.56786    0.21382   2.656  0.00791 ** 
    ## genderM:ideolSL    0.22804    0.21512   1.060  0.28912    
    ## genderM:ideolVC    0.43398    0.20143   2.154  0.03120 *  
    ## genderM:ideolVL    0.37663    0.22689   1.660  0.09691 .  
    ## partyR:lin.score2  0.29223    0.04271   6.842 7.80e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 255.148  on 19  degrees of freedom
    ## Residual deviance:  12.954  on  7  degrees of freedom
    ## AIC: 147.38
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
library(car)
```

    ## Loading required package: carData

``` r
Anova(mod.homo.lin2.PI)
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: count
    ##                  LR Chisq Df Pr(>Chisq)    
    ## gender             20.637  1  5.551e-06 ***
    ## party               0.528  1    0.46736    
    ## ideol             161.315  3  < 2.2e-16 ***
    ## lin.score2                 0               
    ## gender:party        4.095  1    0.04300 *  
    ## gender:ideol        9.530  4    0.04913 *  
    ## party:lin.score2   50.847  1  9.987e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#try score set 2-1-0-1-2
mod.extrm.score.PI <- glm(formula = count ~ gender*party + gender*ideol + party*extrm.score, 
                        family = poisson(link = "log"), data = alldata)
summary(mod.extrm.score.PI)
```

    ## 
    ## Call:
    ## glm(formula = count ~ gender * party + gender * ideol + party * 
    ##     extrm.score, family = poisson(link = "log"), data = alldata)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -3.08647  -1.97307  -0.00652   1.72582   2.74892  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         4.72882    0.08729  54.172  < 2e-16 ***
    ## genderM            -0.72734    0.13617  -5.341 9.23e-08 ***
    ## partyR             -0.21974    0.11708  -1.877  0.06055 .  
    ## ideolSC            -1.20792    0.14976  -8.066 7.28e-16 ***
    ## ideolSL            -1.01757    0.14012  -7.262 3.80e-13 ***
    ## ideolVC            -0.97032    0.15240  -6.367 1.93e-10 ***
    ## ideolVL            -1.22521    0.16388  -7.476 7.64e-14 ***
    ## extrm.score              NA         NA      NA       NA    
    ## genderM:partyR      0.31795    0.14114   2.253  0.02428 *  
    ## genderM:ideolSC     0.58620    0.21364   2.744  0.00607 ** 
    ## genderM:ideolSL     0.20396    0.21496   0.949  0.34270    
    ## genderM:ideolVC     0.48926    0.19947   2.453  0.01418 *  
    ## genderM:ideolVL     0.31128    0.22525   1.382  0.16699    
    ## partyR:extrm.score  0.03765    0.08370   0.450  0.65289    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 255.148  on 19  degrees of freedom
    ## Residual deviance:  63.598  on  7  degrees of freedom
    ## AIC: 198.03
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
Anova(mod.extrm.score.PI)
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: count
    ##                   LR Chisq Df Pr(>Chisq)    
    ## gender              20.637  1  5.551e-06 ***
    ## party                0.528  1    0.46736    
    ## ideol               51.427  3  3.967e-11 ***
    ## extrm.score                 0               
    ## gender:party         5.089  1    0.02408 *  
    ## gender:ideol        10.524  4    0.03247 *  
    ## party:extrm.score    0.202  1    0.65289    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Refitting the model with score set 0-2-3-4-6 provides a very similar but
result comparing with score set 1-2-3-4-5.

Refitting the model with score set 2-1-0-1-2 doesn’t provide a good
result. There is no linear association between party and ideology.
Hence, the test suggests one party or gender doesn’t hold more extreme
views than the other.

b).

``` r
#try score set 1-2-3-4-5
mod.homo.lin1.PI <- glm(formula = count ~ gender*party + gender*ideol + party*lin.score1, 
                        family = poisson(link = "log"), data = alldata)

#GI interaction of score set 1-2-3-4-5
Anova(mod.homo.lin1.PI)[6,]
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: count
    ##              LR Chisq Df Pr(>Chisq)  
    ## gender:ideol   9.3358  4    0.05323 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#GI interaction of score set 0-2-3-4-6
Anova(mod.homo.lin2.PI)[6,]
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: count
    ##              LR Chisq Df Pr(>Chisq)  
    ## gender:ideol   9.5303  4    0.04913 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#GI interaction of score set 2-1-0-1-2
Anova(mod.extrm.score.PI)[6,]
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: count
    ##              LR Chisq Df Pr(>Chisq)  
    ## gender:ideol   10.524  4    0.03247 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Reduced number of parameters in each still provides an adequate
explanation for the association.

Based on alpha = 0.05, the association is significant in the second and
third case.

c).

``` r
contr.mat <- rbind(c(rep(0, 7), 1, 0, 0, 0, 0, 0), 
                   c(rep(0, 7), 0,-1, 0, 1, 0, 0),
                   c(rep(0, 7), 0, 0, 0, 1, 0, 0),
                   c(rep(0, 7), 0, 0,-1, 1, 0, 0),
                   c(rep(0, 7), 0, 0, 0, 1,-1, 0),
                   c(rep(0, 7), 0, 1, 0, 0, 0, 0),
                   c(rep(0, 7), 0, 1,-1, 0, 0, 0),
                   c(rep(0, 7), 0, 1, 0, 0,-1, 0),
                   c(rep(0, 7), 0, 0,-1, 0, 0, 0),
                   c(rep(0, 7), 0, 0, 0, 0,-1, 0),
                   c(rep(0, 7), 0, 0, 1, 0,-1, 0),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 1),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 2),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 3),
                   c(rep(0, 7), 0, 0, 0, 0, 0, 4)
                   )

check.na <- is.na(mod.homo.lin1.PI$coefficients)
mod.homo.lin1.PI$coefficients <- mod.homo.lin1.PI$coefficients[!check.na]

library(multcomp)
```

    ## Loading required package: mvtnorm

    ## Loading required package: survival

    ## Loading required package: TH.data

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'TH.data'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     geyser

``` r
wald <- glht(mod.homo.lin1.PI, linfct = contr.mat)
wald.ci <- round(exp(confint(wald, calpha = qnorm(0.975))$confint), 2)
row.names(wald.ci) <- c("GP Rep | M:F", "GI VC:SC | M:F", 
                        "GI VC:M | M:F", "GI VC:SL | M:F", 
                        "GI VC:VL | M:F", "GI SC:M | M:F", 
                        "GI SC:SL | M:F", "GI SC:VL | M:F", 
                        "GI M:SL | M:F", "GI M:VL | M:F", 
                        "GI SL:VL | M:F", "PI REP | 1 Cat Ideol", 
                        "PI REP | 2 Cat Ideol", "PI REP | 3 Cat Ideol", 
                        "PI REP | 4 Cat Ideol")
colnames(wald.ci) <- c("Estimate", "Lower CI", "Upper CI")
wald.ci[2:11,]
```

    ##                Estimate Lower CI Upper CI
    ## GI VC:SC | M:F     0.88     0.55     1.42
    ## GI VC:M | M:F      1.55     1.04     2.30
    ## GI VC:SL | M:F     1.22     0.75     1.98
    ## GI VC:VL | M:F     1.06     0.64     1.76
    ## GI SC:M | M:F      1.75     1.15     2.66
    ## GI SC:SL | M:F     1.38     0.83     2.28
    ## GI SC:VL | M:F     1.20     0.71     2.03
    ## GI M:SL | M:F      0.79     0.52     1.20
    ## GI M:VL | M:F      0.69     0.44     1.07
    ## GI SL:VL | M:F     0.87     0.52     1.46

The GI odds ratio are shown above.

To interpret them, for example, in the first line: between very
conservative and slightly conservative, male are more likely to be very
conservative is 0.88 times as high than female with a 95% Wald CI of
(0.55&lt;OR&lt;1.42). Similarly for the rest.

Comparing the CI for each odds ratio, they give similar interpretation.

d). The reduction of association parameters comes at the cost of
more-structured assumptions regarding the nature of the log-odds ratios.
Thus, when a test for Ho is rejected, it doesn’t imply the association
is entirely linear. There might be additional structure that wasn’t
captured. When a test for Ho is not rejected, it only implies
insufficient evidence of linear-by-linear association, not that there is
no association.
