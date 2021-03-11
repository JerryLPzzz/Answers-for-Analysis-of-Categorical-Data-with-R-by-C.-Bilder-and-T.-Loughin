Chp 3 Q14
================

a).

``` r
pol <- read.table(file = 
"C:\\Users\\Moon\\Desktop\\New folder\\pol_ideol_data.csv", 
                  header = TRUE, sep = ",")

pol$gender <- factor(x = pol$gender, levels = c("F", "M"))
pol$party <- factor(x = pol$party, levels = c("D", "R"))
pol$ideol <- factor(x = pol$ideol, levels = c("VL", "SL", "M", "SC", "VC"))
```

b).

``` r
c.table <- xtabs(formula = count ~ party + ideol + gender, data = pol)
ftable(x = c.table, row.vars = c("gender", "party"), col.vars = "ideol")
```

    ##              ideol  VL  SL   M  SC  VC
    ## gender party                          
    ## F      D            44  47 118  23  32
    ##        R            18  28  86  39  48
    ## M      D            36  34  53  18  23
    ##        R            12  18  62  45  51

ci).

``` r
#proportional odds regression
library(package = MASS)  
mod.fit.ord<-polr(formula = ideol ~ party + gender + party:gender, 
                  data = pol, weights = count, method = "logistic")

summary(mod.fit.ord)
```

    ## 
    ## Re-fitting to get Hessian

    ## Call:
    ## polr(formula = ideol ~ party + gender + party:gender, data = pol, 
    ##     weights = count, method = "logistic")
    ## 
    ## Coefficients:
    ##                  Value Std. Error t value
    ## partyR          0.7562     0.1659  4.5593
    ## genderM        -0.1431     0.1820 -0.7861
    ## partyR:genderM  0.5091     0.2550  1.9965
    ## 
    ## Intercepts:
    ##       Value    Std. Error t value 
    ## VL|SL  -1.5521   0.1332   -11.6560
    ## SL|M   -0.5550   0.1157    -4.7965
    ## M|SC    1.1647   0.1226     9.5009
    ## SC|VC   2.0012   0.1364    14.6666
    ## 
    ## Residual Deviance: 2470.15 
    ## AIC: 2484.15

``` r
#LRTs
library(package = car)  
```

    ## Loading required package: carData

``` r
Anova(mod.fit.ord)
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: ideol
    ##              LR Chisq Df Pr(>Chisq)    
    ## party          56.847  1  4.711e-14 ***
    ## gender          0.843  1    0.35864    
    ## party:gender    3.992  1    0.04571 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Using proportional odds regression, due to large test statistic values for 
#party and party:gender, as well as the corresponding LRT provided by Anova()
#function, there is sufficient evidence that these are important explanatory
#variables.


#multinomial regression
library(nnet)
mod.fit <- multinom(formula = ideol ~ party + gender + party:gender, 
                    data = pol, weights = count)
```

    ## # weights:  25 (16 variable)
    ## initial  value 1343.880657 
    ## iter  10 value 1231.244704
    ## iter  20 value 1229.548447
    ## final  value 1229.543342 
    ## converged

``` r
summary(mod.fit)
```

    ## Call:
    ## multinom(formula = ideol ~ party + gender + party:gender, data = pol, 
    ##     weights = count)
    ## 
    ## Coefficients:
    ##    (Intercept)    partyR     genderM partyR:genderM
    ## SL  0.06598601 0.3758637 -0.12315074      0.0867552
    ## M   0.98652431 0.5774673 -0.59976058      0.6779778
    ## SC -0.64869284 1.4219096 -0.04442702      0.5929326
    ## VC -0.31838463 1.2992041 -0.12968265      0.5957616
    ## 
    ## Std. Errors:
    ##    (Intercept)    partyR   genderM partyR:genderM
    ## SL   0.2097724 0.3677971 0.3181097      0.5756306
    ## M    0.1766421 0.3136662 0.2790125      0.4944619
    ## SC   0.2573076 0.3839323 0.3867020      0.5799046
    ## VC   0.2323285 0.3610630 0.3538841      0.5518725
    ## 
    ## Residual Deviance: 2459.087 
    ## AIC: 2491.087

``` r
#LRTs
Anova(mod.fit)
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: ideol
    ##              LR Chisq Df Pr(>Chisq)    
    ## party          60.555  4  2.218e-12 ***
    ## gender          8.965  4    0.06198 .  
    ## party:gender    3.245  4    0.51763    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Using multinomial regression, the LRT for the interaction gives a p-value 
#of 0.51 indicating there is not sufficient evidence of an interaction. 
#Moreover, party has a very small p-value and gender has a slightly small 
#p-value. Thus, there is sufficient evidence that party has an effect on 
#ideology and marginal evidence that gender has an effect on ideology.
```

cii). For example, female.D means Female & Democrat

``` r
#estimate probabilities for each ideology level based on 
#proportional odds regression
pi.hat.prop <- predict(object = mod.fit.ord, type = "probs")
female.D.pred.prop <- pi.hat.prop[1,]
female.D.pred.prop
```

    ##        VL        SL         M        SC        VC 
    ## 0.1747832 0.1899213 0.3974724 0.1187475 0.1190756

``` r
female.R.pred.prop <- pi.hat.prop[6,]
female.R.pred.prop
```

    ##         VL         SL          M         SC         VC 
    ## 0.09043739 0.12184718 0.38843045 0.17571928 0.22356570

``` r
male.D.pred.prop <- pi.hat.prop[11,]
male.D.pred.prop
```

    ##        VL        SL         M        SC        VC 
    ## 0.1963909 0.2020635 0.3886812 0.1080000 0.1048645

``` r
male.R.pred.prop <- pi.hat.prop[16,]
male.R.pred.prop
```

    ##         VL         SL          M         SC         VC 
    ## 0.06450371 0.09295524 0.35313867 0.19600868 0.29339370

``` r
#estimate probabilities for each ideology level based on 
#multinomial regression
pi.hat.mult <- predict(object = mod.fit, type = "probs")
female.D.pred.mult <- pi.hat.mult[1,]
female.D.pred.mult 
```

    ##         VL         SL          M         SC         VC 
    ## 0.16666222 0.17803054 0.44697087 0.08711911 0.12121726

``` r
female.R.pred.mult  <- pi.hat.mult[6,]
female.R.pred.mult 
```

    ##         VL         SL          M         SC         VC 
    ## 0.08219087 0.12785463 0.39269600 0.17808499 0.21917351

``` r
male.D.pred.mult  <- pi.hat.mult[11,]
male.D.pred.mult 
```

    ##        VL        SL         M        SC        VC 
    ## 0.2195138 0.2073173 0.3231701 0.1097599 0.1402390

``` r
male.R.pred.mult  <- pi.hat.mult[16,]
male.R.pred.mult 
```

    ##         VL         SL          M         SC         VC 
    ## 0.06383112 0.09574563 0.32978787 0.23935868 0.27127670

ciii).

``` r
#three-dimensional contingency table for estimated count
#based on proportional odds regression
pred.prop.count <- c(
sum(pol[1:5, 4])*female.D.pred.prop,
sum(pol[6:10, 4])*female.R.pred.prop,
sum(pol[11:15, 4])*male.D.pred.prop,
sum(pol[16:20, 4])*male.R.pred.prop)

c.table1 <- xtabs(formula = pred.prop.count ~ pol$party + pol$ideol + pol$gender)
ftable(x = c.table1, row.vars = c("pol$gender", "pol$party"), 
       col.vars = "pol$ideol")
```

    ##                      pol$ideol        VL        SL         M        SC        VC
    ## pol$gender pol$party                                                            
    ## F          D                    46.14278  50.13922 104.93272  31.34933  31.43596
    ##            R                    19.80579  26.68453  85.06627  38.48252  48.96089
    ## M          D                    32.20810  33.13842  63.74372  17.71199  17.19777
    ##            R                    12.12670  17.47558  66.39007  36.84963  55.15802

``` r
#three-dimensional contingency table for estimated count
#based on multinomial regression
pred.mult.count <- c(
sum(pol[1:5, 4])*female.D.pred.mult,
sum(pol[6:10, 4])*female.R.pred.mult,
sum(pol[11:15, 4])*male.D.pred.mult,
sum(pol[16:20, 4])*male.R.pred.mult)

c.table2 <- xtabs(formula = pred.mult.count ~ pol$party + pol$ideol + pol$gender)
ftable(x = c.table2, row.vars = c("pol$gender", "pol$party"), 
       col.vars = "pol$ideol")
```

    ##                      pol$ideol        VL        SL         M        SC        VC
    ## pol$gender pol$party                                                            
    ## F          D                    43.99883  47.00006 118.00031  22.99945  32.00136
    ##            R                    17.99980  28.00016  86.00042  39.00061  47.99900
    ## M          D                    36.00026  34.00003  52.99990  18.00062  22.99919
    ##            R                    12.00025  18.00018  62.00012  44.99943  51.00002

civ).

``` r
#observed
c.table <- xtabs(formula = count ~ party + ideol + gender, data = pol)
ftable(x = c.table, row.vars = c("gender", "party"), col.vars = "ideol")
```

    ##              ideol  VL  SL   M  SC  VC
    ## gender party                          
    ## F      D            44  47 118  23  32
    ##        R            18  28  86  39  48
    ## M      D            36  34  53  18  23
    ##        R            12  18  62  45  51

Comparing the observed counts, estimated count based on multinomial
regression are same, estimated count based on proportional odds
regression are different. This suggests that multinomial regression fits
the data well. Proportional odds regression fits the data poorly.

cv).

``` r
#odds ratio for estimated count based on proportional odds regression
pol.table.prop.count <- xtabs(formula = pred.prop.count ~ 
                                pol$gender + pol$party + pol$ideol)
prop.count.odds <- pol.table.prop.count[1,1,]*pol.table.prop.count[2,2,]/ 
  (pol.table.prop.count[2,1,]*pol.table.prop.count[1,2,])
prop.count.odds
```

    ##        VL        SL         M        SC        VC 
    ## 0.8771805 0.9908728 1.2847519 1.6948469 2.0592730

``` r
1/prop.count.odds
```

    ##        VL        SL         M        SC        VC 
    ## 1.1400162 1.0092113 0.7783604 0.5900238 0.4856083

``` r
#The estimated odd of being republican conditioning on very liberal is 14% as
#large as when the target is female than when the target is male.

#The estimated odd of being republican conditioning on slightly liberal is 0.9%
#as large as when the target is female than when the target is male.

#The estimated odd of being democratic conditioning on moderate is 28% as large
#as when the target is female than when the target is male.

#The estimated odd of being democratic conditioning on slightly conservative is
#69% as large as when the target is female than when the target is male.

#The estimated odd of being democratic conditioning on very conservative is 105%
#as large as when the target is female than when the target is male.



#odds ratio for estimated count based on multinomial regression
pol.table.mult.count <- xtabs(formula = pred.mult.count ~ 
                                pol$gender + pol$party + pol$ideol)
mult.count.odds <- pol.table.mult.count[1,1,]*pol.table.mult.count[2,2,]/ 
  (pol.table.mult.count[2,1,]*pol.table.mult.count[1,2,])
mult.count.odds
```

    ##        VL        SL         M        SC        VC 
    ## 0.8148132 0.8886594 1.6050925 1.4742306 1.4784071

``` r
1/mult.count.odds
```

    ##        VL        SL         M        SC        VC 
    ## 1.2272752 1.1252905 0.6230171 0.6783199 0.6764037

``` r
#The estimated odd of being republican conditioning on very liberal is 22% as
#large as when the target is female than when the target is male.

#The estimated odd of being republican conditioning on slightly liberal is 12%
#as large as when the target is female than when the target is male.

#The estimated odd of being democratic conditioning on moderate is 60% as large
#as when the target is female than when the target is male.

#The estimated odd of being democratic conditioning on slightly conservative is
#47% as large as when the target is female than when the target is male.

#The estimated odd of being democratic conditioning on very conservative is 47%
#as large as when the target is female than when the target is male.
```

d). Comparing the results for the two models, multinomial regression is
more appropriate for this setting. The predictions and odds ratios based
on multinomial regression are the same comparing the observed data. This
indicates that multinomial regression fits the data perfectly.
