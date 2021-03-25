Chp4 Q30
================

``` r
#Create data frame
BirdData1 <- function(){
  Guild <- c("Air-Arth","Leaf-Arth","Wood-Arth","Ground-Arth",
             "Fruit","Seeds","Nectar")
  Degradation.high <- c(rep("High",7))
  Degradation.moderate <- c(rep("Moderate",7))
  Degradation.low <- c(rep("Low",7))
  Guild <- c(rep(Guild,3))
  
  High.count <- c(8,69,2,25,20,7,100)
  Moderate.count <- c(39,139,39,57,48,20,177)
  Low.count <- c(48,131,50,115,16,19,190)
  
  BirdData <- data.frame(Guild = Guild,
                         Degradation = c(Degradation.high,
                                         Degradation.moderate,
                                         Degradation.low),
                         Count = c(High.count,Moderate.count,Low.count))
  return(BirdData)
}
BirdData <- BirdData1()

#there is no level for "Degradation", we need level for it.
BirdData$Degradation <- factor(BirdData$Degradation, levels = c("High","Moderate","Low"))

#Create contingency table
GPI3 <- xtabs(formula= Count ~ Guild + Degradation, data=BirdData)

#fit with poison regression
poi.fit <- glm(Count ~ Guild*Degradation,
               family = poisson(link = "log"), data = BirdData)

#check out odds ratio based on estimated coefficient at interaction terms.
summary(poi.fit)
```

    ## 
    ## Call:
    ## glm(formula = Count ~ Guild * Degradation, family = poisson(link = "log"), 
    ##     data = BirdData)
    ## 
    ## Deviance Residuals: 
    ##  [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                            2.0794     0.3536   5.882 4.06e-09 ***
    ## GuildFruit                             0.9163     0.4183   2.190  0.02850 *  
    ## GuildGround-Arth                       1.1394     0.4062   2.805  0.00503 ** 
    ## GuildLeaf-Arth                         2.1547     0.3735   5.769 7.97e-09 ***
    ## GuildNectar                            2.5257     0.3674   6.874 6.24e-12 ***
    ## GuildSeeds                            -0.1335     0.5175  -0.258  0.79640    
    ## GuildWood-Arth                        -1.3863     0.7906  -1.754  0.07951 .  
    ## DegradationModerate                    1.5841     0.3881   4.081 4.48e-05 ***
    ## DegradationLow                         1.7918     0.3819   4.692 2.71e-06 ***
    ## GuildFruit:DegradationModerate        -0.7087     0.4706  -1.506  0.13212    
    ## GuildGround-Arth:DegradationModerate  -0.7599     0.4563  -1.666  0.09580 .  
    ## GuildLeaf-Arth:DegradationModerate    -0.8838     0.4151  -2.129  0.03326 *  
    ## GuildNectar:DegradationModerate       -1.0131     0.4078  -2.484  0.01297 *  
    ## GuildSeeds:DegradationModerate        -0.5343     0.5861  -0.912  0.36196    
    ## GuildWood-Arth:DegradationModerate     1.3863     0.8224   1.686  0.09185 .  
    ## GuildFruit:DegradationLow             -2.0149     0.5083  -3.964 7.36e-05 ***
    ## GuildGround-Arth:DegradationLow       -0.2657     0.4411  -0.602  0.54689    
    ## GuildLeaf-Arth:DegradationLow         -1.1507     0.4098  -2.808  0.00499 ** 
    ## GuildNectar:DegradationLow            -1.1499     0.4014  -2.865  0.00417 ** 
    ## GuildSeeds:DegradationLow             -0.7932     0.5842  -1.358  0.17455    
    ## GuildWood-Arth:DegradationLow          1.4271     0.8160   1.749  0.08030 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance:  9.8178e+02  on 20  degrees of freedom
    ## Residual deviance: -9.5479e-15  on  0  degrees of freedom
    ## AIC: 157.28
    ## 
    ## Number of Fisher Scoring iterations: 3

For example, at “GuildFruit:DegradationModerate”. Since we are using
High level degradation as a base line, we would interpret this line as:
the estimate for OR is exp(-0.7087), the ratio of fruit guild is
exp(-0.7087)-1 higher in Moderate level degradation then in High level
degradation.

Keep this concept in mind, we can find that when we interpret -2.0149
and -0.2657, those two values represent largest and smallest odds
ratios. Hence, we can conclude that fruit guild is more affected by
habitat degradation, ground guild is less affected by habitat
degradation.
