Chp5 Q7
================

``` r
placekick <- read.table(file = "C:\\Users\\Moon\\Desktop\\New folder\\Placekick.csv", 
                        header = TRUE, sep = ",")


# Must first fit the smallest and largest models to be considered
empty.mod <- glm(formula = good ~ 1, family = binomial(link = logit), data = placekick)
full.mod <- glm(formula = good ~ ., family = binomial(link = logit), data = placekick)


# The step() function uses information criteria for variable selection, 
#  k = 2 (default) gives AIC, k = log(nrow(...)) gives BIC when "..." is
#  replaced with the data set name

step.sel <- step(object = empty.mod, scope = list(upper = full.mod), 
                 k = log(nrow(placekick)), trace = TRUE)
```

    ## Start:  AIC=1020.69
    ## good ~ 1
    ## 
    ##            Df Deviance     AIC
    ## + distance  1   775.75  790.27
    ## + PAT       1   834.41  848.93
    ## + change    1   989.15 1003.67
    ## <none>         1013.43 1020.69
    ## + elap30    1  1007.71 1022.23
    ## + wind      1  1010.59 1025.11
    ## + week      1  1011.24 1025.76
    ## + type      1  1011.39 1025.92
    ## + field     1  1012.98 1027.50
    ## 
    ## Step:  AIC=790.27
    ## good ~ distance
    ## 
    ##            Df Deviance     AIC
    ## + PAT       1   762.41  784.20
    ## <none>          775.75  790.27
    ## + change    1   770.50  792.29
    ## + wind      1   772.53  794.32
    ## + week      1   773.86  795.64
    ## + type      1   775.67  797.45
    ## + elap30    1   775.68  797.47
    ## + field     1   775.74  797.53
    ## - distance  1  1013.43 1020.69
    ## 
    ## Step:  AIC=784.2
    ## good ~ distance + PAT
    ## 
    ##            Df Deviance    AIC
    ## <none>          762.41 784.20
    ## + change    1   759.33 788.38
    ## + wind      1   759.66 788.71
    ## + week      1   760.57 789.62
    ## - PAT       1   775.75 790.27
    ## + type      1   762.25 791.30
    ## + elap30    1   762.31 791.36
    ## + field     1   762.41 791.46
    ## - distance  1   834.41 848.93

``` r
BIC(step.sel) 
```

    ## [1] 784.1956

``` r
#the final model includes distance and PAT as its 
#explanatory variables. It has a BIC of 784.2

#for each possible change to the model such as add wind as its
#explanatory variables, the BIC would increases to 788.71. 
#The table is provided here as a reference.
```
