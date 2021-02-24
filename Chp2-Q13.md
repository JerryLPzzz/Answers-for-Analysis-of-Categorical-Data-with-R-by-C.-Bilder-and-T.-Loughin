Chp2 Q13
================

a).

``` r
c.table<-array(data = c(251, 48, 34, 5), 
               dim = c(2,2), 
               dimnames = list(First = c("made", "missed"),
               Second = c("made", "missed"))) #c.table created

bird1 <- as.data.frame(as.table(c.table))
#"as.table" function transform "c.table" from class array to
# class table. "as.data.frame" transform class table to class
# data frame and list 'First', 'Second', 'Freq' in each column.

trials <- aggregate(formula = Freq ~ First, 
                    data = bird1, FUN = sum)
#"aggregate" function computes summary statistics. Inside
# brackets, it sums Freq based on First regardless of Second. 

success <- bird1[bird1$Second == "made",]
#Inside brackets, provides a list of logical values where
# Second is made retruns TRUE, otherwise returns FALSE,
# regardless of First. Outside brackets, given the logical
# values, it subsets first two rows where Second is made.

bird2 <- data.frame(First = success$First,
                    success = success$Freq, 
                    trials = trials$Freq)
#"data.frame" function creates data frame. In this case, it has
# three columns, their names are First, success, trials. First
# has two levels: made and missed. Success has two values based
# on Freq in success, trials has two values based on Freq in
# trials.
```

b).

``` r
logistic.fit <- glm(formula = success/trials ~ First, 
                    weights = trials,
                    family = binomial,
                    data = bird2)
#Estimate a logistic regression model
summary(logistic.fit)
```

    ## 
    ## Call:
    ## glm(formula = success/trials ~ First, family = binomial, data = bird2, 
    ##     weights = trials)
    ## 
    ## Deviance Residuals: 
    ## [1]  0  0
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   1.9991     0.1827  10.939   <2e-16 ***
    ## Firstmissed   0.2627     0.5042   0.521    0.602    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance:  2.8575e-01  on 1  degrees of freedom
    ## Residual deviance: -1.6431e-14  on 0  degrees of freedom
    ## AIC: 12.624
    ## 
    ## Number of Fisher Scoring iterations: 3

c). The values computed are different comparing the example given in
section 1.2.5. However, they are telling the same thing. The inverted
value in section 1.2.5 equals the value computed here. it is a matter of
interpretation.

``` r
#The estimated odds of a successful second free throw
#attempt are 1.30 times as large as when the first free 
#throw is missed than when the first free throw is made.
exp(logistic.fit$coefficients[2]) #calculate OR
```

    ## Firstmissed 
    ##    1.300398

``` r
exp(confint.default(object = logistic.fit, 
                    parm = "Firstmissed", 
                    level = 0.95)) #Wald CI for OR
```

    ##                 2.5 %   97.5 %
    ## Firstmissed 0.4840513 3.493506

``` r
exp(confint(object = logistic.fit, 
            parm = "Firstmissed", 
            level = 0.95)) #Profile LR intervals for OR
```

    ## Waiting for profiling to be done...

    ##     2.5 %    97.5 % 
    ## 0.5243198 3.9402136

d). The results computed are same comparing the example given in section
1.2.3. The decision is to not reject the null hypothesis, can’t conclude
that there is a significant change in Bird’s second free throw success
percentage over the possible outcomes of the first attempt.

``` r
summary(logistic.fit)$coefficients[2,][4] #p-value based on Wald test
```

    ##  Pr(>|z|) 
    ## 0.6023988

``` r
library(car) #load necessary package for LRTs
```

    ## Loading required package: carData

``` r
Anova(logistic.fit, test = "LR") #p-value based on LRTs
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: success/trials
    ##       LR Chisq Df Pr(>Chisq)
    ## First  0.28575  1      0.593

e). Similarities occurred between the calculation here using logistic
regression and the corresponding calculations in Chapter 1 is because
that the binary table (Bird) given fits the assumption for logistic
regression
