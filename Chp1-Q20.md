Chp1 Q20
================

a).

-   Sample for “intent to treat”:

16402 healthy men and women between the ages of 18 and 30 years in
Rayong and Chon Buri provinces in Thailand.

-   Sample for “per-protocol”:

12542 healthy men and women (weren’t HIV-positive on PCR before the
trial started and received treatment according to the protocol) between
the ages of 18 and 30 years in Rayong and Chon Buri provinces in
Thailand.

-   The population used in those studies:

community-based population with largely heterosexual risk.

-   The population ultimately that the researchers would like to extend:

To use the results to advise the entire world.

b1). for intent-to-treat data

``` r
#compute table value
c.table <- array(data = c(56, 76, 8146, 8124), dim = c(2,2), 
                 dimnames = list(Trt = c("vaccine", "placebo"), 
                                 Response = c("HIV", "No HIV")))

#We can use Odds Ratio to solve this problem
#Odds Ratio
OR.hat <- c.table[1,1]*c.table[2,2] / (c.table[2,1]*c.table[1,2])
round(OR.hat, 4) #OR.hat
```

    ## [1] 0.7349

``` r
round(1/OR.hat, 4) #inverse of OR.hat
```

    ## [1] 1.3608

``` r
alpha <- 0.05 #set 95% CI
var.log.or <- 1/c.table[1,1] + 1/c.table[1,2] + 
              1/c.table[2,2] + 1/c.table[2,1] #Variance of log(OR.hat)
OR.CI <- exp(log(OR.hat) + qnorm(p = c(alpha/2, 1-alpha/2)) *
               sqrt(var.log.or)) #Compute Wald CI for OR
round(OR.CI, 4) #95% CI for OR
```

    ## [1] 0.5196 1.0392

``` r
rev(round(1/OR.CI, 4)) #inverse of 95% CI for OR
```

    ## [1] 0.9623 1.9244

The estimated odd of contracting HIV is 73.49% as large as when the
vaccine is given than when a placebo is given.

The 95% CI is (0.5196 &lt; OR &lt; 1.0392). Because 1 is within the
interval, there is insufficient evidence to indicate the vaccine does
decrease the true odds of contracting HIV

Alternatively, we could interpret the inverted odds ratio in terms of
protection due to vaccination: The estimated odds of being HIV free are
1.3608 times as large as when the vaccine is given than when the placebo
is given. The 95% confidence interval for the inverted odds ratio is
(0.9623, 1.9244).

b2). for per-protocol data

``` r
#compute table value
c.table <- array(data = c(36, 50, 6140, 6316), dim = c(2,2), 
                 dimnames = list(Trt = c("vaccine", "placebo"), 
                                 Response = c("HIV", "No HIV")))

#We can use Odds Ratio to solve this problem
#Odds Ratio
OR.hat <- c.table[1,1]*c.table[2,2] / (c.table[2,1]*c.table[1,2])
round(OR.hat, 4) #OR.hat
```

    ## [1] 0.7406

``` r
round(1/OR.hat, 4) #inverse of OR.hat
```

    ## [1] 1.3502

``` r
alpha <- 0.05 #set 95% CI
var.log.or <- 1/c.table[1,1] + 1/c.table[1,2] + 
              1/c.table[2,2] + 1/c.table[2,1] #Variance of log(OR.hat)
OR.CI <- exp(log(OR.hat) + qnorm(p = c(alpha/2, 1-alpha/2)) *
               sqrt(var.log.or)) #Compute Wald CI for OR
round(OR.CI, 4) #95% CI for OR
```

    ## [1] 0.4819 1.1384

``` r
rev(round(1/OR.CI, 4)) #inverse of 95% CI for OR
```

    ## [1] 0.8784 2.0753

The estimated odd of contracting HIV is 74.06% as large as when the
vaccine is given than when a placebo is given.

The 95% CI is (0.4819 &lt; OR &lt; 1.1384). Because 1 is within the
interval, there is insufficient evidence to indicate the vaccine does
decrease the true odds of contracting HIV

Alternatively, we could interpret the inverted odds ratio in terms of
protection due to vaccination: The estimated odds of being HIV free are
1.3502 times as large as when the vaccine is given than when the placebo
is given. The 95% confidence interval for the inverted odds ratio is
(0.8784, 2.0753).
