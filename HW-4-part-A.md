Homework Assignment 4 part A
================

``` r
pol <- read.table(file = 
"C:\\Users\\Moon\\Desktop\\New folder\\pol_ideol_data.csv", 
                  header = TRUE, sep = ",")

pol$gender <- factor(x = pol$gender, levels = c("F", "M"))
pol$party <- factor(x = pol$party, levels = c("D", "R"))
pol$ideol <- factor(x = pol$ideol, levels = c("VL", "SL", "M", "SC", "VC"))
```

a). The estimated odd of being democratic is 38.18% as large as when the
target is female than when the target is male.

``` r
pol.table.a <- xtabs(formula = count ~ gender + party, data = pol)
Q1 <- pol.table.a[1,1]*pol.table.a[2,2] / (pol.table.a[2,1]*pol.table.a[1,2])
Q1
```

    ## [1] 1.381891

b).

``` r
pol.table.b <- xtabs(formula = count ~ gender + party + ideol, data = pol)

Q2 <- pol.table.b[1,1,]*pol.table.b[2,2,]/ 
  (pol.table.b[2,1,]*pol.table.b[1,2,])
Q2
```

    ##        VL        SL         M        SC        VC 
    ## 0.8148148 0.8886555 1.6050900 1.4743590 1.4782609

``` r
1/Q2
```

    ##        VL        SL         M        SC        VC 
    ## 1.2272727 1.1252955 0.6230180 0.6782609 0.6764706

The estimated odd of being republican conditioning on very liberal is
22.72% as large as when the target is female than when the target is
male.

The estimated odd of being republican conditioning on slightly liberal
is 12.52% as large as when the target is female than when the target is
male.

The estimated odd of being democratic conditioning on moderate is 60.5%
as large as when the target is female than when the target is male.

The estimated odd of being democratic conditioning on slightly
conservative is 47.43% as large as when the target is female than when
the target is male.

The estimated odd of being democratic conditioning on very conservative
is 47.82% as large as when the target is female than when the target is
male.

c).

``` r
mantelhaen.test(pol.table.b)
```

    ## 
    ##  Mantel-Haenszel chi-squared test with continuity correction
    ## 
    ## data:  pol.table.b
    ## Mantel-Haenszel X-squared = 3.2436, df = 1, p-value = 0.0717
    ## alternative hypothesis: true common odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.987835 1.754986
    ## sample estimates:
    ## common odds ratio 
    ##          1.316676

Give the p-value is a little larger than 0.05 and CI based on 95% is
(0.98, 1.75). Strictly speaking, based on a 95% CI, we cannot conclude
that the gender and party are not conditionally independent. However,
based on 95% CI, we may say that there is a marginal evidence that
gender and party are not conditionally independent.

d).

``` r
#install.packages("DescTools")
library(DescTools)
BreslowDayTest(pol.table.b)
```

    ## 
    ##  Breslow-Day test on Homogeneity of Odds Ratios
    ## 
    ## data:  pol.table.b
    ## X-squared = 3.2354, df = 4, p-value = 0.5192

There is not strong evidence against Ho that all five ideologies have
homogeneous conditional association. It is appropriate to pull data from
all five ideologies together to increase study sample size to draw
conclusion about gender and party.
