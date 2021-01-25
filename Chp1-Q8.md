Chp1 Q8
================

a). The researchers are trying to form a likelihood function for further
interpretation.

b).

``` r
# Wilson C.I. for box # 5, 6, 7, 9
alpha = 0.05
n = 30
pi.hat = 0
w = 0

p.tilde <- (w + qnorm(p = 1-alpha/2)^2 / 2) / 
  (n + qnorm(p = 1-alpha/2)^2)

round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * 
        sqrt(n) / (n + qnorm(p = 1 - alpha/2)^2) *
        sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n)), 4)
```

    ## [1] 0.0000 0.1135

``` r
# Wilson C.I. for box # 1, 4
alpha = 0.05
n = 30
pi.hat = 1/30
w = 1

p.tilde <- (w + qnorm(p = 1-alpha/2)^2 / 2) / 
  (n + qnorm(p = 1-alpha/2)^2)

round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * 
        sqrt(n) / (n + qnorm(p = 1 - alpha/2)^2) *
        sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n)), 4)
```

    ## [1] 0.0059 0.1667

``` r
# Wilson C.I. for box # 2, 10
alpha = 0.05
n = 30
pi.hat = 2/30
w = 2

p.tilde <- (w + qnorm(p = 1-alpha/2)^2 / 2) / 
  (n + qnorm(p = 1-alpha/2)^2)

round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * 
        sqrt(n) / (n + qnorm(p = 1 - alpha/2)^2) *
        sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n)), 4)
```

    ## [1] 0.0185 0.2132

``` r
# Wilson C.I. for box # 3
alpha = 0.05
n = 30
pi.hat = 4/30
w = 4

p.tilde <- (w + qnorm(p = 1-alpha/2)^2 / 2) / 
  (n + qnorm(p = 1-alpha/2)^2)

round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * 
        sqrt(n) / (n + qnorm(p = 1 - alpha/2)^2) *
        sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n)), 4)
```

    ## [1] 0.0531 0.2968

``` r
# Wilson C.I. for box # 8
alpha = 0.05
n = 30
pi.hat = 12/30
w = 12

p.tilde <- (w + qnorm(p = 1-alpha/2)^2 / 2) / 
  (n + qnorm(p = 1-alpha/2)^2)

round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * 
        sqrt(n) / (n + qnorm(p = 1 - alpha/2)^2) *
        sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n)), 4)
```

    ## [1] 0.2459 0.5768

c). Based on the intervals, it appears that the probability of an egg
hatches is different. For box \# 8, its C.I. does not overlap with box
\# 1, 2, 4, 5, 6, 7, 9, 10.

d). For box \# 1, 2, 3, 4, 5, 6, 7, 9, 10, the calculated intervals are
reasonable. For box \# 8, the calculated interval is (0.2459, 0.5768).
The result seems on the extreme side. I think it is normal, because it
reflects a part of the likelihood function

e). The MLE is simply the observed proportion of successes, which is
22/300. Thus, it is appropriate to consider the data as w = 22 successes
coming from a binomial distribution with n = 300 trials.
