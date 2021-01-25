Chp1 Q7
================

A). 
Since n &lt; 40, we use Wilson CI as recommended by Brown et al.

``` r
# Wilson C.I. for box at 10 Celsius
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
# Wilson C.I. for box at 15 Celsius
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
# Wilson C.I. for box at 20 Celsius
alpha = 0.05
n = 30
pi.hat = 25/30
w = 25

p.tilde <- (w + qnorm(p = 1-alpha/2)^2 / 2) / 
  (n + qnorm(p = 1-alpha/2)^2)

round(p.tilde + qnorm(p = c(alpha/2, 1-alpha/2)) * 
        sqrt(n) / (n + qnorm(p = 1 - alpha/2)^2) *
        sqrt(pi.hat*(1-pi.hat) + qnorm(p = 1-alpha/2)^2/(4*n)), 4)
```

    ## [1] 0.6644 0.9266

B). 
By assessing the three Wilson C.I. calculated above, the
probabilities could not be the same at each temperature.
