Chp4 Q4
================

a). Rush hours are typically occurred during the morning and evening on
a non-holiday weekday. The data for this problem were collected between
3:25pm and 4:05pm which might be the busiest time of rush hours. Other
factors as mentioned in this problem, such as school isn’t in session
for varies reasons and this may occur during the non-holiday weekday may
reduce the traffic congestion. Different time of the day such as night
time, the traffic is usually good and there will be no congestion.
Different day of the week such as weekend, the traffic is usually good
and there will be no congestion.

b). The observations are not truly independent. The first assumption is
that all counts taken on some process have the same underlying
intensity. The second assumption is that the period of observation is
constant for each count.

c). The probability is only 1.78% during the rush hour. We can assume a
smaller probability during the regular hours. Thus, a warning sign is
sufficient.

``` r
stoplight <- read.csv(file  = 
                        "C:\\Users\\Moon\\Desktop\\New folder\\stoplight.csv")
# Frequencies
rel.freq <- table(stoplight$vehicles)/length(stoplight$vehicles)
rel.freq2 <- c(rel.freq, rep(0, times = 7))

# Poisson calculations
y <- 0:15
prob <- round(dpois(x = y, lambda = mean(stoplight$vehicles)), 4)

# Observed and Poisson
data.frame(y, prob, rel.freq = rel.freq2)
```

    ##     y   prob rel.freq
    ## 1   0 0.0208    0.025
    ## 2   1 0.0804    0.125
    ## 3   2 0.1558    0.175
    ## 4   3 0.2013    0.075
    ## 5   4 0.1950    0.200
    ## 6   5 0.1511    0.175
    ## 7   6 0.0976    0.125
    ## 8   7 0.0540    0.050
    ## 9   8 0.0262    0.050
    ## 10  9 0.0113    0.000
    ## 11 10 0.0044    0.000
    ## 12 11 0.0015    0.000
    ## 13 12 0.0005    0.000
    ## 14 13 0.0001    0.000
    ## 15 14 0.0000    0.000
    ## 16 15 0.0000    0.000

``` r
# Poisson probability that 9 vehicles or more happens for one stoplight cycle
sum(data.frame(y, prob, rel.freq = rel.freq2)[10:16,][2])
```

    ## [1] 0.0178

d). 65.96%

``` r
#probability that at least once over 60 cycles
1 - dbinom(x = 0, size = 60, prob = 0.0178)
```

    ## [1] 0.6595951
