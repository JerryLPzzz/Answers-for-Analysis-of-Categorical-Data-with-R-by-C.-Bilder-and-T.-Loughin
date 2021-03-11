Chp 3 Q15
================

``` r
library(package = MASS)
pol <- read.table(file = 
"C:\\Users\\Moon\\Desktop\\New folder\\pol_ideol_data.csv", 
                  header = TRUE, sep = ",")

pol$gender <- factor(x = pol$gender, levels = c("F", "M"))
pol$party <- factor(x = pol$party, levels = c("D", "R"))
pol$ideol <- factor(x = pol$ideol, levels = c("VL", "SL", "M", "SC", "VC"))

mod.fit.ord<-polr(formula = ideol ~ party + gender + party:gender, 
                  data = pol, weights = count, method = "logistic")
pi.hat.prop <- predict(object = mod.fit.ord, type = "probs")
female.D.pred.prop <- pi.hat.prop[1,]
female.R.pred.prop <- pi.hat.prop[6,]
male.D.pred.prop <- pi.hat.prop[11,]
male.R.pred.prop <- pi.hat.prop[16,]

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
