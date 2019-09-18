# Gapminder_Assignment
Michael Enquist  
11/16/2016  


```r
#library(devtools)
#devtools::install_github("jennybc/gapminder")
```


```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```











![](Gapminder_Assignment_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


![](Gapminder_Assignment_files/figure-html/unnamed-chunk-6-1.png)<!-- -->![](Gapminder_Assignment_files/figure-html/unnamed-chunk-6-2.png)<!-- -->![](Gapminder_Assignment_files/figure-html/unnamed-chunk-6-3.png)<!-- -->


![](Gapminder_Assignment_files/figure-html/unnamed-chunk-7-1.png)<!-- -->![](Gapminder_Assignment_files/figure-html/unnamed-chunk-7-2.png)<!-- -->![](Gapminder_Assignment_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-8-1.png)<!-- -->




##Now doing a regression on the year 1992 based on gdppercap. After, we used a cross validation called hold-out. We split the set into a training and one test. The relative sizes of the training and test set is arbitrary. The model is built on the training set, the prediction errors are calculated using the validation set, and the test set is used to assess the generalization error of the final model.




```r
# Looking into lifeExp and gdpPercap

# Now using a cross validation between lifeExp and gdpPercap
gdp.median <- gdp.median[-1]
colnames(gdp.median) <- c("year.1957", "year.1962", "year.1967", "year.1972","year.1977","year.1982","year.1987","year.1992","year.1997","year.2002", "year.2007")


gdp.year <- lm(year.1992~., data=gdp.median)
summary(gdp.year)
```

```
## 
## Call:
## lm(formula = year.1992 ~ ., data = gdp.median)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1971.6  -186.7    44.8   193.1  2131.8 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -19.51471   79.82038  -0.244 0.807239    
## year.1957     0.04600    0.13775   0.334 0.738966    
## year.1962    -0.14304    0.19038  -0.751 0.453795    
## year.1967    -0.13147    0.09277  -1.417 0.158779    
## year.1972     0.22731    0.06996   3.249 0.001471 ** 
## year.1977    -0.15869    0.07076  -2.243 0.026602 *  
## year.1982     0.34349    0.08670   3.962 0.000122 ***
## year.1987     0.04736    0.05978   0.792 0.429640    
## year.1997     0.84655    0.06183  13.691  < 2e-16 ***
## year.2002     0.14236    0.07773   1.831 0.069302 .  
## year.2007    -0.26242    0.04387  -5.982 1.98e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 648.4 on 131 degrees of freedom
## Multiple R-squared:  0.9952,	Adjusted R-squared:  0.9948 
## F-statistic:  2723 on 10 and 131 DF,  p-value: < 2.2e-16
```

```r
train <- gdp.median[1:50,]
test <- gdp.median[51:142,]

Trainx <- train[sample.int(dim(train)[1], size=50), ]
Testx <- test[sample.int(dim(test)[1], size=92), ]
dim(Trainx)      
```

```
## [1] 50 11
```

```r
dim(Testx)
```

```
## [1] 92 11
```

```r
x.train <- sample_n(train,50)
x.test <- sample_n(test, 92)
dim(x.train)
```

```
## [1] 50 11
```

```r
dim(x.test)
```

```
## [1] 92 11
```

```r
#Running a regression for train
A <- lm( year.1992~., data=Trainx)
summary(A)
```

```
## 
## Call:
## lm(formula = year.1992 ~ ., data = Trainx)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1654.36  -114.09    40.32   208.56   861.38 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -49.27919  118.59873  -0.416 0.680044    
## year.1957     0.17512    0.20646   0.848 0.401486    
## year.1962    -0.14795    0.37558  -0.394 0.695777    
## year.1967    -0.25000    0.40614  -0.616 0.541756    
## year.1972     0.41535    0.24340   1.706 0.095876 .  
## year.1977    -0.20436    0.10235  -1.997 0.052886 .  
## year.1982     0.43887    0.23642   1.856 0.070975 .  
## year.1987    -0.20353    0.17602  -1.156 0.254592    
## year.1997     0.73839    0.12154   6.076 4.07e-07 ***
## year.2002     0.37584    0.16569   2.268 0.028924 *  
## year.2007    -0.31857    0.08615  -3.698 0.000668 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 532.3 on 39 degrees of freedom
## Multiple R-squared:  0.9971,	Adjusted R-squared:  0.9963 
## F-statistic:  1333 on 10 and 39 DF,  p-value: < 2.2e-16
```

```r
#Running the regression for test
B <- lm(year.1992 ~ ., data=Testx)
summary(B)
```

```
## 
## Call:
## lm(formula = year.1992 ~ ., data = Testx)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1885.59  -163.81    55.38   196.74  2127.90 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -38.63133  105.81035  -0.365   0.7160    
## year.1957     0.01145    0.20547   0.056   0.9557    
## year.1962    -0.07302    0.28386  -0.257   0.7977    
## year.1967    -0.16927    0.11511  -1.471   0.1453    
## year.1972     0.19848    0.09156   2.168   0.0331 *  
## year.1977    -0.08667    0.12391  -0.699   0.4863    
## year.1982     0.27610    0.12770   2.162   0.0336 *  
## year.1987     0.07216    0.06961   1.037   0.3030    
## year.1997     0.92256    0.08906  10.359  < 2e-16 ***
## year.2002     0.06495    0.10663   0.609   0.5441    
## year.2007    -0.25855    0.05694  -4.541 1.93e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 695.1 on 81 degrees of freedom
## Multiple R-squared:  0.9949,	Adjusted R-squared:  0.9943 
## F-statistic:  1585 on 10 and 81 DF,  p-value: < 2.2e-16
```




```r
plot(fitted(A), residuals(A), xlab="Fitted", ylab= "residuals")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
plot(fitted(A), residuals(A), xlab= "Fitted", main="|residuals|")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )     
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
     summary(lm((residuals(A) ~ fitted(A))))
```

```
## 
## Call:
## lm(formula = (residuals(A) ~ fitted(A)))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1654.36  -114.09    40.32   208.56   861.38 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -9.784e-14  9.081e+01       0        1
## fitted(A)    1.110e-17  7.807e-03       0        1
## 
## Residual standard error: 479.8 on 48 degrees of freedom
## Multiple R-squared:  6.471e-32,	Adjusted R-squared:  -0.02083 
## F-statistic: 3.106e-30 on 1 and 48 DF,  p-value: 1
```

```r
             qqnorm(residuals(A), ylab="residuals")
             qqline(residuals(A), col= "red", lwd = 2)  
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
             hist(residuals(A))
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

```r
             Z <- lm(year.1992 ~ ., data=Trainx)
             summary(Z)
```

```
## 
## Call:
## lm(formula = year.1992 ~ ., data = Trainx)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1654.36  -114.09    40.32   208.56   861.38 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -49.27919  118.59873  -0.416 0.680044    
## year.1957     0.17512    0.20646   0.848 0.401486    
## year.1962    -0.14795    0.37558  -0.394 0.695777    
## year.1967    -0.25000    0.40614  -0.616 0.541756    
## year.1972     0.41535    0.24340   1.706 0.095876 .  
## year.1977    -0.20436    0.10235  -1.997 0.052886 .  
## year.1982     0.43887    0.23642   1.856 0.070975 .  
## year.1987    -0.20353    0.17602  -1.156 0.254592    
## year.1997     0.73839    0.12154   6.076 4.07e-07 ***
## year.2002     0.37584    0.16569   2.268 0.028924 *  
## year.2007    -0.31857    0.08615  -3.698 0.000668 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 532.3 on 39 degrees of freedom
## Multiple R-squared:  0.9971,	Adjusted R-squared:  0.9963 
## F-statistic:  1333 on 10 and 39 DF,  p-value: < 2.2e-16
```

```r
             plot(residuals(Z))
              abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-5.png)<!-- -->

```r
             plot(fitted(Z), residuals(Z), xlab="Fitted", ylab= "residuals")
             abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-6.png)<!-- -->

```r
             plot(fitted(Z), residuals(Z), xlab= "Fitted", main="|residuals|")
             abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-7.png)<!-- -->

```r
                  summary(lm((residuals(Z) ~ fitted(Z))))
```

```
## 
## Call:
## lm(formula = (residuals(Z) ~ fitted(Z)))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1654.36  -114.09    40.32   208.56   861.38 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept) -9.784e-14  9.081e+01       0        1
## fitted(Z)    1.110e-17  7.807e-03       0        1
## 
## Residual standard error: 479.8 on 48 degrees of freedom
## Multiple R-squared:  6.471e-32,	Adjusted R-squared:  -0.02083 
## F-statistic: 3.106e-30 on 1 and 48 DF,  p-value: 1
```

```r
                          qqnorm(residuals(Z), ylab="residuals")
                          qqline(residuals(Z), col= "red", lwd = 2)  
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-8.png)<!-- -->

```r
                          hist(residuals(Z))
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-9.png)<!-- -->

```r
                          pred <- predict(Z, newdata = x.test)
                          obs <- x.test$year.1992
                          plot(obs ~ pred)
                          abline(c(0,1), col="red", lwd = 2)
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-10.png)<!-- -->

```r
                          RVSE <- sqrt(mean(pred - obs)^2)
                          AE <- sum(abs(pred - obs))
                          acf(pred)
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-11.png)<!-- -->

```r
#Autocorrelation function (ACF). At lag k, this is the correlation between series values that are k intervals apart.
                          

                          pacf(pred)
```

![](Gapminder_Assignment_files/figure-html/unnamed-chunk-11-12.png)<!-- -->

```r
#Partial autocorrelation function (PACF). At lag k, this is the correlation between    series values that are k intervals apart, accounting for the values of the intervals between.
                          
#A positive correlation indicates that large current values correspond with large values at the specified lag; a negative correlation indicates that large current values correspond with small values at the specified lag.
                          
#The absolute value of a correlation is a measure of the strength of the association, with larger absolute values indicating stronger relationships.

                          summary(pred)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   282.9  1378.0  4901.0  8705.0 11320.0 54710.0
```
