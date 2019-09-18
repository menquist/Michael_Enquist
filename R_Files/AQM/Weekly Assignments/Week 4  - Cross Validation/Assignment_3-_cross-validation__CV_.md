# Assignment 3- cross-validation (CV)
Michael Enquist  
11/30/2016  










```r
#------------------ run LOOCV -----------------------------
# initialise prediction vector
isCorrect <- NULL

# perform cv
for(i in 1:nrow(dat.bc))
{
  x.train <- dat.bc[-i,-1] # remove ID
  x.test <- dat.bc[i,-1] # remove ID
  
  mod <- glm(Class ~ ., x.train, family = "binomial")
  pred <- predict(mod, newdata = x.test, type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- x.test$Class
  isCorrect[i] <- ifelse(y.pred == y.true, 1, 0)
}

# misclassification error
sum(isCorrect)/length(isCorrect)
```

```
## [1] 0.9677892
```

```r
#------------------ run K-fold -----------------------------
error <- NULL
nRuns <- 1000
kSize <- 600

# perform k-fold
for(i in 1:nRuns)
{
  randomSample <- sample(1:nrow(dat.bc), kSize, replace = FALSE)
  x.train <- dat.bc[randomSample, -1] # remove ID
  x.test <- dat.bc[-randomSample, -1] # remove ID
  
  mod <- glm(Class ~ ., x.train, family = "binomial")
  pred <- predict(mod, newdata = x.test, type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- x.test$Class
  isCorrect <- ifelse(y.pred == y.true, 1, 0)
  error[i] <- sum(isCorrect)/length(isCorrect)  
}
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
# plot error over iterations
plot(cumsum(error)/seq_along(error), type = "l")
```

![](Assignment_3-_cross-validation__CV__files/figure-html/unnamed-chunk-2-1.png)<!-- -->



```r
# fit model
mod1 <- glm(Class ~ ., dat.bc[,-1], family = "binomial") # remove ID

#---------------- Bootstrapping ----------------------------
# quantify variance in your model
# set params
error <- NULL
nRuns <- 50
sampleSize <- 50
```



```r
for(i in 1:nRuns)
{
  subsetDat <- dplyr::sample_n(dat.bc, sampleSize)
  pred <- predict(mod1, newdata = subsetDat[,-1], type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- subsetDat$Class
  isCorrect <- ifelse(y.pred == y.true, 1, 0)
  error[i] <- sum(isCorrect)/length(isCorrect)
}

mse <- mean(error^2)
```


```r
df <- dat.bc
head(df)
```

```
##        Id Cl.thickness Cell.size Cell.shape Marg.adhesion Epith.c.size
## 1 1000025            5         1          1             1            2
## 2 1002945            5         4          4             5            7
## 3 1015425            3         1          1             1            2
## 4 1016277            6         8          8             1            3
## 5 1017023            4         1          1             3            2
## 6 1017122            8        10         10             8            7
##   Bare.nuclei Bl.cromatin Normal.nucleoli Mitoses Class
## 1           1           3               1       1     0
## 2          10           3               2       1     0
## 3           2           3               1       1     0
## 4           4           3               7       1     0
## 5           1           3               1       1     0
## 6          10           9               7       1     1
```

```r
#Look up ?factor

df$Class <- factor(df$Class, levels=c(0,1), labels = c("benign","malignant"))




fit.logit <- glm(Class~., data=df, family = binomial())
summary(fit.logit)
```

```
## 
## Call:
## glm(formula = Class ~ ., family = binomial(), data = df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4877  -0.1156  -0.0613   0.0223   2.4668  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.015e+01  1.454e+00  -6.983 2.89e-12 ***
## Id               4.008e-08  7.331e-07   0.055 0.956396    
## Cl.thickness     5.349e-01  1.420e-01   3.767 0.000165 ***
## Cell.size       -6.818e-03  2.091e-01  -0.033 0.973995    
## Cell.shape       3.234e-01  2.307e-01   1.402 0.161010    
## Marg.adhesion    3.306e-01  1.234e-01   2.679 0.007389 ** 
## Epith.c.size     9.624e-02  1.567e-01   0.614 0.539233    
## Bare.nuclei      3.840e-01  9.546e-02   4.022 5.77e-05 ***
## Bl.cromatin      4.477e-01  1.716e-01   2.608 0.009099 ** 
## Normal.nucleoli  2.134e-01  1.131e-01   1.887 0.059224 .  
## Mitoses          5.344e-01  3.294e-01   1.622 0.104740    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 884.35  on 682  degrees of freedom
## Residual deviance: 102.89  on 672  degrees of freedom
## AIC: 124.89
## 
## Number of Fisher Scoring iterations: 8
```

```r
prob <- predict(fit.logit, df, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), labels = c("benign","malignant"))
#By default predict() function predicts the log odds of having a malignant outcome

logit.perf <- table(df$Class, logit.pred, dnn = c("Actual", "Predicted") )
logit.perf
```

```
##            Predicted
## Actual      benign malignant
##   benign       434        10
##   malignant     11       228
```

##Write maximum 300 words on cross-validation (CV), while considering the following (LaTeX preferred):




These methods try to refit a model of interest to samples formed from the training set. This process is to obtain additional information about the fitted model. Leave-one-out cross-validation (LOOCV) is an estimate of the generalisation performance of a model trained on n−1 samples of data. Rather than choosing one model, the thing to do is to fit the model to all of the data, and use LOO-CV to provide a slightly conservative estimate of the performance of that model. K-means is used for clustering as a technique for finding similarity groups in a data, called clusters. It attempts to group together by similarity. Bootstrapping is used to estimate a sample distribution by using the information based on a number of resamples from the population. The number of resamples from the sample to estimate the population distribution. The procedure for Bootstrapping is to treat the sample as population, draw x amount of samples sizes of n with replacement to the sample, and try to estimate the sample distribution of the statistic by the bootstrap sample distribution.

Now which method is "better"? The problem is what "better" means. First, these methods is biased for the estimation of the model error (for an infinite amount of future data). Second, if these methods converge to the true model error (if they are not biased), how certain can you be that the correct model error is close? Lastly, which one is "better?", machine learning and data mining use the k-fold cross validation as a preferecen to CV.


The reason we use cross-validation is solve for overfitting.  For example, when using the translink data, we need toidentified our best combination of parameters (in our case time and route) to test the performance. We might want to test Tue and Thu on the 49 busline to ensure that our choices work for those days as well. Also, we have a limited set of data to estimate the unknown parameters. If we overfit then our parameter estimates will work very well for the existing data but not as well for when we use them in another context. Thus, cross-validation helps in avoiding the above issue of overfitting by proving us some reassurance that the parameter estimates are not unique to the data we used to estimate them.





