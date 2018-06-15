





################################
## a)
## Read the data
D <- read.table("dataSkivefjord.csv", sep=",", header=TRUE)

## Use for the description of the data
summary(D)
str(D)



################################
## b)
## Make a logarithmic transformation of the chlorophyll concentration
D$logChlorophyll <- log(D$chlorophyll)
## Make (yourself) a scatter plot of logChlorophyll vs. totalP



################################
## e)
## A simple linear regression
fit <- lm(logChlorophyll ~ totalP, data=D)
## A summary of the result
summary(fit)
var(D$totalP)


################################
## f)
## Make a QQ-plot
qqnorm(fit$residuals)
qqline(fit$residuals)

## Plot the residuals vs. the fitted values, i.e. the 
## expected (predicted by the model) values of logChlorophyll
plot(fit$fitted.values, fit$residuals)



################################
## g)
## t quantile for calculation of confidence bands
qt(0.975, fit$df.residual)  
## Function for calculation of parameter confidence bands
confint(fit, level=0.95)



################################
## h)
## The critical value of the test statistic
qt(0.975, fit$df.residual)



################################
## i)
## Prediction of chlorophyll concentration in December 2006
exp(predict(fit, newdata=data.frame(totalP=0.0703)))
## 95% prediction interval
PI <- predict(fit, newdata=data.frame(totalP=0.0703), 
              interval="prediction", level=0.95)
## Lower bound
exp(PI[,"lwr"])
## Upper bound
exp(PI[,"upr"])


