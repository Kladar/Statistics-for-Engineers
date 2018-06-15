



###########################################################################
## Ryan Kladar (S141094)
## 20 OCT 2014
## Adapted R File for Project 1

## Contains given R code from original document
## with some unnecessary or unused portions removed,
## plus the R code to complete sections on 
## confidence intervals, hypothesis testing, and
## summary statistics

################################
## Read the data

## Import Excel file, should appear as a table in R
## Set data to D variable
D <- dataSkivefjord

################################
## Overview of the data

## The dimensions (number of row and columns)
dim(D)
## The names of the columns
names(D)
## First rows
head(D)
## Last rows
tail(D)
## Summary statistics
summary(D)
## Another summary
str(D)

################################
## Emperical denstity

## Histogram of the Nitrate load
hist(D$NLoad)

## Default Density plot of Emprical Density
plot(density(x = D$NLoad))

################################
## Subset for each VMP

## Subset for VMP 0 (before the first VMP)
VMP0 <- subset(D, D$vmp == 0)
## A subset for VMP 1
VMP1 <- subset(D, D$vmp == 1)
## A subset for VMP 2
VMP2 <- subset(D, D$vmp == 2)
## A subset for VMP 3
VMP3 <- subset(D, D$vmp == 3)

################################
## Plot time series

## Plot the Nitrate load over time
plot(D$year, D$NLoad, type="b")

## Plot the Nitrate load over time and color each VMP

## An empty plot
plot(D$year, D$NLoad, type="n")
## Add a line for each VMP
lines(VMP0$year, VMP0$NLoad, type="b", col=1)
lines(VMP1$year, VMP1$NLoad, type="b", col=2)
lines(VMP2$year, VMP2$NLoad, type="b", col=3)
lines(VMP3$year, VMP3$NLoad, type="b", col=4)
## Add a legend
legend("topright", paste0("VMP",0:3), lty=1, col=1:4)

################################
## Histogram for each VMP

## Make 2 by 2 plots
par(mfrow=c(2,2))
## Plot a histogram of each VMP
hist(VMP0$NLoad, main="VMP = 0", xlab="Nitrate load", col=5 )
hist(VMP1$NLoad, main="VMP = 1", xlab="Nitrate load", col=4 )
hist(VMP2$NLoad, main="VMP = 2", xlab="Nitrate load", col=3 )
hist(VMP3$NLoad, main="VMP = 3", xlab="Nitrate load", col=2 )

################################
## Box-plots

## Reset device to one plot again, for more on plots 
## see ?plot, ?par, and maybe ?plotmath
par(mfrow=c(1,1))
## Box-plot of each VMP
boxplot(NLoad~vmp, data=D, xlab="VMP", ylab="Nitrate load", col = 4)


################################
## Table of sample statistics

## Use the VMP subsets for each period
mean(VMP0$NLoad)
var(VMP0$NLoad)
summary(VMP0$NLoad)
length(VMP0$NLoad)
sd(VMP0$NLoad)
## Repeat for each subset to fill in table

################################
## 95% Confidence Interval Calculations
##
t.test(VMP0$NLoad)
t.test(VMP1$NLoad)
t.test(VMP2$NLoad)
t.test(VMP3$NLoad)
## The confidence interval is given in the output
#################################
## Hypothesis Testing (non directional)

## overall hypothesis test
t.test(VMP3$NLoad, mu = mean(VMP0$NLoad))

## Individual VMP hypothesis test
t.test(VMP3$NLoad, mu = mean(VMP2$NLoad))
t.test(VMP2$NLoad, mu = mean(VMP1$NLoad))
t.test(VMP1$NLoad, mu = mean(VMP0$NLoad))

## Compare to your own alpha significance threshold on paper or input
## as an argument to the t.test function. check help for details
#######################################################################