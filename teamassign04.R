
###########################
#                         #
#   Team Assignment 4     #
#                         #
###########################

## Please submit one set of answers per team.                  ##
## Your answers may be submitted as an annotated R file.       ##
## Please submit your plots in a PDF as a separate attachment. ##
#################################################################

library(tidyverse)
library(qpcR)
library(MASS)
library(readxl)
library(plot3D)

#################
## Question 1: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications. If it is 
# not possible, explain why it is not possible. For each part, include a plot that shows the 
# interesting points.

#   (a) The data set has a point that is clearly visible for all four types of residuals 
#       discussed -- standardized, studentized, PRESS, R-student.

#Create data set for comparing the grades of people at different ages and IQ levels
IQ <- c(94,93:120,119)
age <- c(22,22:49,51)
grade <- c(67,69:95,98,98)
grades.df <- data.frame(IQ, age, grade)

## Run the linear regression
grades.lm <- lm(grade~.,data=grades.df)
summary(grades.lm)

#Find the residuals
ei<-resid(grades.lm)

#Find standardized residuals
di <- stdres(grades.lm)

# Find the studentized residuals (the quotient resulting from the division of a 
#residual by an estimate of its standard deviation)
ri <- studres(grades.lm) #studentized -- e_i/((MSres*(1-h_ii))^1/2)

#Find the R-student residuals
ti <- rstudent(grades.lm) #R-student -- e_i/((((S_i)^2)*(1-h_ii))^1/2)

#Find the PRESS residuals
grades.lm.age <- lm(grade~age,data=grades.df)
pi.age <- PRESS(grades.lm.age)

#Residual plot vs. fitted values
fit.grades <- fitted(grades.lm)

#NOTE: Point in top right corner (or bottom left) is the one that is present in all plots
plot(fit.grades,ti) #R-STUDENT
plot(fit.grades,ri) #STUDENTIZED
plot(fit.grades,di) #STANDARDIZED
plot(fit.grades,pi.age$residuals) #PRESS (for age variable only)

#   (b) The data set has a point that stands out when viewing studentized residuals but not 
#       when viewing standardized residuals.

#Create data set for comparing the grades of people at different ages and IQ levels
IQb <- c(92:120,60)
ageb <- c(23,25,23:49, 80)
gradeb <- c(68:96, 50)
grades.df.b <- data.frame(IQb, ageb, gradeb)

## Run the linear regression
grades.lm.b <- lm(gradeb~.,data=grades.df.b)
summary(grades.lm.b)

#Find standardized residuals
di <- stdres(grades.lm.b)

# Find the studentized residuals
ri<- studres(grades.lm.b)

#Residual plot vs. fitted values
fit.grades.b <- fitted(grades.lm.b)
plot(fit.grades.b,di, ylim=c(-20000000, 40000000)) #STANDARDIZED - NO OUTLIER PRESENT
plot(fit.grades.b,ri, ylim=c(-20000000, 40000000)) #STUDENTIZED - POINT PRESENT

#   (c) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing standardized residuals.

#Create data set for comparing the grades of people at different ages and IQ levels
agec <- c(23,25,23:50)
gradec <- c(70,69:96, 50)
grades.df.c <- data.frame(agec, gradec)

## Run the linear regression
grades.lm.c <- lm(gradec~agec,data=grades.df.c)
summary(grades.lm.c)

#Find standardized residuals
di <- stdres(grades.lm.c)

#PRESS residuals
pi <- PRESS(grades.lm.c)

#Residual plot vs. fitted values
fit.grades.c <- fitted(grades.lm.c)
plot(fit.grades.c,di, ylim=c(-80, 20)) #STANDARDIZED - BARELY OFF LINE
plot(fit.grades.c,pi$residuals, ylim=c(-80, 20)) #PRESS - MUCH MORE NOTICEABLE OF AN OUTLIER

#   (d) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing standardized residuals.

aged <- c(23,25,23:49, 50)
graded <- c(68:96, 50)
grades.df.d <- data.frame(aged, graded)

## Run the linear regression
grades.lm.d <- lm(graded~aged,data=grades.df.d)

#Find standardized residuals
di <- stdres(grades.lm.d)

#Find the R-student residuals
ti <- rstudent(grades.lm.d) #R-student -- e_i/((((S_i)^2)*(1-h_ii))^1/2)

#Residual plot vs. fitted values
fit.grades.d <- fitted(grades.lm.d)
plot(fit.grades.d,di, ylim=c(-100, 50)) #STANDARDIZED - BARELY OFF LINE
plot(fit.grades.d,ti, ylim=c(-100, 50)) #R-STUDENT - MUCH MORE NOTICEABLE OF AN OUTLIER

#   (e) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing studentized residuals.

agee <- c(23,25,23:50)
gradee <- c(70,69:97)
grades.df.e <- data.frame(agee, gradee)

## Run the linear regression
grades.lm.e <- lm(gradee~agee,data=grades.df.e)
summary(grades.lm.e)

#Studentized
ri <- studres(grades.lm.e)

#PRESS residuals
pi <- PRESS(grades.lm.e)

#Residual plot vs. fitted values
fit.grades.e <- fitted(grades.lm.e)
plot(fit.grades.e,ri, ylim=c(-5, 5)) #STUDENTIZED - NO OUTLIER PRESENT
plot(fit.grades.e,pi$residuals, ylim=c(-5, 5)) #PRESS - POINT PRESENT

#   (f) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing PRESS residuals.

#This is not possible, as PRESS will always identify influence points and highlight those. R-student works
#in a very similar way, but for a studentized distribution. As such, PRESS might pick up on points that
#R-student doesn't, but never the other way around.


#################
## Question 2: ##
#################

# For this problem you will use the file "data-table-B2.XLS".
#
prob2 <- read_xls('data-table-B2.xls')
#   (a) Fit the model using all explanatory variables. Iteratively remove insignificant variables
#       one-by-one until the all remaining variables are significant. Which variables remain in your model?
# I kept removing the least significant variable until I have all variables significant at 5%
lm.full <- lm(y ~ ., prob2)
summary(lm.full)
# Remove x5
lm.partial <- lm(y ~ . - x5, prob2)
summary(lm.partial)
# Remove x1
lm.partial <- lm(y ~ . - x5 - x1, prob2)
summary(lm.partial)
# Remove x2
lm.partial <- lm(y ~ . - x5 - x1 - x2, prob2)
summary(lm.partial)
# Only variables x3 and x4 remain in the model

#   (b) Compute each of the five types of residuals discussed in the textbook:
#       Residuals; Standardized residuals; Studentized residuals; PRESS residuals; R-student residuals.
#       You may use R functions.
## Find the residuals
ei<-resid(lm.partial)
# Find the standardized residuals
SS_res <- sum(ei^2)
df_res <- 26
MS_res <- SS_res / df_res
di <- res / sqrt(MS_res)
## Find the studentized residuals
ri<-rstandard(lm.partial)
# Find the PRESS residuals
# This code was adopted from 
pressi <- residuals(lm.partial)/(1-lm.influence(lm.partial)$hat)
## Find the R-student residuals
ti<-rstudent(lm.partial)

#   (c) Use the results from part(a) to decide if there appear to be any outliers and/or high 
#       influence points.
predictions <- fitted(lm.partial)
# Decided to produce a 3d plot to look at some of the points compared to the fitted surface
# predict on x-y grid, for surface
x3.pred <- seq(31, 41, length.out = 30)
x4.pred <- seq(15, 19.5, length.out = 30)
xy <- expand.grid(x3 = x3.pred, 
                  x4 = x4.pred)
xy <- data.frame(xy)
xy$x1 <- 0
xy$x2 <- 0
xy$x5 <- 0
y.pred <- matrix (nrow = 30, ncol = 30, 
                    data = predict(lm.partial, newdata = xy, interval = "prediction"))

# predicted z-values, fitted points for droplines to surface
fitpoints <- predict(lm.partial) 

scatter3D(z = prob2$y, x = prob2$x3, y = prob2$x4, pch = 18, cex = 2, 
          theta = 125, phi = 0, ticktype = "detailed",
          xlab = "x3", ylab = "x4", zlab = "y", 
          surf = list(x = x3.pred, y = x4.pred, z = y.pred, 
                      facets = NA, fit = fitpoints, col='black'),
          colkey = list(length = 0.8, width = 0.4),         
          main = "prob2")

# Also regular 2d plots
plot(prob2$x3, prob2$y)
plot(prob2$x4, prob2$y)
# It looks like the point at (x3,x4,y) = (35.90, 19.05, 181.5) could be an influential point. It doesn't fall 
# on the fit surface and looks like it has a high negative residual
# It also looks like the point (36.26, 17.62, 254.4) could be an influential point. It doesn't fall on the fit 
# surface and looks to have a high positive residual.
# The final influential point candidate is (36.76, 18.53, 196) because it also has a large negative residual

#  (d) Produce a normal probability plot of the R-student residuals and evaluate the plot for 
#       signs of departures from normality.
qqnorm(ti)
qqline(ti)
# So judging by the generated plot, it looks like we have an example of heavier tails here. That is,
# we get more extreme values on the edge of the distribution that we would actually expect if the 
# distribution of residuals was normal. Otherrwise though the model looks reasonably normally
# distributed within +/- 1 sd

#   (e) Produce plots of the R-student residuals vs. 
#       (1) the predicted values, and
plot(predictions, ti)
# It looks like this plot has random scatter. At least there is no obvious pattern of the points to me.
# It looks like this plot may not have constant variance. Especially for smaller predicted values it seems
#   there is a lot more variance in the data (but also fewer data points), than at the higher prediction values.
# It looks like this plot is pretty well centered around 0 for the residuals. With the exception of the R-student 
#   residual at 3, most points seem fairly centered around 0.
#       (2) each explanatory variable.
#       What assumptions may not be satisfied? Explain.
plot(prob2$x3, ti)
# It looks like this plot has random scatter. At least there is no obvious pattern of the points to me.
# It looks like this plot may not have constant variance. Especially for smaller and larger values of x3 it seems
#   there is a lot less variance in the data (but also fewer data points), than at the higher values of x3.
# It looks like this plot is pretty well centered around 0 for the residuals. With the exception of the R-student 
#   residual at 3, most points seem fairly centered around 0.
plot(prob2$x4, ti)
# It looks like this plot may not have random scatter. It seems like removing the 2 points at the largest value
#   of x4 would produce a resid plot that has a clearly discernible upward trend.
# It looks like this plot may not have constant variance. Especially for smaller values of x4 it seems
#   there is a lot less variance in the data (but also fewer data points), than at larger values of x4.
# It looks like this plot may not be centered around 0. I would argue that it seems centered around 0 for smaller
#   values of x4, but for larger values it seems to be a region of really positive resid followed by a region of 
#   negative resids





