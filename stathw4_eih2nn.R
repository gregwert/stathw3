
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
