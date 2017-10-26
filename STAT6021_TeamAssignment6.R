###########################
#                         #
#   Team Assignment 6     #
#                         #
###########################

#Boh Young Suh
#Caitlin Dreisbach
#Sai Prakesh
#Isabelle Liu

## Please submit one set of answers per team.                    ##
## Your answers may be submitted as an annotated R file.         ##
## Please submit your plots in one PDF as a separate attachment. ##
###################################################################

install.packages("readxl")
library("readxl")

#################
## Question 1: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications. If it is 
# not possible, explain why it is not possible. For each part, include a plot that shows the 
# interesting points.
#
#   (a) The data set has a point that is identified by all of the leverage and influential point
#       diagnostics -- hat matrix diagonal, Cook's D, DFBETAS, and DFFITS.

data.a <- read_excel("data-table-B13.xls")
lm.a <- lm(y ~ x1+x2+x3, data = data.a)

summary(influence.measures(lm.a))
#     dfb.1_  dfb.x1  dfb.x2  dfb.x3  dffit   cov.r   cook.d  hat 
# 20 -2.98_*  4.09_* -5.36_*  4.60_* -5.90_*  0.22_*  4.90_*  0.55_*

# obervation 20 is identified by all of the leverage and influential points diagnostics.

plot(cooks.distance(lm.a))

#   (b) The data set has a point that is identified by Cook's D but not by DFBETAS.

#After testing multuple datasets including those from the book and the hospital text data,
#I don't think it is possible to have a point that is idnetified by Cook's D and not by DFBETAS. 
#The main component of a problem with a significant point highlighted by the DFBetas is
#that each observation has several measures of influence one for each coefficient in the parameter estimate. 
#Cook's D helps with this problem because it only presents a single summary measure for each observation.
#Because Cook's D is a single measure the the DFBetas includes multiple estimates of the coefficients it
#would require an extreme dataset to potentially satisfy this problem.

#   (c) The data set has a point that is identified by DFBETAS but not by Cook's D.

data.c <- read_excel("data-table-B5.xls")
lm.c <- lm(y ~ x1+x2+x3+x4, data = data.c)
summary(influence.measures(lm.c))

#      dfb.1_  dfb.x1  dfb.x2  dfb.x3  dfb.x4  dffit   cov.r   cook.d hat
#  9  -1.16_*  1.20_*  1.18_* -1.10_*  1.16_*  2.13_*  0.66    0.74   0.44

# observation 9 is identified by DFBETAS but not by Cook's D.

plot(cooks.distance(lm.c))

#   (d) The data set has a point that is identified by DFBETAS but not by DFFITS.

data.d <- read_excel("data-table-B1.xls")
lm.d <- lm(y ~ ., data = data.d)

summary(influence.measures(lm.d))
#   dfb.1_  dfb.x1  dfb.x2  dfb.x3  dfb.x4 dfb.x5 dfb.x6 dfb.x7 dfb.x8 dfb.x9  dffit   cov.r   cook.d hat  
#10  0.36   -1.23_*  0.18   -0.18    0.06   0.23  -0.27   0.54  -0.69   0.25   -1.78    0.17    0.25   0.37

plot(lm.d, data=data.d) #can see the residuals vs. the fitted points

#   (e) The data set has a point that is identified by DFBETAS and DFFITS but not by the hat matrix
#       diagonal.

data.e <- read_excel("data-table-B14.xls")
lm.e <- lm(y ~ x1+x2+x3+x4, data = data.e)
summary(influence.measures(lm.e))
#      dfb.1_  dfb.x1 dfb.x2  dfb.x3  dfb.x4  dffit   cov.r   cook.d  hat  
# 2    1.67_*  0.21  -3.77_*  0.05   -1.07_* -4.67_*  0.04_*  1.98_*  0.47

# observation 2 is identified by DFBETAS and DFFITS but not by the hat matrix

plot(cooks.distance(lm.e))

#   (f) The data set has a point that is identified by the hat matrix diagonal and DFFITS but not 
#       by Cook's D.

data.f <- read_excel("data-table-B14.xls")
lm.f <- lm(y ~ x2, data = data.f)
summary(influence.measures(lm.f))

#     dfb.1_  dfb.x2  dffit   cov.r   cook.d  hat
# 8  -0.47    0.94    1.03_*  1.10    0.48    0.24_*

# observation 8 is identified by the hat matrix diagonal and DFFITS but not by Cook's D.

plot(cooks.distance(lm.f))


#################
## Question 2: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that has a nonlinear relationship, but is suitable for simple linear regression with some 
# modifications and satisfies the given specifications. For each part, include at least a scatterplot 
# of the data and explain your criteria for assessing performance.
#
#   (a) The linear model using a transformed explanatory variable outperforms the model using the
#       quadratric term of the explanatory variable.


# transformed model

set.seed(1)
x <- rnorm(30, 20, 10)
slope <- 3
intercept <- 0
error <- rnorm(30, 0, 0.5)
y <- intercept + slope*log(x) + error

plot(x,y)

model1 <- lm(y~ log(x))

summary(model1)

# R square is 0.99

qqnorm(rstudent(model1))
qqline(rstudent(model1))

ti<-rstudent(model1)
yhat <- fitted(model1)
plot(yhat,ti)

# Scatter plot shows log relationship. R square is very high. Residuals plots look great

model2 <- lm(y~ x + x^2)

summary(model2)

# R square is 0.56

qqnorm(rstudent(model2))
qqline(rstudent(model2))

ti<-rstudent(model2)
yhat <- fitted(model2)
plot(yhat,ti)

# R square is quite low. 

#   (b) The linear model using the quadratric term of the explanatory variable outperforms the model 
#       using a transformed explanatory variable.

set.seed(1)
x <- rnorm(30, 20, 10)
slope <- 3
intercept <- 0
error <- rnorm(30, 0, 0.5)
y <- intercept + x + x^2 + error

plot(x,y)

model1 <- lm(y~ x + x^2)

summary(model1)

# R square is 0.90

qqnorm(rstudent(model1))
qqline(rstudent(model1))

ti<-rstudent(model1)
yhat <- fitted(model1)
plot(yhat,ti)

# The errors show an ideal quadratic pattern

model2 <- lm(y~ log(x))

summary(model2)

# R square is just 0.34

qqnorm(rstudent(model2))
qqline(rstudent(model2))

ti<-rstudent(model2)
yhat <- fitted(model2)
plot(yhat,ti)

# clearly the quadratic model is better

#   (c) The linear model using a transformed explanatory variable and the model using the quadratric 
#       term of the explanatory variable perform equally well.

set.seed(1)
x <- rnorm(30, 15, 2)
slope <- 3
intercept <- 0
error <- rnorm(30, 0, 0.5)
y <- intercept + log(x) + x^2 + error

plot(x,y)

model1 <- lm(y~ log(x))

summary(model1)

qqnorm(rstudent(model1))
qqline(rstudent(model1))

ti<-rstudent(model1)
yhat <- fitted(model1)
plot(yhat,ti)

# R square is 0.98

model2 <- lm(y~ x^2)

summary(model2)

# R square is 0.99

qqnorm(rstudent(model2))
qqline(rstudent(model2))

ti<-rstudent(model2)
yhat <- fitted(model2)
plot(yhat,ti)

# even the residuals plots look very similar
