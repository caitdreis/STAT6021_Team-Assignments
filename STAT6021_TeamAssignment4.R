#################
## Question 1: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications. If it is 
# not possible, explain why it is not possible. For each part, include a plot that shows the 
# interesting points.
#
## Read in the data
bpdata <- read.table("Blood pressure data.txt", header=TRUE); bpdata

bp.lm <- lm(BP~weight, data=bpdata) #Run the linear regression
ei <-resid(bp.lm) ## Find the residuals

library("MASS") #need this package to calculate the studentized residual with studres()
studentized <- studres(bp.lm); studentized #Find the studentized residuals
rstudent <-rstudent(bp.lm); rstudent #Find the R-student residuals
standardized = rstandard(bp.lm); standardized #Find the standardized residuals

#Find the PRESS residuals
(r <- resid(bp.lm))
(pr <- resid(bp.lm)/(1 - lm.influence(bp.lm)$hat))
sum(r^2)
press<- sum(pr^2) #PRESS residual is 2061.659

qqnorm(rstudent(bp.lm)) #Normal probabilty plot
qqline(rstudent(bp.lm)) #fitted line to the probability plot

##Residual plot vs. fitted values with rstudent residuals
yhat <- fitted(bp.lm)
plot(yhat,rstudent)

##rstudent residual plots vs. explanatory variables
plot(bpdata$weight,rstudent)

#plot preidcted values and actual values
predict<- predict(bp.lm)
attach(bpdata)
library("ggplot2")
ggplot(bpdata, aes(x = BP, y = weight)) +
  geom_point() +
  geom_point(aes(y = predict), shape = 1)  

#   (a) The data set has a point that is clearly visible for all four types of residuals 
#       discussed -- standardized, studentized, PRESS, R-student.

#residuals on the y axis, BP on the x axis
plot(bpdata$BP, standardized, ylab="Standardized Residuals", xlab="Blood Pressure") 
plot(bpdata$BP, studentized, ylab="Studentized Residuals", xlab="Blood Pressure") 
plot(bpdata$BP, rstudent, ylab="R-Student Residuals", xlab="Blood Pressure") 
plot(bpdata$BP, pr, ylab="PRESS Residuals", xlab="Blood Pressure") 

#residuals on the x axis, BP on the y axis
plot(standardized, bpdata$BP, ylab="Standardized Residuals", xlab="Blood Pressure") 
plot(studentized,bpdata$BP, ylab="Studentized Residuals", xlab="Blood Pressure") 
plot(rstudent, bpdata$BP, ylab="R-Student Residuals", xlab="Blood Pressure") 
plot(pr,bpdata$BP, ylab="PRESS Residuals", xlab="Blood Pressure") 

#using weight as the xaxis
plot(bpdata$weight, standardized, ylab="Standardized Residuals", xlab="Blood Pressure") 
plot(bpdata$weight, studentized, ylab="Studentized Residuals", xlab="Blood Pressure") 
plot(bpdata$weight, rstudent, ylab="R-Student Residuals", xlab="Blood Pressure") 
plot(bpdata$weight, pr, ylab="PRESS Residuals", xlab="Blood Pressure") 

#There is one point that is at x=140 BP and the residual is the lowest that is constant across all 4 types.

#   (b) The data set has a point that stands out when viewing studentized residuals but not 
#       when viewing standardized residuals.

plot(standardized, bpdata$BP, ylab="Standardized Residuals", xlab="Blood Pressure") 
plot(studentized, bpdata$BP, ylab="Studentized Residuals", xlab="Blood Pressure") 

#   (c) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing standardized residuals.

plot(standardized, bpdata$BP, ylab="Standardized Residuals", xlab="Blood Pressure") 
plot(pr, bpdata$BP, ylab="PRESS Residuals", xlab="Blood Pressure") 

#   (d) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing standardized residuals.

plot(standardized, bpdata$BP, ylab="Standardized Residuals", xlab="Blood Pressure") 
plot(rstudent,bpdata$BP, ylab="R-Student Residuals", xlab="Blood Pressure") 

#   (e) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing studentized residuals.

plot(pr, bpdata$BP, ylab="PRESS Residuals", xlab="Blood Pressure") 
plot(studentized, bpdata$BP, ylab="Studentized Residuals", xlab="Blood Pressure") 

#   (f) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing PRESS residuals.

plot(pr, bpdata$BP, ylab="PRESS Residuals", xlab="Blood Pressure") 
plot(rstudent,bpdata$BP, ylab="R-Student Residuals", xlab="Blood Pressure") 

#################
## Question 2: ##
#################

# For this problem you will use the file "data-table-B2.XLS".
#
library("MASS")
library("readxl")
data <- read_excel("data-table-B2.xls")

#   (a) Fit the model using all explanatory variables. Iteratively remove insignificant variables
#       one-by-one until the all remaining variables are significant. Which variables remain in your model?

lm1 <- lm(y ~., data = data)
summary(lm1) # x5 is not significant
lm2 <- lm(y ~.-x5, data = data)
summary(lm2) # all variables are significant

#   (b) Compute each of the five types of residuals discussed in the textbook:
#       Residuals; Standardized residuals; Studentized residuals; PRESS residuals; R-student residuals.
#       You may use R functions.

residuals(lm2) # Residuals
residuals(lm2)/sqrt(anova(lm2)[["Mean Sq"]][5]) # Standardized residuals
rstandard(lm2) # Studentized residuals
sum((residuals(lm2)/(1 - lm.influence(lm2)$hat))^2) # PRESS residuals: 2847.167
rstudent(lm2) # R-student residuals

#   (c) Use the results from part(a) to decide if there appear to be any outliers and/or high 
#       influence points.



#   (d) Produce a normal probability plot of the R-student residuals and evaluate the plot for 
#       signs of departures from normality.

qqnorm(rstudent(lm2))
qqline(rstudent(lm2))

#   (e) Produce plots of the R-student residuals vs. 
#       (1) the predicted values, and
#       (2) each explanatory variable.
#       What assumptions may not be satisfied? Explain.

# (1)
plot(predict(lm2), rstudent(lm2))
# (2)
plot(data$x1, rstudent(lm2))
plot(data$x2, rstudent(lm2))
plot(data$x3, rstudent(lm2))
plot(data$x4, rstudent(lm2))

