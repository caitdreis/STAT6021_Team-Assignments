
###########################
#                         #
#   Team Assignment 4     #
#                         #
###########################

## Please submit one set of answers per team.                  ##
## Your answers may be submitted as an annotated R file.       ##
## Please submit your plots in a PDF as a separate attachment. ##
#################################################################


#################
## Question 1: ##
#################

# For each part of this problem, you are to create (or find) a data set with about 30 observations 
# that is suitable for simple linear regression and satisfies the given specifications. If it is 
# not possible, explain why it is not possible. For each part, include a plot that shows the 
# interesting points.
#
#   (a) The data set has a point that is clearly visible for all four types of residuals 
#       discussed -- standardized, studentized, PRESS, R-student.
#   (b) The data set has a point that stands out when viewing studentized residuals but not 
#       when viewing standardized residuals.
#   (c) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing standardized residuals.
#   (d) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing standardized residuals.
#   (e) The data set has a point that stands out when viewing PRESS residuals but not when 
#       viewing studentized residuals.
#   (f) The data set has a point that stands out when viewing R-student residuals but not when 
#       viewing PRESS residuals.



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

# (2)
plot(data$x1, rstudent(lm2))
plot(data$x2, rstudent(lm2))
plot(data$x3, rstudent(lm2))
plot(data$x4, rstudent(lm2))

