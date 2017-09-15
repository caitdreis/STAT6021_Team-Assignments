
###########################
#                         #
#   Team Assignment 3     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################


#################
## Question 1: ##
#################

# For this problem you will use the file "teamassign03data01.csv" to # demonstrate through simulation
# the effects of leaving an important predictor out of the model.
#
#   (a) Repeat (1)-(3) 1000 times:
#       (1) Select a random sample of 100 observations from data01.
#       (2) Fit a linear model to the 100 observations using all three variables. Save the values
#           of the estimated coefficients (including the intercept) in separate vectors.
#       (3) Conduct the individual t-test on each coefficient (including the intercept) and record
#           whether the estimate is found to be significant.
#       (4) Compute the mean of each vector containing the coefficients and compute the proportion of
#           times that each coefficient was found to be significant. Record these values.
#   (b) Repeat part (a) using only the variables x1 and x3 in the model.
#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.


#################
## Question 2: ##
#################

# For this problem you will use the files "teamassign03data02.csv" and "teamassign03data03.csv" to 
# demonstrate through simulation the effects of multicollinearity on the variance of the regression 
# coefficients and how they influence the accuracy of predictions.
#
#   (a) Repeat the following 1000 times:
#       (1) Select a random sample of 100 observations from data02.
#       (2) Fit a linear model to the 100 observations using all four variables. Save the values
#           of the estimated coefficients in separate vectors.
#       (3) Use your linear model to predict the y-values given in data03 then compute the MSE
#           using these residuals. Save this value in a vector.
#       (4) Compute the standard deviation for the vectors containing the coefficients and compute
#           the mean of the vector containing the MSEs. Record these values.
#   (b) Choose a suitable variable to remove from the model. Repeat (1)-(4) given in part (a)
#       1000 times using this model.
#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.

