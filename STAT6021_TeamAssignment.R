#Boh Suh
#Caitlin Dreisbach
#Sai Prakesh
#Isabelle Liu

setwd("~/Dropbox (Personal)/Academic/University of Virginia/Data Science Institute/Fall 2017/STAT 6021")

## Start with given x-values
x <- read.table("teamassign01data.txt")[,1]
x

## Generate corresponding y-values according to the model y ~ 25 + 4x + e, where e~N(0,var=12^2)
y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)

## Plot the relationship
plot(x,y, pch=20, cex=0.3)

#################
## Question 1: ##
#################

# Using the (x,y) from above, generate a linear model
lm.1 <- lm(y~x)
summary(lm.1)
mse1 <- sum(lm.1$residuals^2)/98;mse1 #171.7776

#   (a) Report the coefficients hat(beta_0) and hat(beta_1).
#hat(beta_0) = 26.9169
#hat(beta_1) = 3.9087

#   (b) Report the predicted value of y for x=18.
yhat <- 26.9169 + 3.9087*18
yhat #97.2735

#   (c) Report MS_Res.
aov(y~x) 
16834.2/98 #171.7776

#################
## Question 2: ##
#################

# Generate the linear model requested in Question 1 1000 times. Create a new vector of y-values 
# for each repetition.

# Create three vectors for vectors of y, values of intercept, and values of x coefficient
y_vec <- c()
intercept <- c()
x_coeff <- c()
# Generate the linear model 1000 times
for (i in 1:1000){
  y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)
  # Append the vector of y to the original y vector
  y_vec <- rbind(y_vec, y)
  lm <- lm(y~x)
  # Extract the values for coefficients and append them to corresponding vectors
  intercept <- c(intercept, summary(lm)$coefficients[1, 1])
  x_coeff <- c(x_coeff, summary(lm)$coefficients[2, 1])
}
# Convert the vectors of y to a dataframe
y_vec <- data.frame(y_vec)

#   (a) Determine and report the mean and variance of the generated coefficients.

# Find the mean and variance of coefficients for intercept
intercept_mean <- mean(intercept)
intercept_mean # 25.11058
intercept_var <- var(intercept)
intercept_var # 20.45632

# Find the mean and variance of coefficients for x
x_coeff_mean <- mean(x_coeff)
x_coeff_mean # 3.998241
x_coeff_var <- var(x_coeff)
x_coeff_var # 0.0116911

#   (b) Based on theoretical considerations, what should the mean and variance of the  
#       generated coefficients be? Explain your answer.

# The mean of coefficients for intercept should be 25,
# and the mean of coefficients for x should be 4,
# because the original model is y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)

Sxx <- sum((x-mean(x))^2)
var_intercept <- 144*(1/1000 + (mean(x)^2)/Sxx)
var_intercept # 19.01978
var_x_coeff <- 144/Sxx
var_x_coeff # 0.01182987

#   (c) Find a 95% confidence interval centered at each coefficient. Determine and report  
#       the percentage of intervals that contain the true value of the coefficient. 
#       What should the percentage be?


#   (d) Carry out the hypothesis test H0: beta_1 = 4 vs H1: beta_1 not= 4 at a 5% significance level. 
#       Determine and report the proportion of times that the null hypothesis is rejected, 
#       implying that beta_1 not= 4.


#   (e) For each set of coefficients, find a 95% confidence interval for the mean
#       response associated with x = 18. Determine and report the percentage of your
#       intervals that contain the true value. What should the percentage be?


#   (f) For each estimated mean response from part (d), find a corresponding
#       95% prediction interval for the response y. Generate one random response y based 
#       on the true model. Determine and report the percentage of intervals that contain the response.
#       What should the percentage be?


#   (g) Find and report a 95% confidence interval for sigma^2 by finding the 2.5th and 97.5th 
#       percentiles of the generated values of MS_Res to give the lower and upper confidence limits.
percentile
