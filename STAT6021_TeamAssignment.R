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
intercept_lower_conf <- c()
intercept_high_conf <- c()
slope_high_conf <- c()
slope_lower_conf <- c()
predicted_values_lower <- c()
predicted_values_higher <- c()
predicted_values_lower_2 <- c()
predicted_values_higher_2 <- c()
ms_res <- c()

# Generate the linear model 1000 times

X6 <- data.frame(x = 18)

for (i in 1:1000) {
  y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)
  # Append the vector of y to the original y vector
  y_vec <- rbind(y_vec, y)
  lm <- lm(y~x)
  # Extract the values for coefficients and append them to corresponding vectors
  intercept <- c(intercept, summary(lm)$coefficients[1, 1])
  x_coeff <- c(x_coeff, summary(lm)$coefficients[2, 1])
  ms_res <- c(ms_res, anova(lm)$'Mean Sq'[2])
  intercept_lower_conf <- c(intercept_lower_conf, confint(lm)[1,1])
  intercept_high_conf <- c(intercept_high_conf, confint(lm)[1,2])
  slope_lower_conf <- c(slope_lower_conf, confint(lm)[2,1])
  slope_high_conf <- c(slope_high_conf, confint(lm)[2,2])
  predicted_values_lower <- c(predicted_values_lower, predict(lm, X6, interval="confidence", level=0.95)[2])
  predicted_values_higher <- c(predicted_values_higher, predict(lm, X6, interval="confidence", level=0.95)[3])
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

intercept_lower_conf = as.data.frame(intercept_lower_conf)
intercept_high_conf = as.data.frame(intercept_high_conf)

intercept_95 = cbind(intercept_lower_conf, intercept_high_conf)

i = 0

for (g in 1:nrow(intercept_high_conf)) { 
  if (intercept_high_conf[g,1] >= 25 & intercept_lower_conf[g,1] < 25) {
    i = i + 1
  }
}

i

# 957 values. 95.7 % of values have the value 25

slope_lower_conf = as.data.frame(slope_lower_conf)
slope_high_conf = as.data.frame(slope_high_conf)

slope_95 = cbind(slope_lower_conf, slope_high_conf)

i = 0

for (g in 1:nrow(slope_high_conf)) { 
  if (slope_high_conf[g,1] >= 4 & slope_lower_conf[g,1] < 4) {
    i = i + 1
  }
}

i

# 957. 95.7% of values have the value 4

# Well I guess both values should be close to 95


#   (d) Carry out the hypothesis test H0: beta_1 = 4 vs H1: beta_1 not= 4 at a 5% significance level. 
#       Determine and report the proportion of times that the null hypothesis is rejected, 
#       implying that beta_1 not= 4.

t_val <- c()
for (i in 1:1000){
  # Calculate the t value for each x coefficient, and store the absolute value of t
  t <- (x_coeff[i] - 4) / sd(x_coeff)
  t_val <- rbind(t_val, abs(t))
}
n <- 0
# Compare each t value to t0.025,1000-2, and reject null hypothesis if absolute value of t is bigger than 1.96
for (i in 1:1000){
  if (t_val[i] > 1.96){
    n <- n + 1
  }
}
n/1000 # 4.7%

#   (e) For each set of coefficients, find a 95% confidence interval for the mean
#       response associated with x = 18. Determine and report the percentage of your
#       intervals that contain the true value. What should the percentage be?

# the 95% confidence interval for all the values

predicted_values_lower = as.data.frame(predicted_values_lower)
predicted_values_higher = as.data.frame(predicted_values_higher)

predicted_values = cbind(predicted_values_lower, predicted_values_higher)

i = 0

for (g in 1:nrow(predicted_values)) { 
  if (predicted_values_higher[g,1] >= 97 & predicted_values_lower[g,1] < 97) {
    i = i + 1
  }
}

i

# 956 values have 97 which is 25 + 4*18. 95.6% values have it
# The percentage should be 95

#   (f) For each estimated mean response from part (d), find a corresponding
#       95% prediction interval for the response y. Generate one random response y based 
#       on the true model. Determine and report the percentage of intervals that contain the response.
#       What should the percentage be?

y_vec_2 <- c()

for (i in 1:1000) {
  y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)
  # Append the vector of y to the original y vector
  y_vec_2 <- rbind(y_vec_2, y)
  lm2 <- lm(y~x)
  predicted_values_lower_2 <- c(predicted_values_lower_2, predict(lm2, data.frame(x=18), interval="prediction", level=0.95)[2])
  predicted_values_higher_2 <- c(predicted_values_higher_2, predict(lm2, data.frame(x=18), interval="prediction", level=0.95)[3])
}

predicted_values_lower_2 = as.data.frame(predicted_values_lower_2)
predicted_values_higher_2 = as.data.frame(predicted_values_higher_2)

predicted_values = cbind(predicted_values_lower_2, predicted_values_higher_2)

i = 0

for (g in 1:nrow(predicted_values)) { 
  if (predicted_values_higher_2[g,1] >= 97 & predicted_values_lower_2[g,1] < 97) {
    i = i + 1
  }
}

i

# 1000/1000  100%

#   (g) Find and report a 95% confidence interval for sigma^2 by finding the 2.5th and 97.5th 
#       percentiles of the generated values of MS_Res to give the lower and upper confidence limits.

quantile(ms_res, c(.025, .975)) #MS_res based on above calculation
