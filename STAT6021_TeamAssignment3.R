
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
data1 <- read.csv("teamassign03data01.csv") # Read in data

#   (a) Repeat (1)-(3) 1000 times:

intercept <- c()
x1_coeff <- c()
x2_coeff <- c()
x3_coeff <- c()
sigintcp <- c()
sigx1 <- c()
sigx2 <- c()
sigx3 <- c()
set.seed(1)

for (i in 1:1000){
  
#       (1) Select a random sample of 100 observations from data01.
  rand_data1 <- data1[sample(nrow(data1), 100),]
#       (2) Fit a linear model to the 100 observations using all three variables. Save the values
#           of the estimated coefficients (including the intercept) in separate vectors.
  lm1 <- lm(y ~., data = rand_data1)
  # Extract the values for coefficients and append them to corresponding vectors
  intercept <- c(intercept, summary(lm1)$coefficients[1, 1])
  x1_coeff <- c(x1_coeff, summary(lm1)$coefficients[2, 1])
  x2_coeff <- c(x2_coeff, summary(lm1)$coefficients[3, 1])
  x3_coeff <- c(x3_coeff, summary(lm1)$coefficients[4, 1])
#       (3) Conduct the individual t-test on each coefficient (including the intercept) and record
#           whether the estimate is found to be significant.
  # t value of 95% confidence interval is qt(0.975, df=98) = 1.984467
  # If the t value of the coefficient is larger than 1.984467, then it is significant
  if (summary(lm1)$coefficients[1, 3] > qt(0.975, df=98)){
    sigintcp <- c(sigintcp, 1) # 1 if significant, 0 when insignificant
  } else{
    sigintcp <- c(sigintcp, 0)
  }
  if (summary(lm1)$coefficients[2, 3] > qt(0.975, df=98)){
    sigx1 <- c(sigx1, 1) # 1 if significant, 0 when insignificant
  } else{
    sigx1 <- c(sigx1, 0)
  }
  if (summary(lm1)$coefficients[3, 3] > qt(0.975, df=98)){
    sigx2 <- c(sigx2, 1) # 1 if significant, 0 when insignificant
  } else{
    sigx2 <- c(sigx2, 0)
  }
  if (summary(lm1)$coefficients[4, 3] > qt(0.975, df=98)){
    sigx3 <- c(sigx3, 1) # 1 if significant, 0 when insignificant
  } else{
    sigx3 <- c(sigx3, 0)
  }
}
#       (4) Compute the mean of each vector containing the coefficients and compute the proportion of
#           times that each coefficient was found to be significant. Record these values.

mean(intercept) # 11.20536
mean(x1_coeff) # 5.28508
mean(x2_coeff) # 131.8363
mean(x3_coeff) # 1.717074
sigintcp_prop <- sum(sigintcp) / 1000
sigintcp_prop # 0.033
sigx1_prop <- sum(sigx1) / 1000
sigx1_prop # 1
sigx2_prop <- sum(sigx2) / 1000
sigx2_prop # 1
sigx3_prop <- sum(sigx3) / 1000
sigx3_prop # 0.999

#   (b) Repeat part (a) using only the variables x1 and x3 in the model.

intercept <- c()
x1_coeff <- c()
x2_coeff <- c()
x3_coeff <- c()
sigintcp <- c()
sigx1 <- c()
sigx2 <- c()
sigx3 <- c()
set.seed(1)

for (i in 1:1000){
  
  #       (1) Select a random sample of 100 observations from data01.
  rand_data1 <- data1[sample(nrow(data1), 100),]
  #       (2) Fit a linear model to the 100 observations using all three variables. Save the values
  #           of the estimated coefficients (including the intercept) in separate vectors.
  lm1 <- lm(y ~ rand_data1$x1 + rand_data1$x3, data = rand_data1)
  # Extract the values for coefficients and append them to corresponding vectors
  intercept <- c(intercept, summary(lm1)$coefficients[1, 1])
  x1_coeff <- c(x1_coeff, summary(lm1)$coefficients[2, 1])
  x3_coeff <- c(x3_coeff, summary(lm1)$coefficients[3, 1])
  #       (3) Conduct the individual t-test on each coefficient (including the intercept) and record
  #           whether the estimate is found to be significant.
  # t value of 95% confidence interval is qt(0.975, df=98) = 1.984467
  # If the t value of the coefficient is larger than 1.984467, then it is significant
  if (summary(lm1)$coefficients[1, 3] > qt(0.975, df=98)){
    sigintcp <- c(sigintcp, 1) # 1 if significant, 0 when insignificant
  } else{
    sigintcp <- c(sigintcp, 0)
  }
  if (summary(lm1)$coefficients[2, 3] > qt(0.975, df=98)){
    sigx1 <- c(sigx1, 1) # 1 if significant, 0 when insignificant
  } else{
    sigx1 <- c(sigx1, 0)
  }
  if (summary(lm1)$coefficients[3, 3] > qt(0.975, df=98)){
    sigx3 <- c(sigx3, 1) # 1 if significant, 0 when insignificant
  } else{
    sigx3 <- c(sigx3, 0)
  }
}
#       (4) Compute the mean of each vector containing the coefficients and compute the proportion of
#           times that each coefficient was found to be significant. Record these values.

mean(intercept) # 9300.229
mean(x1_coeff) # 2.508636
mean(x3_coeff) # -8.311971
sigintcp_prop <- sum(sigintcp) / 1000
sigintcp_prop # 1
sigx1_prop <- sum(sigx1) / 1000
sigx1_prop # 0.025
sigx3_prop <- sum(sigx3) / 1000
sigx3_prop # 0.003

#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.

plot(data1$y, data1$x1)

plot(data1$y, data1$x2)

plot(data1$y, data1$x3)

# We see that x3 which was previously significant now shows up as insignificant. When we plot out y vs these variables, we observed that x2 is almost linearly correlated with y but x1 and x3 don't seem to demonstrate any meaningful relationship with y.

#################
## Question 2: ##
#################

# For this problem you will use the files "teamassign03data02.csv" and "teamassign03data03.csv" to 
# demonstrate through simulation the effects of multicollinearity on the variance of the regression 
# coefficients and how they influence the accuracy of predictions.
#
# Read in data

data02 <- read.csv("teamassign03data02.csv")
data03 <- read.csv("teamassign03data03.csv")


#   (a) Repeat the following 1000 times:


# create empty vector to store each coefficient

q2_x1_coeff <- c()
q2_x2_coeff <- c()
q2_x3_coeff <- c()
q2_x4_coeff <- c()

set.seed(23)
for ( i in 1:1000){
  
  #       (1) Select a random sample of 100 observations from data02.
  
  sample <- data02[sample(nrow(iris), 100, replace = FALSE), ]  # select 100 random observations
  
  #       (2) Fit a linear model to the 100 observations using all four variables. Save the values
  #           of the estimated coefficients in separate vectors.
  
  
  lm2 <- lm(y ~., data=sample)
  
  q2_x1_coeff <- c(q2_x1_coeff, summary(lm2)$coefficients[2])
  q2_x2_coeff <- c(q2_x2_coeff, summary(lm2)$coefficients[3])
  q2_x3_coeff <- c(q2_x3_coeff, summary(lm2)$coefficients[4])
  q2_x4_coeff <- c(q2_x4_coeff, summary(lm2)$coefficients[5])
  
}


#       (3) Use your linear model to predict the y-values given in data03 then compute the MSE
#           using these residuals. Save this value in a vector.

mses <- c()

# predict y-values using linear model
y_hat2 <- predict(lm2, newdata = data03, type = "response")

# compute mse and save the values in a vector
mse <- lm2$residuals^2
mses <- cbind(mses, mse)



#       (4) Compute the standard deviation for the vectors containing the coefficients and compute
#           the mean of the vector containing the MSEs. Record these values.


sd(q2_x1_coeff)  # 2.009702
sd(q2_x2_coeff)  # 2.027125
sd(q2_x3_coeff)  # 2.023239
sd(q2_x4_coeff)  # 0.08561664

# compute mean mse
mean(mses)  # 492.8647

#   (b) Choose a suitable variable to remove from the model. Repeat (1)-(4) given in part (a)
#       1000 times using this model.

# remove variable x3 (highest p-value)

data02$x3 <- NULL

# create empty vector to store each coefficient

q2_b_x1_coeff <- c()
q2_b_x2_coeff <- c()
q2_b_x4_coeff <- c()

for ( i in 1:1000){
  sample <- data02[sample(nrow(iris), 100, replace = FALSE), ]  # select 100 random observations
  
  lm3 <- lm(y ~., data=sample)
  
  q2_b_x1_coeff <- c(q2_b_x1_coeff, summary(lm3)$coefficients[2])
  q2_b_x2_coeff <- c(q2_b_x2_coeff, summary(lm3)$coefficients[3])
  q2_b_x4_coeff <- c(q2_b_x4_coeff, summary(lm3)$coefficients[4])
  
}

mses_b <- c()

# predict y-values using linear model
y_hat3 <- predict(lm3, newdata = data03, type = "response")

# compute mse and save the values in a vector
mse_b <- lm3$residuals^2
mses_b <- cbind(mses_b, mse_b)

sd(q2_b_x1_coeff)  # 0.08379399
sd(q2_b_x2_coeff)  # 0.08591905
sd(q2_b_x4_coeff)  # 0.08898976

# compute mean mse
mean(mses_b)  # 394.8633

#   (c) How do the results from parts (a)(4) and (b)(4) compare? Explain what you observe.

# part (a) (4) with all the variables shows higher standard deviation across all coefficients and higher mean MSE compared 
#to the model with removing the variable with the highest p-value significance.
