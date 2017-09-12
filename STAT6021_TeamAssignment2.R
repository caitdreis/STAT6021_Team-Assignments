###########################
#                         #
#   Team Assignment 2     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################


#################
## Question 1: ##
#################

# For this problem you will use the data in the file "teamassign02data01.csv" to implement   
# a simple form of the bootstrap resampling method.  Repeat (a) and (b) 1000 times:
#
data <- read.csv("teamassign02data01.csv") # Read in data

#   (a) From the data set, select **with replacement** 100 random pairs (x,y).
#       You will have some repeats -- which is OK and expected.

sample_x <- sample(data[1]$x, replace=TRUE)
sample_y <- sample(data[2]$y, replace=TRUE)

pairing <- cbind(sample_x,sample_y)
View(pairing)

#   (b) Use your sample to generate a regression equation. Save the values of 
#       hat(beta_0) and hat(beta_1).

lm.1 <- lm(sample_y~sample_x)
summary(lm.1)
n=100
mse1 <- sum(lm.1$residuals^2)/n;mse1 #2092.287

#   (c) Find and report a 95% confidence interval for beta_0 and beta_1 by determining
#       the 2.5th and 97.5th percentiles for each set of values.  Do the confidence 
#       intervals contain the true parameter values?

beta_0 <- lm.1$coefficients[1]; beta_0
beta_1 <- lm.1$coefficients[2]; beta_1
  
quantile(beta_0, c(.025, .975)) 

quantile(beta_1, c(.025, .975)) 

#################
## Question 2: ##
#################

# Import the data set "teamassign02data02.csv" which contains 100 sets of data for 
# the variables x1, x2, ..., x20.  Repeat (a)-(c) 100 times: 
#

sigvar_prop <- c()
for (i in 1:100){

#   (a) Generate 100 y values according to the model y ~ N(10, var=5^2) and pair up 
#       the y-values with corresponding rows from the data set of x-values.
  
  data2 <- read.csv("teamassign02data02.csv")
  
  y <- rnorm(100, mean=10, sd = 5)
  data2 <- cbind(data2, y)
  
#   (b) On the data set from part (a), generate a multiple regression model with
#       all of the x-values as explanatory variables.
  
  data2.lm <- lm(y ~., data = data2)

#   (c) Determine the number of significant explanatory variables at the 5% level.
  
  summary(data2.lm) # x4 and x12

#   (d) Determine and report the proportion of significant variables in the 100
#       simulations. Compare this proportion with the expected theoretical value.
  
  numsig <- 0
  for (j in 2:20){
    if (summary(data2.lm)$coefficients[j, 4] <= 0.05){
      numsig <- numsig + 1
    }
  }
  sigvar_prop <- c(sigvar_prop, numsig/20)
}
sigvar_prop
mean(sigvar_prop) # 0.0535, which is really close to 0.05 (the expected theoretical value)
