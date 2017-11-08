
###########################
#                         #
#   Team Assignment 5     #
#                         #
###########################

## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################


# For this team assignment you will use the file "teamassign05train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign05test.csv". 
#

library(car)
library(MASS)

train <- read.csv('teamassign07train.csv')
test <- read.csv('teamassign07test.csv')

par(mfrow = c(2, 3))
plot(train$x1, train$y)
plot(train$x2, train$y)
plot(train$x3, train$y)
plot(train$x4, train$y)
plot(train$x5, train$y)
plot(train$x6, train$y)
plot(train$x7, train$y)
# x1, x3, x4 have two obvious clusters,
# and x1, x2, x4, x6 look like categorical variables
# with only few x values to select from

# Process training dataset
train$x11 <- 0
train$x12 <- 0
for (i in 1:nrow(train)){
  if (train$x1[i] < 675){
    train$x11[i] <- train$x1[i]
  } else{
    train$x12[i] <- train$x1[i]
  }
}
train$x11 <- as.factor(train$x11)
train$x12 <- as.factor(train$x12)

train$x2 <- as.factor(train$x2)

train$x31 <- 0
train$x32 <- 0
for (i in 1:nrow(train)){
  if (train$x3[i] < 180){
    train$x31[i] <- train$x3[i]
  } else{
    train$x32[i] <- train$x3[i]
  }
}

train$x41 <- 0
train$x42 <- 0
for (i in 1:nrow(train)){
  if (train$x4[i] < 150){
    train$x41[i] <- train$x4[i]
  } else{
    train$x42[i] <- train$x4[i]
  }
}
train$x41 <- as.factor(train$x41)
train$x42 <- as.factor(train$x42)

train$x6 <- as.factor(train$x6)

# Build a linear model
lm <- lm(y ~ .-x1 - x3 - x4, data = train)
summary(lm) # Adjusted R-squared:  0.9631
anova(lm) # MSE: 3.2

vif(lm) # "there are aliased coefficients in the model"
alias(lm) # There maybe perfect multicollinearity between x11 and x12, x41, x42

# Select significant variables and build another model
lm1 <- lm(y ~ x11 + x12 + x2 + x31 + x32 + x6 + x7, data = train)
summary(lm1) # Adjusted R-squared:  0.9632
anova(lm1) # MSE: 3.2

library(DAAG)
cvResults <- cv.lm(train, lm1, m = 5)
attr(cvResults, 'ms') # 3.59

# Do the same data preprocessing with test dataset
test$x11 <- 0
test$x12 <- 0
for (i in 1:nrow(test)){
  if (test$x1[i] < 675){
    test$x11[i] <- test$x1[i]
  } else{
    test$x12[i] <- test$x1[i]
  }
}
test$x11 <- as.factor(test$x11)
test$x12 <- as.factor(test$x12)

test$x2 <- as.factor(test$x2)

test$x31 <- 0
test$x32 <- 0
for (i in 1:nrow(test)){
  if (test$x3[i] < 180){
    test$x31[i] <- test$x3[i]
  } else{
    test$x32[i] <- test$x3[i]
  }
}

test$x41 <- 0
test$x42 <- 0
for (i in 1:nrow(test)){
  if (test$x4[i] < 150){
    test$x41[i] <- test$x4[i]
  } else{
    test$x42[i] <- test$x4[i]
  }
}
test$x41 <- as.factor(test$x41)
test$x42 <- as.factor(test$x42)

test$x6 <- as.factor(test$x6)

# Predict on test set
predvect <- as.vector(predict(lm1, newdata = test))

# Once you have predictexd the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
write.table(predvect, file="teamassign05preds.csv", row.names=F, col.names=F, sep=",")
#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.
