
###########################
#                         #
#   Team Assignment 8     #
#                         #
###########################

## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################


# For this team assignment you will use the file "teamassign08train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign08test.csv". 
#
# These data are from credit card applications with the variable names and values
# changed for confidentiality. Information regarding the variables is given below.
#
library(MASS)
library(car)

train <- read.csv("teamassign08train.csv")
test <- read.csv("teamassign08test.csv")

# Plot response variable against each predictor
par(mfrow = c(2, 3))
plot(train$A1, train$A16)
plot(train$A2, train$A16)
plot(train$A3, train$A16)
plot(train$A4, train$A16)
plot(train$A5, train$A16)
plot(train$A6, train$A16)
plot(train$A7, train$A16)
plot(train$A8, train$A16)
plot(train$A9, train$A16)
plot(train$A10, train$A16)
plot(train$A11, train$A16)
plot(train$A12, train$A16)
plot(train$A13, train$A16)
plot(train$A14, train$A16)
plot(train$A15, train$A16)

# Factorize categorical variables
train$A1 <- as.factor(train$A1)
train$A4 <- as.factor(train$A4)
train$A5 <- as.factor(train$A5)
train$A6 <- as.factor(train$A6)
train$A7 <- as.factor(train$A7)
train$A9 <- as.factor(train$A9)
train$A10 <- as.factor(train$A10)
train$A12 <- as.factor(train$A12)
train$A13 <- as.factor(train$A13)

# Convert factor A2 and A14 to numeric
train$A2 <-as.numeric(levels(train$A2))[train$A2]
train$A14 <-as.numeric(levels(train$A14))[train$A14]

# Create a function to return mode
mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# As A2 and A14 are continuous numeric values, in order to impute missing values,
# we need the mean of all their observations. So we substitute "?" in the two columns with 0.
for (i in 1:nrow(train)){
  if (is.na(train$A2[i])){
    train$A2[i] <- 0
  }
}
for (i in 1:nrow(train)){
  if (is.na(train$A14[i])){
    train$A14[i] <- 0
    print(i)
  }
} # i = 100 and 350 are missing values

# Deal with missing values (?)
# There are too many missing values for observation 100 and 350, so delete the two rows
train <- train[-c(100, 350),]
for (i in 1:nrow(train)){
  if (train$A1[i] == "?"){
    train$A1[i] <- mode(train$A1)
  }
  if (train$A2[i] == 0){
    train$A2[i] <- mean(train$A2)
  }
  if (train$A6[i] == "?"){
    train$A6[i] <- mode(train$A6)
  }
  if (train$A7[i] == "?"){
    train$A7[i] <- mode(train$A7)
  }
}

# Build a linear model
lg <- glm(A16 ~., data = train, family = "binomial")
summary(lg) # AIC: 297.03
anova(lg, test = "Chisq")

# Fit another model using significant variables in anova
lg1 <- glm(A16 ~ A2 + A3 + A4 + A6 + A8 + A9 + A11 + A13, data = train, family = "binomial")
summary(lg1) # AIC: 284.99

# Use stepwise function to find the best model that minimizes AIC
fit <- glm(A16 ~., data = train, family = "binomial")
step <- step(fit, direction="both")
step$anova

# Fit a model based on results from stepwise regression
lg2 <- glm(A16 ~ A1 + A2 + A3 + A4 + A5 + A6 + A8 + A9 + A10 + A11 + A12 + 
             A13 + A14 + A15, data = train, family = "binomial")
summary(lg2) # AIC: 281.95


# Factorize categorical variables
test$A1 <- as.factor(test$A1)
test$A4 <- as.factor(test$A4)
test$A5 <- as.factor(test$A5)
test$A6 <- as.factor(test$A6)
test$A7 <- as.factor(test$A7)
test$A9 <- as.factor(test$A9)
test$A10 <- as.factor(test$A10)
test$A12 <- as.factor(test$A12)
test$A13 <- as.factor(test$A13)

# Convert factor A2 and A14 to numeric
test$A2 <-as.numeric(levels(test$A2))[test$A2]
test$A14 <-as.numeric(levels(test$A14))[test$A14]

# As A2 and A14 are continuous numeric values, in order to impute missing values,
# we need the mean of all their observations. So we substitute "?" in the two columns with 0.
for (i in 1:nrow(test)){
  if (is.na(test$A2[i])){
    test$A2[i] <- 0
  }
}
for (i in 1:nrow(test)){
  if (is.na(test$A14[i])){
    test$A14[i] <- 0
    print(i)
  }
}
# i = 63, 86, 120, 144, 164, 184, 186, 187, 191, 246, 290 are missing values

# Deal with missing values (?)
test$A14[63] <- mean(test$A14)
test$A14[86] <- mean(test$A14)
test$A14[120] <- mean(test$A14)
test$A14[144] <- mean(test$A14)
test$A14[164] <- mean(test$A14)
test$A14[184] <- mean(test$A14)
test$A14[186] <- mean(test$A14)
test$A14[187] <- mean(test$A14)
test$A14[191] <- mean(test$A14)
test$A14[246] <- mean(test$A14)
test$A14[290] <- mean(test$A14)
for (i in 1:nrow(test)){
  if (test$A1[i] == "?"){
    test$A1[i] <- mode(test$A1)
  }
  if (test$A2[i] == 0){
    test$A2[i] <- mean(test$A2)
  }
  if (test$A4[i] == "?"){
    test$A4[i] <- mode(test$A4)
  }
  if (test$A5[i] == "?"){
    test$A5[i] <- mode(test$A5)
  }
  if (test$A6[i] == "?"){
    test$A6[i] <- mode(test$A6)
  }
  if (test$A7[i] == "?"){
    test$A7[i] <- mode(test$A7)
  }
}

# Predict on test set
probs <- as.vector(predict(lg2, newdata = test, type = "response"))
predvect <- rep(0, 290)  # Initialize prediction vector
predvect[probs > 0.5] <- 1
predvect

# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:
write.table(predvect, file="teamassign08preds.csv", row.names=F, col.names=F, sep=",")
#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.
#
#
# List of variables and values:
# A1:  b, a.
# A2:  continuous.
# A3:	 continuous.
# A4:	 u, y, l, t.
# A5:	 g, p, gg.
# A6:	 c, d, cc, i, j, k, m, r, q, w, x, e, aa, ff.
# A7:	 v, h, bb, j, n, z, dd, ff, o.
# A8:  continuous.
# A9:	 t, f.
# A10: t, f.
# A11: continuous.
# A12: t, f.
# A13: g, p, s.
# A14: continuous.
# A15: continuous.
# A16: 0,1 (response variable: whether the application was approved or denied)
