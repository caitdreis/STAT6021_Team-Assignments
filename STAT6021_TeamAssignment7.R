###########################
#                         #
#   Team Assignment 7     #
#                         #
###########################

#Boh Young Suh
#Caitlin Dreisbach
#Sai Prakesh
#Isabelle Liu

## Please submit one set of answers per team.                                  ##
## Your answers should be submitted as a .csv file per the instructions below. ##
## You should also submit your annotated R code per the instructions below.    ##
#################################################################################


# For this team assignment you will use the file "teamassign07train.csv" to develop
# a linear model using whatever methods you consider appropriate. You will then use
# the model that you have developed to predict the values of the response variable
# corresponding to the explanatory variable values given in the file
# "teamassign07test.csv". 

# Just a basic start for our modeling...

train <- read.csv("teamassign07train.csv")
test <- read.csv("teamassign07test.csv")

lm <- lm(y ~., data = train)
summary(lm) # remove x5 which has the highest p value

lm2 <- lm(y ~ x1+x2+x3+x4+x6+x7, data = train)
summary(lm2) # remove x3 which is also not significant

lm3 <- lm(y ~ x1+x2+x4+x6+x7, data = train)
summary(lm3)

#Create predictions for test set
predict(lm3, newdata = test) # use predict function to apply the best model above to the test data


# Once you have predicted the values of the response variable for the testing set,
# you should save them to a vector called predvect and write them into a .csv file 
# using the following code:

predvect <- c(predict(lm3, newdata = test))
write.table(predvect, file="teamassign07preds.csv", row.names=F, col.names=F, sep=",")

#
# Your annotated R code should explain the reasoning behind your choices in 
# model selection and should be neatly organized.
#
# Your grade on this team assignment will be based on how well your model predicts
# the observed values relative to the other teams.
