
################### Logistic Regression : Concept ###################

#create 2 vectors
sal <- seq(21000, 40000, 1000)
exp <- 2:21

#join to make a dataframe
da <- data.frame(sal,exp) ; da

#plot to view relation of experience with salary
plot(sal~exp, da)

#create a new binary variable and assign high/low 
da$sal.class <- ifelse(sal > 35000 , 1, 0) ; da

#plot the salary binary variable with experience
plot(sal.class~exp, da)

#building a linear regression model to find the predicted values
model <- lm(sal.class ~ exp , da)

#the predicted values (b0+b1x)
da$pred <- predict.lm(model , newdata = da) ; da


#P= e^(b0+b1x) / 1 + e^(b0+b1x)
da$logodds <- exp(da$pred) / (1 + exp(da$pred)) ; da


#plot logodds probability with exp
plot(logodds ~ exp, da)







################### Logistic Regression : Boston ###################
#taking the regular boston dataset and converting the DV into binary variable to run logit

rm(list=ls())
library(MASS); data(Boston)

names(Boston)

summary(Boston$medv)

#create a new variable with the DV using ifelse
Boston$class_medv <- ifelse (Boston$medv >= 25, "High", "Low")

#check if new variable is created
View(Boston)

#check the variable: variable is in character format
summary(Boston$class_medv)

#convert to factor so it becomes useful in the model
Boston$class_medv <- as.factor(Boston$class_medv)

#check variable: converted to factor
summary(Boston$class_medv)

#remove original medv variable
Boston$medv <- NULL
View(Boston)


#build model
model <- glm(class_medv ~ . , family = binomial(logit) , data = Boston)
summary(model)

#optimize model with step function
model <- step(model)
summary(model)


#removing "nox" manually as its p > 0.05
glm(formula = class_medv ~ rm + dis + rad + tax + ptratio + 
      lstat, family = binomial(logit), data = Boston)


#write the predicted values to the dataset : 
    #in this case the log odds values are written
Boston$pred <- predict.glm(model , Boston)
View(Boston)

#for probability values
Boston$pred <- predict.glm(model , Boston, type = "response")
View(Boston)

#flag the predicted values as hi/lo (< 0.5 since we modeled base value as 0)
Boston$flag <- as.factor(ifelse(Boston$pred <= 0.5, "High", "Low"))

#create a confusion matrix bearing the actual vs pred values
table(Actual = Boston$class_medv , Predicted = Boston$flag)

#overall model accuracy
(106+357)/(106+26+17+357)


