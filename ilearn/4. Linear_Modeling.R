
################### Linear Modeling : General ###################

data(mtcars)
summary(mtcars)

#simple linear regression
model <- lm(mpg~disp, data=mtcars)
summary(model)


#multiple linear regression
model <- lm(mpg~., data=mtcars)
summary(model)


#fit the model into the test data
datafit <- data.frame (mtcars, fit=fitted(model), residual=resid(model))
head(datafit)

#check the confidence intervals
head(predict(model,interval = "confidence"))

#find out the parameters
names(model)

#check for parameters
model$residuals
model$terms
model$coefficients





################### Linear Modeling : Boston ###################


library(MASS); data(Boston)

#summary stats
summary(Boston)

#check for correlation
View(cor(Boston))

#lstat has highest correlation with DV medv
model <- lm(medv ~ lstat, Boston)
summary(model)


#calculate the predicted values for model1
Boston$pred <- predict.lm(model, Boston)
Boston$prederr <- Boston$medv - Boston$pred

#check plot for error distribution
hist(Boston$prederr)

#now, match the summary values with the regression residuals
summary(Boston$prederr)


#plot the graphs for technical aspects
plot(Boston$medv , Boston$lstat)
abline(model, col="black")
abline(h=mean(Boston$medv),col="red")
abline(v=mean(Boston$lstat),col="purple")

#notice the result of the below formula is the B-coefficient for lstat
cov(Boston$medv , Boston$lstat) / var(Boston$lstat)

#notice the result of below formula is the residual standard error
sd(Boston$prederr)

#anova
anova(model)



#perform multiple regression
model <- lm(medv~. , data=Boston)
summary(model)

#Remove insignificant variables using AIC / optimize model
opt_model <- step(model, direction = "both") 
summary(opt_model)

#call the VIF function from CAR package
library(car)
vif(opt_model)

#Predict the values in boston using optimized model
Boston$pred <- predict.lm(opt_model, newdata = Boston)
View(Boston)

#CALCULATE THE RMSE
sqrt(mean((Boston$medv - Boston$pred)^2))

#Split the data into training and testing data
Boston$pred <- NULL

training_data <- Boston[sample(nrow(Boston),size=0.7*nrow(Boston)),]
testing_data <- Boston[-sample(nrow(Boston),size=0.7*nrow(Boston)),]


#run model on training data and optimize
model <- lm(medv~. , data=training_data)
summary(opt_model <- step(model, direction = "both"))


#using the optimized training model, predicting the test data
training_data$prediction <- predict.lm(opt_model, newdata = training_data)
testing_data$prediction <- predict.lm(opt_model, newdata = testing_data)

#calculate rmse for training & test data
sqrt(mean((training_data$medv - training_data$prediction)^2))
sqrt(mean((testing_data$medv - testing_data$prediction)^2))

#plot original vs predicted values
plot(training_data$medv , training_data$prediction)


#----------------------------------------------------#
#identify the predicted values and the residuals
data.frame(Boston, fitted(model), resid(model))

#plot various basic plots
plot(model)






################### Linear Modeling : Cars93 ###################

rm(list=ls())

#load data
data("Cars93")

#check structure
str(Cars93)

#check correlation between mpg(highway) vs mpg(city)
cor(Cars93$MPG.city , Cars93$MPG.highway)

#highway mpg is a good predictor for city mpg, hence removing the highway mpg, to model on new handicap data
car <- Cars93[-8]
rm(Cars93)

#run first iteration model
summary(model <- lm(MPG.city ~ . , data=car))

#model shows error as all factors are considered when building model automatically

#transforming the data to fit in all the factors to categorical variables
car2 <- model.matrix(MPG.city~. , data=car)
summary(car2 <- as.data.frame(car2))

#due to the presence of NA in data so many variables are formed
car2 <- na.omit(car)
summary(car2)

#transforming data to matrix
car3  <- model.matrix(MPG.city ~ . , data=car2)

#and back to data frame
car3 <- as.data.frame(car3)

#remove the intercept variable as its useless
car3$`(Intercept)` <- NULL

#now adding the dependent variable to the encoded datasat
car3$MPG.city <- car2$MPG.city

#building the model on the encoded dataset. Notice the Rsquared !
summary(model <- lm(MPG.city ~ . , data=car3))



