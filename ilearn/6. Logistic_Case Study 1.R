

################### Logistic Regression : Bank Loan Default : Case Study ###################

#import the data
bank <- read.csv("bank-additional.csv",sep = ";")

#exploring the data
str(bank)
summary(bank)


#VARIABLE TREATMENTS / TRANSFORMATIONS
attach(bank)

#default (change yes to unknown)
bank$default <- as.factor(ifelse(default=="yes","unknown",as.character(default)))
summary(default)

#pdays
summary(pdays) 
#notice the large no of 999s overshadowing. check distribution
hist(pdays)
#investigate further
summary(as.factor(pdays))
table(pdays,y)
#exporing the variable shows that this can be converted to factor to gain insights
bank$pdays <- as.factor(pdays)


#previous
summary(previous)
summary(as.factor(previous))
table(previous,y)
bank$previous <- as.factor(previous)
summary(bank)


# standardizing variables : emp.var.rate | cons.price.idx | cons.conf.idx | euribor3m | nr.employed
bank$emp.var.rate <- (emp.var.rate - min(emp.var.rate)) / (max(emp.var.rate) - min(emp.var.rate))
bank$cons.price.idx <- (cons.price.idx - min(cons.price.idx)) / (max(cons.price.idx) - min(cons.price.idx))
bank$cons.conf.idx <- (cons.conf.idx - min(cons.conf.idx)) / (max(cons.conf.idx) - min(cons.conf.idx))
bank$euribor3m <- (euribor3m - min(euribor3m)) / (max(euribor3m) - min(euribor3m))
bank$nr.employed <- (nr.employed - min(nr.employed)) / (max(nr.employed) - min(nr.employed))
summary(bank)

detach(bank)

#transform the data for creating dummy vars
transformed <- model.matrix(y ~ . , data=bank)

#convert the transformed matrix to dataframe
transformed <- as.data.frame(transformed)

#check the new vars
names(transformed)

#remove the intercept
transformed$`(Intercept)` <- NULL

#add the Dependent variable to transformed
transformed$y <- bank$y

#check names
names(transformed)


#create a vector containing random row nos to sample data
samp <- sample(nrow(transformed),0.7*nrow(transformed))

#create train and test datasets.
#the row nos are random and are based on the above code
train <- transformed[samp,]
test <- transformed[-samp,]


#run the regression
model <- glm(y ~ . , binomial("logit"), train)
summary(model)

#filter using step
opt_model <- step(model, direction = "both")
summary(opt_model)


#manually itereate after step function to remove insignificant vars
opt_model2 <- glm(formula = y ~ jobtechnician + contacttelephone + 
      monthjun + monthmar + monthmay + monthnov + 
      monthsep + day_of_weekwed + duration + 
      pdays3 + pdays999 + cons.conf.idx + 
      nr.employed, family = binomial("logit"), data = train)
summary(opt_model2)




#calling for Hosmer Lemeshow Test
library(MKmisc)
HLgof.test(fit = fitted(opt_model2), obs = ifelse(train$y == "yes",1,0))


#now, use model to find predicted values
train$pred <- predict.glm(model , newdata = train , type = "response")
test$pred <- predict.glm(model , newdata = test , type = "response")

#using opt_model to find the pred values
train$pred_opt <- predict.glm(opt_model , newdata = train , type = "response")
test$pred_opt <- predict.glm(opt_model , newdata = test , type = "response")

#using opt_model2 to find the pred values
train$pred_opt2 <- predict.glm(opt_model2 , newdata = train , type = "response")
test$pred_opt2 <- predict.glm(opt_model2 , newdata = test , type = "response")




#validate the models to see performance : AUC/ROC
library(pROC)

#check the area under the curve : model1
roc(y ~ pred , data = train)

#check the roc curve plot : model1
plot(roc(y ~ pred , data = train))
plot(roc(y ~ pred , data = test))

#check the roc curve plot : model_opt
plot(roc(y ~ pred_opt , data = train))
plot(roc(y ~ pred_opt , data = test))

#check the roc curve plot : model_opt2
plot(roc(y ~ pred_opt2 , data = train))
plot(roc(y ~ pred_opt2 , data = test))


##roc
roc(original,predicted)



#Validate data : confusion matrix
#using std 0.5 cutoff (sensitivity & specificity looks ok)
table(Actual=train$y, Predicted=train$pred > 0.5)

#if we decrease the cutoff value, then misclassification rate increases
table(Actual=train$y, Predicted=train$pred > 0.3)




################### additional inference from results  ###################

#Calculate the chi-sq value from the Null and deviance residuals
pchisq(<nulldeviance> - <residualdeviance> , <nulldegf> - <residegf> , lower.tail=FALSE)



