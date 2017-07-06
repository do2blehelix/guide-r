

################### Logistic Regression : Currency Note Real/Fake : Case Study ###################

#read the file from uci repository
bank <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", header = FALSE, sep = ",")

#summary stats
summary(bank)

#checking the DV as a factor variable
summary(as.factor(bank$V5))

#can be used to manually edit the variable names
fix(bank)
#or setting names
names(bank) <- c("variance" , "skewness" , "kurtosis" , "entropy" , "class")

#creating sample
samp <- sample(1372, 1372*0.7)

#using the sample nos to obtain train & test data
train <- bank[samp,]
test <- bank[-samp,]


#running the 1st iteration on the training data
model <-  glm(class ~ . , data=train, family=binomial("logit"))
summary(model)


#hosmer and lemeshow
library(MKmisc)
HLgof.test(fit = fitted(model) , obs = as.factor(train$class))


#roc curve on training
library(pROC)
roc(train$class, predict.glm(model, train, "response"))
plot(roc(train$class, predict.glm(model, train, "response")))

#roc curve on test
plot(roc(test$class, predict.glm(model, test, "response")))


#using the model to predict full bank dataset
bank$pred <- predict.glm(model, bank, "response")


#confusion matrix
table(Actual = bank$class, Predicted = bank$pred > 0.5)



################### additional checks using packages ###################

library(InformationValue)

#plot roc curve
plotROC(actuals = bank$class , predictedScores = bank$pred)
plotROC(actuals = bank$class , predictedScores = bank$pred , returnSensitivityMat = TRUE)


#check senstitvity
sensitivity(actuals = bank$class , predictedScores = bank$pred)

#find the optimal cutoff
optimalCutoff(actuals = bank$class , predictedScores = bank$pred , optimiseFor = 'Ones')





################### Custom Fucntion for classification table ###################


#define the function
my_Sensitivity <- function(actual_behaviour = input_data , predicted_probabilities = input_probabilities)
{
  cut_val <- seq(0.01 , 0.99, by = 0.01)
  table <- data.frame(matrix(nrow = 99, ncol = 5))
  colnames(table) <- c("Specificity" , "Sensitivity" , "False Positive" , "False Negative" , "Probability Cut")
  for(i in 1:99)
  {
    base <- table(actual_behaviour,predicted_probabilities >= cut_val[i])
    table[i,1] <- base[1,1]/sum(base)
    table[i,2] <- base[2,2]/sum(base)
    table[i,3] <- base[1,2]/sum(base[1,])
    table[i,4] <- base[2,1]/sum(base[2,])
    table[i,5] <- cut_val[i]
  }
return(table)
}


#call the function
df <- my_Sensitivity(actual_behaviour = bank$class, predicted_probabilities = bank$pred)
View(df)


plot(df$Sensitivity, df$Specificity)
