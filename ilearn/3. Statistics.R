

################### Descriptive Statistics ###################


#gives all summary statistics for the dataset
library(psych)
describe(mtcars)

#plot basic graphs
plot(mtcars$mpg)
hist(mtcars$mpg)
boxplot(mtcars$mpg)


#correlation
#Pearson : linear relationship 2 numerical
#Spearman rank : if relationship has either/or ordinal
cor(mtcars, use="complete.obs", method = "pearson")

#covariance
cov(mtcars)





################### Tests ###################

library(car)

#Normality Assumption check
qqPlot(lm(mpg~wt,data=mtcars),main="Q-Q Plot",labels=F)

#Homogeniety test
bartlett.test(mpg~cyl, data=mtcars)


#ANOVA

