i <- 2 ; s <- sprintf("The cube of %d is %d" , i , i^3)
#-----------------------------------------------------------------------#




###################   Operators : Logical, Relational ###################

# Import Boston Dataset
require(MASS) ; data(Boston)



#Summary
summary(Boston$nox)

#Transformation
Boston$nox_scale <- Boston$nox * 100
summary(Boston$nox_scale)

#Print Column
Boston[,"nox_scale" >= 87]
#Print Rows
Boston["nox_scale" >= 87,]

#Subset (subset boston where nox_scale ge 87 and show only values for the first 4 and 16th columns)
subset(Boston , nox_scale >= 87 , select = c(1:4,15))

#Summary with relational operator
summary(Boston$indus >= 20)
Boston["indus" >= 20]




###################   Looping Constructs ###################


#If Construct
Boston_new <- as.matrix(Boston)

if (class(Boston_new) == "data.frame")
{
  print("Yea, its a data frame")
}else
{
  print("Nay its na data frame. we convert...")
  Boston_new <- as.data.frame(Boston_new)
}



#For Loop (inefficient)

for (i in 1:nrow(Boston))
{
 Boston[i,16] <- i
 Boston[i,17] <- nrow(Boston) - i
}




###################   Filters ###################

library(dplyr)

data("Boston")
class(Boston)

#Filter
test <- filter(Boston , medv > 30) ; dim(test)

test <- filter(Boston , medv > 30) %>% select(1:4) ; dim(test)





################### Data Structures ###################

new_vect <- as.vector(Boston)
dim(new_vect)






################### User Defined Functions ###################

w <- function(x) return(x^2 + 1)
w(5)

z <- function(x,m) return((x+m)^2)
z(2,4)





