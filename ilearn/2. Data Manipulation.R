

################### Basic Data Manipulation 1 ###################

#create a data frame
mydata <- data.frame(u=c(4,8,6,4), v=c(9,27,7,8), z=c(1,7,17,3))

#sum normally
mydata$sumx <- mydata$u + mydata$v + mydata$z

#attach data for removing the precursor
attach(mydata)

#sum after attach (note the precursor isnt used)
mydata$sumy <- u + v + z

#detach dataset as attaching keeps data in RAM
detach(mydata)

#sum using transform (even after detach)
transform(mydata, sumz=u+v+z)




################### Basic Data Manipulation 2 ###################


#create vectors
executive <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country <- c("US","US","UK","UK","UK")
gender <- c("M","F","F","M","F")
age <- c(32,45,25,39,99)
u1 <- c(5,3,3,3,2)
u2 <- c(4,5,5,3,2)
u3 <- c(5,2,5,4,1)
u4 <- c(5,5,5,NA,2)
u5 <- c(5,5,2,NA,1)

#join vectors to form data frame
company <- data.frame(executive,date,country,gender,age,u1,u2,u3,u4,u5 , stringsAsFactors = FALSE)
company




#selecting VARIABLES using box reference
newdata <- company[,c(3,6:10)]
newdata

#selecting VARIABLES by names
newdata <- company[c("country","u1","u2","u3","u4","u5")]
newdata

#excluding VARIABLES using box reference
newdata <- company [c(-8,-10)]

#excluding VARIABLES using %in%
myvars <- names (company) %in% c("u3","u5")
myvars
newdata <- company[!myvars] 
newdata

#removing of VARIABLES by assigning Null
newdata$gender <- newdata$age <- NULL
newdata





#selecting OBSERVATIONS using box reference
newdata <- company[1:3,]
newdata

#selecting obeservations based on condition
newdata <- company[which(company$gender == "M" & age > 35),]
newdata

newdata <- company [which(gender == "F" & age > 25),]
newdata






################### Basic Data Manipulation 3 ###################


#date calculation : convert string to date
summary(company$date)
company$date <- as.Date(company$date, "%m/%d/%y")
summary(company$date)

#OBSERVATION filtering based on date
newdata <- company[which(company$date >= "2009-01-01" & company$date <= "2009-10-31"),]
newdata

#subset data based on conditions and specified columns
newdata <- subset(company, age>35 | age<24, select = c(u1,u2,u3,u4))
newdata

#subset based on conditions and ranged columns
newdata <- subset(company, gender == "M" & age >25, gender:u4)
newdata






################### Variable Creation and renaming ###################

#create new variable based on another
newdata <- within(company,{
  agecat <- NA
  agecat [age > 75] = "Elder"
  agecat [age > 35 & age <75] = "Middle aged"
  agecat [age <35] = "Young"
})
newdata

#rename using reshape package
library(reshape)
newdata <- rename(company, c(executive="exID",country="place"))
newdata

#rename using box reference
names(newdata)[4] <- "sex"
newdata

#rename multiple variables
names(newdata)[6:10] <- paste("a",11:15,sep="_")
newdata


#Melting Data {reshape package}
newdata <- melt(company, id=c("executive","date","country","gender","age"), na.rm = TRUE)
newdata

#Casting Data {reshape package}
newdata <- cast(newdata,country~variable,mean)
newdata






################### Missing Values ###################

#to show if NA exists in data
is.na(company)

#to remove NA
newdata <- na.omit(company) ; newdata







################### Group by ###################
library(dplyr)
library(sqldf)

filter(summarise(select(group_by(mtcars,cyl,gear),mpg,cyl,wt,gear,am),avgmpg=mean(mpg),avgwt=mean(wt)),avgmpg>15)

mtcars %>% group_by(cyl,gear) %>%
  select(mpg,cyl,wt,gear,am) %>%
    summarise(avgmpg = mean(mpg), avgwt = mean(wt)) %>%
  filter(avgmpg > 15)


