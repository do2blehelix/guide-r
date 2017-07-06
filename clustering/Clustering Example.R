


library("cluster")
library("factoextra")


# Load data
data("USArrests")
my_data <- USArrests

# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)

# Scale variables
my_data <- scale(my_data)

# View the firt 3 rows
head(my_data, n = 3)




res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



