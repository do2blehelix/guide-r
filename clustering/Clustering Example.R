


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







ibrary(factoextra)
library(NbClust)

df <- scale(USArrests)
head(df)


# Elbow method
  fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
  fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
  set.seed(123)
  fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")
