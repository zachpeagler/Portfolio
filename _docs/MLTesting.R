# Playing around with machine learning

# dependencies
library("ggplot2")
library("mlbench")
library("MASS")
library("modeldata")
# data
## iris flowers
data("iris")
## cars
mtcars <- data(mtcars)
## diamonds
dimaonds <- data(diamonds)
## sonar
sonar <- data(Sonar)
## boston
boston <- data(Boston)
## churn
churn <- data("mlc_churn")
churnTrain <- mlc_churn[1:3000, ]
churnTest <- mlc_churn[3001:5000, ]

# k means clustering
x <- iris$Sepal.Length
cl <- kmeans(x, centers = 3, nstart = 10)
plot(x, col=cl$cluster)
## number of clusters
nk <- c(1:5)
## dummy data frame
clusters <- data.frame(matrix(ncol=2, nrow=0))
## for loop across clusters
for (i in nk){
  cl <- kmeans(x, centers = i, nstart = 10)
  df <- as.data.frame(i)
  df$TwithinSS <- cl$tot.withinss
  clusters <- rbind(clusters, df)
}
ggplot()+
  geom_line(aes(clusters$i, clusters$TwithinSS))

# PCA
irispca <- prcomp(iris[, -5])
summary(irispca)
## plot
biplot(irispca)

## calculate variances and percent and cumulative variance along PCs
var <- irispca$sdev^2
pve <- var/sum(var)
cumsum(pve)
