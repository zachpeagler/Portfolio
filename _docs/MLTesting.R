# Playing around with machine learning

# dependencies
library("dplyr")
library("ggplot2")
library("mlbench")
library("MASS")
library("modeldata")
library("class")
library("caret")
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

# kNN
args(knn)
## k represents the number of nearest neighbors to consider.
set.seed(123)
train <- sample(150, 50)
test <- sample(150, 50)

knnres <- knn(iris[train, -5], iris[test, -5], iris$Species[train], prob = TRUE)
## check knn results against actual values
table(knnres, iris$Species[test])
## model accuracy
mean(knnres == iris$Species[test])
## new model to see what the most accurate k is
### initialize knn Total df
knn1 <- knn(iris[train, -5], iris[test, -5], iris$Species[train], k = 1)
knnTotal <- as.data.frame(1) %>%
  rename(k = "1")
knnTotal$accuracy <- mean(knn1 == iris$Species[test])
nk <- c(2:5)
for (i in nk) {
  tempknn <- knn(iris[train, -5], iris[test, -5], iris$Species[train], k = i)
  df <- as.data.frame(i) %>%
    rename(k = "i")
  df$accuracy <- mean(tempknn == iris$Species[test])
  knnTotal <- rbind(knnTotal, df)
}

# model performance
## use "caret" package
data("diamonds")
mod1 <- lm(price ~ ., diamonds)
p <- predict(mod1, diamonds)
## Error on prediction
error <- p - diamonds$price
rmse_in <- sqrt(mean(error^2)) ## in-sample RMSE
rmse_in
## random sample of 80% of diamond data
set.seed(123)
dtest <- sample(53940, (53940 * .8))
mod2 <- lm(price ~ carat + cut + color + clarity, diamonds[dtest,])
p2 <- predict(mod2, diamonds[-dtest, ])
error2 <- p2 - diamonds$price[-dtest]
rmse_out <- sqrt(mean(error2^2))
## random sample of 80% of diamond data
set.seed(50)
dtest <- nrow(diamonds) * .8
mod3 <- lm(price ~ carat, diamonds[dtest,])
p3 <- predict(mod3, diamonds[-dtest, ])
error3 <- p3 - diamonds$price[-dtest]
rmse3 <- sqrt(mean(error3^2))

# cross-validation
set.seed(123)
mod4 <- train(price ~., diamonds,
              method = "lm",
              trControl = trainControl(
                method = "cv",
                number = 10,
                verboseIter = FALSE
              ))
p4 <- predict(mod4, diamonds)
error4 <- p4 - diamonds$price[-dtest]
rmse4 <- sqrt(mean(error4^2))

# boston median home price 10-fold cross-validation test case
set.seed(123)
data("Boston")
