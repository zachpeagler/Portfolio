# Playing around with machine learning

# dependencies
library("ggplot2")
library("mlbench")
library("MASS")
library("modeldata")
# data
## iris flowers
iris <- data(iris)
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
