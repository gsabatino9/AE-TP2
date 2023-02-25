# Lectura de datos ----
## Importo librerías
load_libraries <- function() {
  library(MASS)
  library(caret)
  library(ISLR2)
  library(carData)
  library(car)
  library(boot) # para usar cv.glm() para cv y boot() para bootstrap
  library(leaps) # para subset selection
  library(Matrix)
  library(glmnet) # para Ridge y Lasso
  library(tree)
  library(randomForest) # para bagging y randomForest
  library(gbm) # para boosting
  library(BART) # para Bayesian Additive Regression Trees
  library(ggplot2)
  library(dplyr)
  print("MASS, ISLR2, carData, car, boot, leaps, glmnet, tree, randomForest, gbm, BART")
}

load_libraries()
mat <- read.table("student-mat.csv",sep=",",header=TRUE)
por <- read.table("student-por.csv",sep=",",header=TRUE)

mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)
por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)

mat[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(mat[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], as.numeric)
por[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(por[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], as.numeric)


mat$G3 <- ifelse(mat$G3 == 0, 0, 1)
por$G3 <- ifelse(por$G3 == 0, 0, 1)

mat <- data.frame(lapply(mat, as.numeric))
por <- data.frame(lapply(por, as.numeric))

# Split train-test ----
set.seed(9)
aux_train <- sample(1:nrow(mat), nrow(mat)*0.7)
aux_test <- (-aux_train)
train.M <- mat[aux_train, ]
test.M <- mat[aux_test, ]

set.seed(9)
aux_train <- sample(1:nrow(por), nrow(por)*0.7)
aux_test <- (-aux_train)
train.P <- por[aux_train, ]
test.P <- por[aux_test, ]

# Clasificación 1 ----
# Mat
glm.fits <- glm(G3 ~ ., data=train.M, family=binomial)
summary(glm.fits)
glm.probs <- predict(glm.fits, test.M, type="response")
glm.probs[1:10]

glm.pred <- rep(1, nrow(test.M))
glm.pred[glm.probs < 0.5] = 0

table(glm.pred, test.M$G3)

mean(glm.pred == test.M)

103 / 106
8 / 13

# Por
glm.fits <- glm(G3 ~ ., data=train.P, family=binomial)
summary(glm.fits)
glm.probs <- predict(glm.fits, test.P, type="response")
glm.probs[1:10]

glm.pred <- rep(1, nrow(test.P))
glm.pred[glm.probs < 0.5] = 0

table(glm.pred, test.P$G3)

mean(glm.pred == test.P)

2 / 8

# Clasificación 2 ----
library(class)
train_X.M <- train.M[, -c(33)]
train_X.P <- train.P[, -c(33)]
test_X.M <- test.M[, -c(33)]
test_X.P <- test.P[, -c(33)]

train_Y.M <- train.M$G3
train_Y.P <- train.P$G3
test_Y.M <- test.M$G3
test_Y.P <- test.P$G3

# Mat
set.seed(9)
knn.pred <- knn(train_X.M, test_X.M, train_Y.M, k = 4)
table(knn.pred, test_Y.M)

# Por
set.seed(9)
knn.pred <- knn(train_X.P, test_X.P, train_Y.P, k = 4)
table(knn.pred, test_Y.P)
