# Lectura de datos ----
## Importo librerías
load_libraries <- function() {
  library(stargazer)
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

mat[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(mat[-c(3,13,14,15,30,31,32,33)], factor)


mat$freetime <- as.numeric(mat$freetime)
mat$famrel <- as.numeric(mat$famrel)
mat$goout <- as.numeric(mat$goout)
mat$Dalc <- as.numeric(mat$Dalc)
mat$Walc <- as.numeric(mat$Walc)
mat$health <- as.numeric(mat$health)

por$freetime <- as.numeric(por$freetime)
por$famrel <- as.numeric(por$famrel)
por$goout <- as.numeric(por$goout)
por$Dalc <- as.numeric(por$Dalc)
por$Walc <- as.numeric(por$Walc)
por$health <- as.numeric(por$health)

# Split train-test ----
set.seed(9)
aux_train <- sample(1:nrow(mat), nrow(mat)*0.8)
aux_test <- (-aux_train)
train.M <- mat[aux_train, ]
test.M <- mat[aux_test, ]

set.seed(9)
aux_train <- sample(1:nrow(por), nrow(por)*0.8)
aux_test <- (-aux_train)
train.P <- por[aux_train, ]
test.P <- por[aux_test, ]

# 1. Modelo con todos los predictores ----
ecm <- function(modelo, a_testear) {
  y.hat <- predict(modelo, a_testear)
  return(mean((y.hat - a_testear$G3)^2))
}

# Mat
m1.M <- lm(G3 ~ ., data=train.M)
summary(m1.M)

ecm(m1.M, test.M)
# Por
m1.P <- lm(G3 ~ ., data=train.P)
summary(m1.P)

ecm(m1.P, test.P)

# 2. Modelo únicamente con G2 ----
# Mat
m2.M <- lm(G3 ~ G2, data=train.M)
summary(m2.M)

ecm(m2.M, test.M)

# Por
m2.P <- lm(G3 ~ G2, data=train.P)
summary(m2.P)

ecm(m2.P, test.P)

# 3. Modelo únicamente con G1 y G2 ----
# Mat
m3.M <- lm(G3 ~ G1+G2, data=train.M)
summary(m3.M)

ecm(m3.M, test.M)

# Por
m3.P <- lm(G3 ~ G1+G2, data=train.P)
summary(m3.P)

ecm(m3.P, test.P)

# Elección primer modelo para cada materia ----
# Mat
# Recordar que anova va del modelo más complejo al más simple
anova(m1.M, m3.M, m2.M) # El modelo con solo G2 es el elegido.
par(mfrow=c(1,2))
plot(train.M$G1, train.M$G3, pch=20)
plot(train.M$G2, train.M$G3, pch=20)
par(mfrow=c(1,1))

# Por
anova(m1.P, m3.P, m2.P) # El modelo con solo G2 es el elegido.
par(mfrow=c(1,2))
plot(train.P$G1, train.P$G3, pch=20)
plot(train.P$G2, train.P$G3, pch=20)
par(mfrow=c(1,1))