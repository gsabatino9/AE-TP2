# PORTUGUÉS ----
# Lectura de datos ----
## Importo librerías
load_libraries <- function() {
  library(MASS)
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
por <- read.table("student-por.csv",sep=",",header=TRUE)

por[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(por[-c(3,13,14,15,30,31,32,33)], factor)

set.seed(9)
aux_train <- sample(1:nrow(por), nrow(por)*0.7)
aux_test <- (-aux_train)

train <- por[aux_train, ]
test <- por[aux_test, ]


n <- nrow(train)
p <- ncol(train)-1

train$freetime <- as.numeric(train$freetime)
train$famrel <- as.numeric(train$famrel)
train$goout <- as.numeric(train$goout)
train$Dalc <- as.numeric(train$Dalc)
train$Walc <- as.numeric(train$Walc)
train$health <- as.numeric(train$health)

test$freetime <- as.numeric(test$freetime)
test$famrel <- as.numeric(test$famrel)
test$goout <- as.numeric(test$goout)
test$Dalc <- as.numeric(test$Dalc)
test$Walc <- as.numeric(test$Walc)
test$health <- as.numeric(test$health)

# Comparación de modelos ----
## Modelo nulo ----
y.hat <- mean(train$G3)
calcular_ecm <- function(y.hat) {
  return(mean((y.hat - test$G3)^2))
}
ECM_modelo_nulo <- calcular_ecm(y.hat) # 8.1731
comparaciones <- data.frame(Modelo="Nulo", "ECM test"=ECM_modelo_nulo)

agregar_modelo <- function(nombre, pred) {
  ECM <- calcular_ecm(pred)
  return(
    rbind(comparaciones, 
          data.frame(Modelo=nombre, "ECM test"=ECM))
  )
}

## Árbol crecido ----
longtree.fit <- tree(G3 ~ ., train,
                     control=tree.control(nobs = nrow(train), mindev = 0))
summary(longtree.fit)

y.hat.tree <- predict(longtree.fit, test)
comparaciones <- agregar_modelo("Árbol crecido", y.hat.tree)

## Árbol podado ----
prunning.fit <- prune.tree(longtree.fit, best=5)
plot(prunning.fit)
text(prunning.fit, pretty=0)

y.hat.prunning <- predict(prunning.fit, test)
comparaciones <- agregar_modelo("Árbol podado", y.hat.prunning)
## Bagging ----
set.seed(9)
bag.fit <- randomForest(G3 ~ ., data=train, mtry=p, importance=TRUE)

y.hat.bag <- predict(bag.fit, test)
comparaciones <- agregar_modelo("Bagging", y.hat.bag)

# Dejo crecer más el árbol todavía
set.seed(9)
bag.fit2 <- randomForest(G3 ~ ., data=train, 
                         mtry=p, ntree=1000)

y.hat.bag2 <- predict(bag.fit2, test)
calcular_ecm(y.hat.bag2) # lo mejora.

varImpPlot(bag.fit)
"
G2, absences, age, G1, failures, Medu.
"
## Random Forest ----
set.seed(9)
rf.fit <- randomForest(G3 ~ ., data=train, mtry=p/3, importance=TRUE)

y.hat.rf <- predict(rf.fit, test)
comparaciones <- agregar_modelo("Random Forest", y.hat.rf)

varImpPlot(rf.fit)
imps <- importance(rf.fit)
ord <- order(imps[, 2], decreasing=T)

importancias <- imps[ord, 2]
total <- sum(importancias)
(importancias / total) * 100 # para ver porcentajes relativos.
## Boosting ----
boost.fit <- gbm(G3 ~ ., data=train,
                 distribution="gaussian", n.trees=5000)
summary(boost.fit)

y.hat.boost <- predict(boost.fit, test)
comparaciones <- agregar_modelo("Boosting", y.hat.boost)

## BART ----
xtrain <- train[, -c(33)]
ytrain <- train$G3
xtest <- test[, -c(33)]
ytest <- test$G3

set.seed(9)
barfit <- gbart(xtrain, ytrain, x.test=xtest)
yhat.bar <- barfit$yhat.test.mean
ECM_bart <- mean((ytest - yhat.bar)^2)

ord <- order(barfit$varcount.mean, decreasing=T)
barfit$varcount.mean[ord] # Dalc como novedad