# Introducción ----
"
Los modelos de árboles que utilizaremos serán:
1. Árbol de decisión (lo dejaremos crecer).
2. Árbol de decisión podado.
3. Bagging.
4. Random Forest.
5. Boosting.
6. BART.
"
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
mat <- read.table("student-mat.csv",sep=",",header=TRUE)

por[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(por[-c(3,13,14,15,30,31,32,33)], factor)
mat[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(mat[-c(3,13,14,15,30,31,32,33)], factor)

por["course_type"] = rep("P", nrow(por))
mat["course_type"] = rep("M", nrow(mat))

df <- rbind(por, mat)
df["course_type"] = lapply(df["course_type"], factor)

set.seed(9)
aux_train <- sample(1:nrow(df), nrow(df)*0.7)
aux_test <- (-aux_train)

train <- df[aux_train, ]
test <- df[aux_test, ]


n <- nrow(train)
p <- ncol(train)-1

y.hat <- mean(train$G3)
calcular_ecm <- function(y.hat) {
  return(mean((y.hat - test$G3)^2))
}
ECM_modelo_nulo <- calcular_ecm(y.hat) # 14.92127

comparaciones <- data.frame(Modelo="Nulo", "ECM test"=ECM_modelo_nulo)
agregar_modelo <- function(nombre_modelo, resultado) {
  return(
    rbind(comparaciones, data.frame(Modelo=nombre_modelo, "ECM test"=resultado))
  )
}
# Comparación de modelos ----
## Árbol crecido ----
longtree.fit <- tree(G3 ~ ., train,
                     control=tree.control(nobs = nrow(train), mindev = 0))
summary(longtree.fit)

y.hat.tree <- predict(longtree.fit, test)
ECM_arbol_total <- calcular_ecm(y.hat.tree)
comparaciones <- agregar_modelo("Árbol totalmente crecido", ECM_arbol_total)

## Árbol podado ----
prunning.fit <- prune.tree(longtree.fit, best=5)
plot(prunning.fit)
text(prunning.fit, pretty=0)

y.hat.prunning <- predict(prunning.fit, test)
ECM_prunning <- calcular_ecm(y.hat.prunning)
comparaciones <- agregar_modelo("Árbol podado", ECM_prunning)

## Bagging ----
set.seed(9)
bag.fit <- randomForest(G3 ~ ., data=train, mtry=p, importance=TRUE)

y.hat.bag <- predict(bag.fit, test)
ECM_bagging <- calcular_ecm(y.hat.bag)
comparaciones <- agregar_modelo("Bagging", ECM_bagging)

# Dejo crecer más el árbol todavía
set.seed(9)
bag.fit2 <- randomForest(G3 ~ ., data=train, 
                         mtry=p, ntree=1000)

y.hat.bag2 <- predict(bag.fit2, test)
calcular_ecm(y.hat.bag2) # no lo mejora.
## Random Forest ----
set.seed(9)
rf.fit <- randomForest(G3 ~ ., data=train, mtry=p/3, importance=TRUE)

y.hat.rf <- predict(rf.fit, test)
ECM_rf <- calcular_ecm(y.hat.rf)
comparaciones <- agregar_modelo("Random Forest", ECM_rf)

## Boosting ----
boost.fit <- gbm(G3 ~ ., data=train,
                 distribution="gaussian", n.trees=5000)
summary(boost.fit)

y.hat.boost <- predict(boost.fit, test)
ECM_boost <- calcular_ecm(y.hat.boost)
comparaciones <- agregar_modelo("Boosting", ECM_boost)

## BART ----
xtrain <- train[, -c(33)]
ytrain <- train$G3
xtest <- test[, -c(33)]
ytest <- test$G3

set.seed(9)
barfit <- gbart(xtrain, ytrain, x.test=xtest)
yhat.bar <- barfit$yhat.test.mean
ECM_bart <- mean((ytest - yhat.bar)^2)

comparaciones <- agregar_modelo("BART", ECM_bart)

ord <- order(barfit$varcount.mean, decreasing=T)
barfit$varcount.mean[ord]

## XGBoost ----
library(xgboost)
library("tidyverse")
library("caret")

train2 <- map_df(train, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

test2 <- map_df(test, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

head(train2)

train_mat <- train2 %>% 
  select(-G3) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = train2$G3)

test_mat <- test2 %>% 
  select(-G3) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = test2$G3)

xg.fit <- xgboost(data = train_mat, 
                  nrounds = 10, max.depth = 2, eta = 0.3, nthread = 2)

xg.fit

pred <- predict(xg.fit, test_mat)
ECM_xgboost <- mean((pred-test$G3)^2)
comparaciones <- agregar_modelo("XGBoost", ECM_xgboost)
