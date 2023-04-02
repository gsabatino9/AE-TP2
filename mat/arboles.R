# MATEMÁTICA ----
# Lectura de datos ----
## Importo librerías
load_libraries <- function() {
  library(MASS)
  library(jtools)
  library(broom.mixed)
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
mat <- read.table("mat_transf.csv",sep=",",header=TRUE)
por <- read.table("por_transf.csv",sep=",",header=TRUE)

mat <- mat[, -1]
por <- por[, -1]

mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)
por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)

mat[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(mat[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], as.numeric)
por[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(por[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], as.numeric)

# Split train-test ----
set.seed(9)
aux_train <- sample(1:nrow(mat), nrow(mat)*0.8)
aux_test <- (-aux_train)
train <- mat[aux_train, ]
test <- mat[aux_test, ]
n <- nrow(train)
p <- ncol(train)-1

# Comparación de modelos ----
library(randomForestExplainer)
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

"varImpPlot(rf.fit)
imps <- importance(bag.fit)
ord <- order(imps[, 2], decreasing=T)

importancias <- imps[ord, 2]
total <- sum(importancias)
(importancias / total) * 100 # para ver porcentajes relativos."

# Dejo crecer más el árbol todavía
set.seed(9)
bag.fit2 <- randomForest(G3 ~ ., data=train, 
                         mtry=p, ntree=1000)

y.hat.bag2 <- predict(bag.fit2, test)
calcular_ecm(y.hat.bag2) # lo mejora.

## Random Forest ----
set.seed(9)
rf.fit <- randomForest(G3 ~ ., data=train, mtry=p/3, importance=TRUE)

y.hat.rf <- predict(rf.fit, test)
comparaciones <- agregar_modelo("Random Forest", y.hat.rf)

#varImpPlot(rf.fit)
"
G2, G1, absences, failures, Mjob, Fedu, age, Medu, Fjob, health,
famrel.
"
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
comparaciones <- rbind(comparaciones, data.frame(Modelo="BART", ECM.test=ECM_bart))

ord <- order(barfit$varcount.mean, decreasing=T)
barfit$varcount.mean[ord] # guardian como novedad

# Gráficos ----
forest <- rf.fit

## Gráficos 1 ----
#min_depth_frame <- min_depth_distribution(forest)
#save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)
plot_min_depth_distribution(min_depth_frame, mean_sample="relevant_trees")

## Importancia de variables ----
#importance_frame <- measure_importance(forest)
#save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame

plot_importance_ggpairs(importance_frame)
plot_multi_way_importance(importance_frame, x_measure = "mse_increase", 
                          y_measure = "node_purity_increase", 
                          size_measure = "p_value", no_of_labels = 5)

plot_multi_way_importance(importance_frame, x_measure = "mse_increase", 
                          y_measure = "node_purity_increase", 
                          no_of_labels = 5)
plot_multi_way_importance(importance_frame, x_measure = "mean_min_depth", 
                          y_measure = "p_value", 
                          no_of_labels = 5)
plot_importance_rankings(importance_frame)

## Interacción ----
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))

#interactions_frame <- min_depth_interactions(forest, vars)
#save(interactions_frame, file = "interactions_frame.rda")
load("interactions_frame.rda")
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

plot_min_depth_interactions(interactions_frame)
plot_predict_interaction(forest, test, "G2", "G1")
plot_predict_interaction(forest, test, "G2", "absences")
plot_predict_interaction(forest, test, "G1", "absences")
plot_predict_interaction(forest, test, "G2", "health")
plot_predict_interaction(forest, test, "G2", "age")
plot_predict_interaction(forest, test, "G1", "age")
