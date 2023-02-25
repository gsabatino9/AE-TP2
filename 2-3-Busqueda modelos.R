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
train.M <- mat[aux_train, ]
test.M <- mat[aux_test, ]

set.seed(9)
aux_train <- sample(1:nrow(por), nrow(por)*0.8)
aux_test <- (-aux_train)
train.P <- por[aux_train, ]
test.P <- por[aux_test, ]

# Comparación de modelos ----
ecm <- function(modelo, a_testear) {
  y.hat <- predict(modelo, a_testear)
  return(mean((y.hat - a_testear$G3)^2))
}

ecm_pred <- function(predicciones, a_testear) {
  return(mean((predicciones - a_testear$G3)^2))
}

cmp.M <- matrix()
cmp.P <- matrix()
## 1. Modelo nulo 1 (promedio) ----
y_h.M <- mean(train.M$G3)
y_h.P <- mean(train.P$G3)

null1.M <- ecm_pred(y_h.M, test.M)
null1.P <- ecm_pred(y_h.P, test.P)
cmp.M[1,] <- null1.M
cmp.P[1,] <- null1.P


## 2. Modelo nulo 2 (solo G2) ----
# Mat
m2.M <- lm(G3 ~ G2, data=train.M)
summary(m2.M)

cmp.M <- rbind(cmp.M, ecm(m2.M, test.M))

plot(test.M$G2, test.M$G3, pch=20)
abline(m2.M, lwd=2, col="red") # acá si hay outliers
# Por
m2.P <- lm(G3 ~ G2, data=train.P)
summary(m2.P)

cmp.P <- rbind(cmp.P, ecm(m2.P, test.P))

plot(test.P$G2, test.P$G3, pch=20)
abline(m2.P, lwd=2, col="red")
## 3. Modelo con todos los predictores ----
# Mat
m3.M <- lm(G3 ~ ., data=train.M)
summary(m3.M)

cmp.M <- rbind(cmp.M, ecm(m3.M, test.M)) # es peor que G2 solo.

# Por
m3.P <- lm(G3 ~ ., data=train.P)
summary(m3.P)

cmp.P <- rbind(cmp.P, ecm(m3.P, test.P)) # es peor que G2 solo.

## 4. Backward Selection con todos los predictores ----
find_coefs_bwd <- function(train) {
  p <- ncol(train)-1
  
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(G3 ~., data = train,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:p),
                      trControl = train.control)
  step.model$results
  step.model$bestTune
  summary(step.model$finalModel)
  print(coef(step.model$finalModel, step.model$bestTune[1,]))
  return(step.model)
}
# Mat
plot(find_coefs_bwd(train.M))
m4.M <- lm(G3 ~ address+Mjob+nursery+famrel+goout+health
           +absences+G1+G2, data=train.M)
summary(m4.M)

cmp.M <- rbind(cmp.M, ecm(m4.M, test.M))

# Por
find_coefs_bwd(train.P)
m4.P <- lm(G3 ~ G1+G2, data=train.P)
summary(m4.P)

cmp.P <- rbind(cmp.P, ecm(m4.P, test.P)) # este si mejora G2 solo.

## 4.1. Forward Selection con todos los predictores ----
find_coefs_fwd <- function(train) {
  p <- ncol(train)-1
  
  set.seed(123)
  train.control <- trainControl(method = "cv", number = 10)
  step.model <- train(G3 ~., data = train,
                      method = "leapForward", 
                      tuneGrid = data.frame(nvmax = 1:p),
                      trControl = train.control)
  step.model$results
  step.model$bestTune
  summary(step.model$finalModel)
  print(coef(step.model$finalModel, step.model$bestTune[1,]))
}

# Mat
find_coefs_fwd(train.M)
m4.1.M <- lm(G3 ~ Mjob+nursery+famrel+goout+health
           +absences+G1+G2, data=train.M)
summary(m4.1.M)

cmp.M <- rbind(cmp.M, ecm(m4.1.M, test.M))

# Por
find_coefs_fwd(train.P) # Llego a lo mismo que con BWD.

## 5. Mixed Selection usando el criterio AIC ----
# Mat
set.seed(9)
m5.M <- stepAIC(m3.M, direction="both", trace=FALSE)
summary(m5.M)

cmp.M <- rbind(cmp.M, ecm(m5.M, test.M))

# Por
set.seed(9)
m5.P <- stepAIC(m3.P, direction="both", trace=FALSE)
summary(m5.P)

cmp.P <- rbind(cmp.P, ecm(m5.P, test.P))

## 6. Lasso con todos los predictores ----
model_lasso <- function(train, test) {
  x.train <- model.matrix(G3 ~ ., train)[, -1]
  y.train <- train$G3
  x.test <- model.matrix(G3 ~ ., test)[, -1]
  y.test <- test$G3
  grid <- 10^seq(10, -2, length=100)
  
  lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)
  
  set.seed(9)
  cv.out <- cv.glmnet(x.train, y.train, alpha=1)
  
  best_lambda <- cv.out$lambda.min
  a <- coef(lasso.mod, s=best_lambda)
  print(names(a[a[,1] != 0, ]))
}
"
  Son interesantes las variables que tira cada modelo porque es medio
  la conclusión del análisis inicial.
"
# Mat
model_lasso(train.M, test.M)

m6.M <- lm(G3 ~ address+Mjob+failures+schoolsup+nursery
           +famrel+goout+health+absences+G1+G2, data=train.M)
cmp.M <- rbind(cmp.M, ecm(m6.M, test.M))

# Por
model_lasso(train.P, test.P)

m6.P <- lm(G3 ~ sex+age+Mjob+Fjob+reason+traveltime+failures+activities
           +higher+freetime+goout+Dalc+Walc+health+absences+G1+G2, data=train.P)
cmp.P <- rbind(cmp.P, ecm(m6.P, test.P))

# Elecciones de mejor modelo ----
# Mat
# El mejor modelo es solo con G2:
summary(m2.M)
"
p-value: < 2.2e-16
Adjusted R-squared:  0.9269
"

best.M <- lm(G3 ~ G2, data=train.M)

# Por
# El mejor modelo es con G1+G2:
summary(m4.P)
"
p-value: < 2.2e-16
Adjusted R-squared:  0.8688
p-valor G1 = 6.46e-08
p-valor G2 < 2.2e-16
"

best.P <- lm(G3 ~ G1+G2, data=train.P)