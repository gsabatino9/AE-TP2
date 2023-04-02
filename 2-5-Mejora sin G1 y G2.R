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

mat <- mat[,-c(31,32)]
por <- por[,-c(31,32)]

mat$absences <- sqrt(mat$absences)
por$absences <- sqrt(por$absences)
por$age <- por$age < 18.5
mat$FMedu <- (mat$Fedu + mat$Medu) < 6.5
mat <- mat[, !(names(mat) %in% c("Medu", "Fedu"))]
por$FMedu <- (por$Fedu + por$Medu) < 5.5
por <- por[, !(names(por) %in% c("Medu", "Fedu"))]
mat$Fjob <- mat$Fjob == "teacher"
por$Mjob <- por$Mjob == "teacher"
por$Fjob <- por$Fjob == "teacher"
mat$studytime <- mat$studytime < 2.5
por$goout <- por$goout < 4.5
mat$Dalc <- mat$Dalc < 1.5
mat$Walc <- mat$Walc < 2.5
por$Dalc <- por$Dalc < 1.5
por$health <- por$health < 4.5
por$freetime <- por$freetime < 3.5

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
  return(
    c(mean((y.hat - a_testear$G3)^2), summary(modelo)$r.squared,
      sum(summary(modelo)$coefficients[,4] < 0.01)-1
    )
  )
}

ecm_pred <- function(predicciones, a_testear) {
  return(mean((predicciones - a_testear$G3)^2))
}

cmp.M <- matrix(ncol=3)
cmp.P <- matrix(ncol=3)
## 1. Modelo nulo 1 (promedio) ----
y_h.M <- mean(train.M$G3)
y_h.P <- mean(train.P$G3)

null1.M <- ecm_pred(y_h.M, test.M)
null1.P <- ecm_pred(y_h.P, test.P)
cmp.M[1,] <- c(null1.M, 0, 0)
cmp.P[1,] <- c(null1.P, 0, 0)


## 2. Modelo con todos los predictores ----
# Mat
m2.M <- lm(G3 ~ ., data=train.M)
summary(m2.M)

cmp.M <- rbind(cmp.M, ecm(m2.M, test.M))

# Por
m2.P <- lm(G3 ~ ., data=train.P)
summary(m2.P)

cmp.P <- rbind(cmp.P, ecm(m2.P, test.P))

## 3. Backward Selection con todos los predictores ----
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
m3.M <- lm(G3 ~ Mjob+Fjob+studytime+failures+schoolsup+
             paid+goout+health+absences, data=train.M)
summary(m3.M)

cmp.M <- rbind(cmp.M, ecm(m3.M, test.M))

# Por
plot(find_coefs_bwd(train.P))
m3.P <- lm(G3 ~ school+sex+famsize+Pstatus+Mjob+Fjob+reason+
             guardian+traveltime+studytime+failures+schoolsup+
             famsup+activities+nursery+higher+internet+romantic+
             famrel+freetime+goout+Dalc+health+absences+FMedu, data=train.P)
summary(m3.P)

cmp.P <- rbind(cmp.P, ecm(m3.P, test.P))

## 4. Mixed Selection usando el criterio AIC ----
# Mat
set.seed(9)
m4.M <- stepAIC(m2.M, direction="both", trace=FALSE)
summary(m4.M)

cmp.M <- rbind(cmp.M, ecm(m4.M, test.M))

# Por
set.seed(9)
m4.P <- stepAIC(m2.P, direction="both", trace=FALSE)
summary(m4.P)

cmp.P <- rbind(cmp.P, ecm(m4.P, test.P))

## 5. Lasso con todos los predictores ----
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

m5.M <- lm(G3 ~ school+sex+age+address+famsize+Mjob+Fjob+reason+
             guardian+studytime+failures+schoolsup+famsup+paid+
             activities+nursery+internet+romantic+goout+Walc+health+
             absences+FMedu, data=train.M)
summary(m5.M)

cmp.M <- rbind(cmp.M, ecm(m5.M, test.M))

# Por
model_lasso(train.P, test.P)

m5.P <- lm(G3 ~ school+sex+age+Pstatus+FMedu+Mjob+Fjob+reason+
             guardian+studytime+failures+schoolsup+
             activities+nursery+higher+internet+romantic+
             famrel+freetime+goout+Dalc+health+absences, data=train.P)
summary(m5.P)

cmp.P <- rbind(cmp.P, ecm(m5.P, test.P))

# Gráficos ----
obtain_sign <- function(modelo) {
  a <- summary(modelo)$coefficients
  b <- a[a[,4] < 0.01, ]
  return(row.names(b)[-1])
}
concat <- function(m1, m2) {
  return(c(obtain_sign(m1), obtain_sign(m2)))
}
plot_models <- function(m1, m2) {
  plot_summs(m1, m2, model.names=c("Mat", "Por"),
             coefs=c(concat(m1, m2)))
}

plot_models(m2.M, m2.P)
plot_models(m3.M, m3.P)
plot_models(m4.M, m4.P)
plot_models(m5.M, m5.P)
