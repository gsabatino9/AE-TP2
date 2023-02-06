# PORTUGUÉS ----
# Introducción ----
"
Pasos del modelo lineal:
1. Estimar Beta_i y sigma^2.
2. Hacer inferencia.
3. Validar supuestos.
4. Predecir.
5. Identificación de datos atípicos.
6. Selección de modelos óptimos.

Iterar.
"

"
Preguntas a responder después de obtener el mejor modelo:

1. Hay alguna relación entre Y y las VAs predictoras?
Para esto sirve el test de hipótesis con H0: B_1 = ... = B_p = 0.
2. Qué tan fuerte es esta relación? Estimación de sigma^2 (RSE).
3. Qué covariable está asociada a Y?
Lo veo con el p-valor del test conjunto.
4. Qué tan grande es la asociación de Y a cada covariable?
Me construyo los intervalos de confianza para los Beta_i
y, si incluye al 0, entonces esa variable no es significativa dadas
las otras.
Previo a esto debo verificar que no exista colinealidad.
Además, puedo fittear una regresión individual para cada
covariable que pienso que puede relacionarse con Y.
5. Con qué precisión puedo predecir una nueva observación (X_0, Y_0)?
Me construyo el intervalo de predicción para esa observación.
6. Es la relación lineal? Es decir, verificar el 1° supuesto del
modelo lineal. Esto lo hago viendo los residuos vs la estimación de Y
y concluyendo que no hay ningún patrón.
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

por[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(por[-c(3,13,14,15,30,31,32,33)], factor)

set.seed(9)
aux_train <- sample(1:nrow(por), nrow(por)*0.7)
aux_test <- (-aux_train)

train <- por[aux_train, ]
test <- por[aux_test, ]


n <- nrow(train)
p <- ncol(train)-1

## Puedo fittear modelos y sacar las variables significativas
## para cada modelo.
## Acá voy a empezar sacando:
## G1, G2, absences, failures, course_type.

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
"
En la siguiente sección entrenamos diversos modelos de regresión lineal:
1. Un modelo nulo sobre el cual comparar todos los demás, es decir, uno
donde Beta_1 = ... = Beta_p = 0. Que se corresponde con la media de las Y.
2. Un modelo con regresión lineal (todas los predictores).
3. Realizamos selección de modelos usando FS, BS y Mixed-Selection.
4. Regularización: Ridge y Lasso.
"
## Modelo nulo ----
y.hat <- mean(train$G3)
calcular_ecm <- function(y.hat) {
  return(mean((y.hat - test$G3)^2))
}
ECM_modelo_nulo <- calcular_ecm(y.hat) # 8.1731
comparaciones <- data.frame(Modelo="Nulo", "ECM test"=ECM_modelo_nulo, "VAs"="-")

agregar_modelo <- function(nombre, pred, variables) {
  ECM <- calcular_ecm(pred)
  return(
    rbind(comparaciones, 
          data.frame(Modelo=nombre, "ECM test"=ECM, "VAs"=variables))
  )
}

## Regresión con todos los predictores ----
lm.fit <- lm(G3 ~ ., data=train)
summary(lm.fit)
"
Cosas importantes a destacar:
- p-valor de la regresión: < 2.2e-16 (prácticamente 0). Regresión es significativa,
entonces tiene sentido plantear un modelo lineal con estos predictores.
- R^2 ajustado: 0.8426 -> Explica muy bien la varianza del modelo.
- No hay prácticamente predictores significativos por su cuenta, 
a excepción de G2. Pero, puede que alguno de ellos se deba a un error
(aunque es muy difícil puesto que el p-valor de la regresión es casi 0).
"

y.hat <- predict(lm.fit, test)
comparaciones <- agregar_modelo("Reg. lineal completo", y.hat, "Todas")
## Selección de modelos ----
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

#### Forward-Selection:
regfit.fwd <- regsubsets(G3 ~ ., data=train, nvmax=p, method="forward")
summary(regfit.fwd)

# Me quedo con el mejor a partir de cv con K=10:
k <- 10
set.seed(9)
folds <- sample(rep(1:k, length=n))

cv.errors <- matrix(NA, k, p, 
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(G3 ~ ., data=train[folds != j, ], 
                         nvmax=p, method="forward")
  for (i in 1:p) {
    pred <- predict(best.fit, train[folds == j, ], id=i)
    cv.errors[j, i] <- mean((train$G3[folds == j] - pred)^2)
  }
}

mean.cv.errors.fwd <- apply(cv.errors, 2, mean)
coef_fwd <- which.min(mean.cv.errors.fwd) # G2
coef(regfit.fwd, coef_fwd)

fwd.fit <- lm(G3 ~ G2, data=train)
y.hat <- predict(fwd.fit, test)
comparaciones <- agregar_modelo("FWD selection", 
                                y.hat, "G2")


### Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(lm.fit, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test)
comparaciones <- agregar_modelo("MIXED selection AIC", y.hat,
"sex, reason, guardian, failures, higher, health, absences, G1, G2")

## Analizo con modelo lineal cada una por separado ----
# 1. School:
school <- lm(G3 ~ school, data=train)
summary(school) # 4.823e-11

# 2. Sex:
sex <- lm(G3 ~ sex, data=train)
summary(sex) # 0.005932

# 3. address:
address <- lm(G3 ~ address, data=train)
summary(address) # 0.0004959

# 4. famsize:
famsize <- lm(G3 ~ famsize, data=train)
summary(famsize) # 0.1413

# 5. Pstatus:
Pstatus <- lm(G3 ~ Pstatus, data=train)
summary(Pstatus) # .7792

# 6. Medu:
Medu <- lm(G3 ~ Medu, data=train)
summary(Medu) # 1.148e-05

# 7. Fedu:
Fedu <- lm(G3 ~ Fedu, data=train)
summary(Fedu) # 0.001107

# 8. Mjob:
Mjob <- lm(G3 ~ Mjob, data=train)
summary(Mjob) # 0.0001402

# 9. Fjob:
Fjob <- lm(G3 ~ Fjob, data=train)
summary(Fjob) # 0.09779

# 10. reason:
reason <- lm(G3 ~ reason, data=train)
summary(reason) # 9.79e-07

# 11. guardian:
guardian <- lm(G3 ~ guardian, data=train)
summary(guardian) # 0.2486

# 12. traveltime:
traveltime <- lm(G3 ~ traveltime, data=train)
summary(traveltime) # 0.00673

# 13. freetime:
freetime <- lm(G3 ~ freetime, data=train)
summary(freetime) # 0.007128

# 14. studytime:
studytime <- lm(G3 ~ studytime, data=train)
summary(studytime) # 7.253e-07

# 15. schoolsup:
schoolsup <- lm(G3 ~ schoolsup, data=train)
summary(schoolsup) # 0.2492

# 16. famsup:
famsup <- lm(G3 ~ famsup, data=train)
summary(famsup) # 0.146

# 17. paid:
paid <- lm(G3 ~ paid, data=train)
summary(paid) # 0.2912

# 18. activities:
activities <- lm(G3 ~ activities, data=train)
summary(activities) # 0.07062

# 19. nursery:
nursery <- lm(G3 ~ nursery, data=train)
summary(nursery) # 0.9727

# 20. higher:
higher <- lm(G3 ~ higher, data=train)
summary(higher) # 2.296e-16

# 21. internet:
internet <- lm(G3 ~ internet, data=train)
summary(internet) # 0.002846

# 22. romantic:
romantic <- lm(G3 ~ romantic, data=train)
summary(romantic) # 0.02026

# 23. famrel:
famrel <- lm(G3 ~ famrel, data=train)
summary(famrel) # 0.1114

# 24. goout:
goout <- lm(G3 ~ goout, data=train)
summary(goout) # 0.1239

# 25. Dalc:
Dalc <- lm(G3 ~ Dalc, data=train)
summary(Dalc) # 4.949e-05

# 26. Walc:
Walc <- lm(G3 ~ Walc, data=train)
summary(Walc) # 0.0003205

# 27. health:
health <- lm(G3 ~ health, data=train)
summary(health) # 0.008802

# 28. G1:
G1 <- lm(G3 ~ G1, data=train)
summary(G1) # 2.2e-16

# 29. age:
age <- lm(G3 ~ age, data=train)
summary(age) # 0.003062

# 30 failures:
failures <- lm(G3 ~ failures, data=train)
summary(failures) # 2.2e-16

# 31. absences:
absences <- lm(G3 ~ absences, data=train)
summary(absences) # 0.3276

## Saco los que tienen p-valor > 0.05 (aprox) ----
sacar <- c("famsize", "Pstatus", "Fjob", "guardian",
           "schoolsup", "famsup", "paid", "activities",
           "nursery", "famrel", "goout", "absences")

train2 <- train[, !(names(train) %in% sacar)]
test2 <- test[, !(names(test) %in% sacar)]

nuevo_modelo <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo)
comparaciones <- agregar_modelo(
  "Quitando VAs rl simple", predict(nuevo_modelo, test), 
  "age, Fedu, studytime, failures, higher, romantic, goout, G1, G2")


## Genero un árbol de decisión por c/u de las variables quitadas ----
# Fjob
aux <- tree(G3 ~ Fjob, data=train)
plot(aux)
text(aux, pretty=0)

train2$Fjob <- train$Fjob %in% c("at_home", "other", "services")
test2$Fjob <- test$Fjob %in% c("at_home", "other", "services")

# famrel
aux <- tree(G3 ~ famrel, data=train)
plot(aux)
text(aux, pretty=0)

train2$famrel <- train$famrel < 3.5
test2$famrel <- test$famrel < 3.5

# goout
aux <- tree(G3 ~ goout, data=train)
plot(aux)
text(aux, pretty=0)

train2$goout <- train$famrel < 4.5
test2$goout <- test$famrel < 4.5

## Fitteo con las columnas nuevas ----
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
p <- ncol(train2)-1

comparaciones <- agregar_modelo(
  "Cols generadas de árboles", predict(nuevo_modelo2, test2), "...")
# No lo mejora.
## Selección de modelos ----
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

#### Forward-Selection:
regfit.fwd <- regsubsets(G3 ~ ., data=train2, nvmax=p, method="forward")
summary(regfit.fwd)

# Me quedo con el mejor a partir de cv con K=10:
k <- 10
set.seed(9)
folds <- sample(rep(1:k, length=n))

cv.errors <- matrix(NA, k, p, 
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(G3 ~ ., data=train2[folds != j, ], 
                         nvmax=p, method="forward")
  for (i in 1:p) {
    pred <- predict(best.fit, train2[folds == j, ], id=i)
    cv.errors[j, i] <- mean((train2$G3[folds == j] - pred)^2)
  }
}

mean.cv.errors.fwd <- apply(cv.errors, 2, mean)
coef_fwd <- which.min(mean.cv.errors.fwd)
coef(regfit.fwd, coef_fwd)

fwd.fit <- lm(G3 ~ school+sex+Mjob+reason+failures+health+G1+G2, 
              data=train2)
y.hat <- predict(fwd.fit, test2)
comparaciones <- agregar_modelo("FWD selection 2", y.hat, 
"school+sex+Mjob+reason+failures+health+G1+G2")

### Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(nuevo_modelo2, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test2)
comparaciones <- agregar_modelo("MIXED selection AIC", y.hat,
"school, sex, G1, G2, reason, failures, health")

## LASSO ----
x.train <- model.matrix(G3 ~ ., train2)[, -1]
y.train <- train2$G3
x.test <- model.matrix(G3 ~ ., test2)[, -1]
y.test <- test2$G3
grid <- 10^seq(10, -2, length=100)

lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)

set.seed(9)
cv.out <- cv.glmnet(x.train, y.train, alpha=1)

best_lambda <- cv.out$lambda.min
coef(lasso.mod, s=best_lambda)
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=x.test)
comparaciones <- agregar_modelo("Lasso", lasso.pred, 
"school+sex+Mjobother+reasonother+failures+higher+freetime+Dalc+health+G1+G2")
