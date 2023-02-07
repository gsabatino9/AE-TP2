# MATEMÁTICA ----
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
mat <- read.table("student-mat.csv",sep=",",header=TRUE)

mat[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(mat[-c(3,13,14,15,30,31,32,33)], factor)

set.seed(9)
aux_train <- sample(1:nrow(mat), nrow(mat)*0.7)
aux_test <- (-aux_train)

train <- mat[aux_train, ]
test <- mat[aux_test, ]


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
ECM_modelo_nulo <- calcular_ecm(y.hat) # 27.86631
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
coef_fwd <- which.min(mean.cv.errors.fwd)
coef(regfit.fwd, coef_fwd)

#fwd.fit <- lm(G3 ~ nursery+famrel+absences+G1+G2, data=train)
fwd.fit <- lm(G3 ~ G1+G2+age+Fedu+nursery+famrel+absences+J2, data=train)
y.hat <- predict(fwd.fit, test)
comparaciones <- agregar_modelo("FWD selection", 
                                y.hat, "nursery+famrel+absences+G1+G2")

#### Backward-Selection:
regfit.bwd <- regsubsets(G3 ~ ., data=train, nvmax=p, method="backward")
summary(regfit.bwd)

# Me quedo con el mejor a partir de cv con K=10:
set.seed(9)
folds <- sample(rep(1:k, length=n))

cv.errors <- matrix(NA, k, p, 
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(G3 ~ ., data=train[folds != j, ], 
                         nvmax=p, method="backward")
  for (i in 1:p) {
    pred <- predict(best.fit, train[folds == j, ], id=i)
    cv.errors[j, i] <- mean((train$G3[folds == j] - pred)^2)
  }
}

mean.cv.errors.bwd <- apply(cv.errors, 2, mean)
coef_bwd <- which.min(mean.cv.errors.bwd)
coef(regfit.bwd, coef_bwd) # mismo que FWD.

""
bwd.fit <- lm(G3 ~ G2+nursery+famrel+absences+J2, data=train)
y.hat <- predict(bwd.fit, test)
comparaciones <- agregar_modelo("BWD selection", 
                                y.hat, "nursery+famrel+absences+G1+G2")

#### Mixed-Selection:
regfit.mix <- regsubsets(G3 ~ ., data=train, nvmax=p, method="seqrep")
summary(regfit.mix)

# Me quedo con el mejor a partir de cv con K=10:
set.seed(9)
folds <- sample(rep(1:k, length=n))

cv.errors <- matrix(NA, k, p, 
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(G3 ~ ., data=train[folds != j, ], 
                         nvmax=p, method="seqrep")
  for (i in 1:p) {
    pred <- predict(best.fit, train[folds == j, ], id=i)
    cv.errors[j, i] <- mean((train$G3[folds == j] - pred)^2)
  }
}

mean.cv.errors.mix <- apply(cv.errors, 2, mean)
coef_mix <- which.min(mean.cv.errors.mix) # 2
coef(regfit.mix, coef_mix) # mismas que FWD.

### Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(lm.fit, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test)
comparaciones <- agregar_modelo("MIXED selection AIC", y.hat,
"school+age+nursery+internet+famrel+Walc+absences+G1+G2")

## Analizo con modelo lineal cada una por separado ----
# 1. School:
school <- lm(G3 ~ school, data=train)
summary(school) # 0.6871

# 2. Sex:
sex <- lm(G3 ~ sex, data=train)
summary(sex) # 0.0905

# 3. address:
address <- lm(G3 ~ address, data=train)
summary(address) # 0.1405

# 4. famsize:
famsize <- lm(G3 ~ famsize, data=train)
summary(famsize) # 0.1385

# 5. Pstatus:
Pstatus <- lm(G3 ~ Pstatus, data=train)
summary(Pstatus) # 0.2711

# 6. Medu:
Medu <- lm(G3 ~ Medu, data=train)
summary(Medu) # 0.09354

# 7. Fedu:
Fedu <- lm(G3 ~ Fedu, data=train)
summary(Fedu) # 0.06633

# 8. Mjob:
Mjob <- lm(G3 ~ Mjob, data=train)
summary(Mjob) # 0.1387

# 9. Fjob:
Fjob <- lm(G3 ~ Fjob, data=train)
summary(Fjob) # 0.609

# 10. reason:
reason <- lm(G3 ~ reason, data=train)
summary(reason) # 0.3631

# 11. guardian:
guardian <- lm(G3 ~ guardian, data=train)
summary(guardian) # 0.7339

# 12. traveltime:
traveltime <- lm(G3 ~ traveltime, data=train)
summary(traveltime) # .114

# 13. freetime:
freetime <- lm(G3 ~ freetime, data=train)
summary(freetime) # 0.5375

# 14. studytime:
studytime <- lm(G3 ~ studytime, data=train)
summary(studytime) # 0.05399

# 15. schoolsup:
schoolsup <- lm(G3 ~ schoolsup, data=train)
summary(schoolsup) # 0.2681

# 16. famsup:
famsup <- lm(G3 ~ famsup, data=train)
summary(famsup) # 0.1008

# 17. paid:
paid <- lm(G3 ~ paid, data=train)
summary(paid) # 0.514

# 18. activities:
activities <- lm(G3 ~ activities, data=train)
summary(activities) # 0.5993

# 19. nursery:
nursery <- lm(G3 ~ nursery, data=train)
summary(nursery) # .9032

# 20. higher:
higher <- lm(G3 ~ higher, data=train)
summary(higher) # 0.001941

# 21. internet:
internet <- lm(G3 ~ internet, data=train)
summary(internet) # 0.393

# 22. romantic:
romantic <- lm(G3 ~ romantic, data=train)
summary(romantic) # 0.03527

# 23. famrel:
famrel <- lm(G3 ~ famrel, data=train)
summary(famrel) # 0.2358

# 24. goout:
goout <- lm(G3 ~ goout, data=train)
summary(goout) # 0.008046

# 25. Dalc:
Dalc <- lm(G3 ~ Dalc, data=train)
summary(Dalc) # 0.2579

# 26. Walc:
Walc <- lm(G3 ~ Walc, data=train)
summary(Walc) # 0.2472

# 27. health:
health <- lm(G3 ~ health, data=train)
summary(health) # 0.6102

# 28. G1:
G1 <- lm(G3 ~ G1, data=train)
summary(G1) # 2.2e-16

# 29. age:
age <- lm(G3 ~ age, data=train)
summary(age) # 0.04311

# 30 failures:
failures <- lm(G3 ~ failures, data=train)
summary(failures) # 1.596e-09

# 31. absences:
absences <- lm(G3 ~ absences, data=train)
summary(absences) # 0.4432

## Saco los que tienen p-valor > 0.05 (aprox) ----
sacar <- c("school", "sex", "address", "famsize", "Pstatus", 
           "Medu", "Mjob", "Fjob", "reason", "guardian", 
           "traveltime", "freetime", "schoolsup", "famsup", 
           "paid", "activities", "nursery", "internet", 
           "famrel", "Dalc", "Walc", "health", "absences")

train2 <- train[, !(names(train) %in% sacar)]
test2 <- test[, !(names(test) %in% sacar)]

nuevo_modelo <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo)
comparaciones <- agregar_modelo(
  "Quitando VAs rl simple", predict(nuevo_modelo, test), 
  "age, Fedu, studytime, failures, higher, romantic, goout, G1, G2")


## Genero un árbol de decisión por c/u de las variables quitadas ----
# Medu
aux <- tree(G3 ~ Medu, data=train)
plot(aux)
text(aux, pretty=0) # 1,2,3

train2$Medu <- train$Medu %in% c(1,2,3)
test2$Medu <- test$Medu %in% c(1,2,3)

# Mjob
aux <- tree(G3 ~ Mjob, data=train)
plot(aux)
text(aux, pretty=0)

train2$Mjob <- train$Mjob %in% c("at_home", "other")
test2$Mjob <- test$Mjob %in% c("at_home", "other")

# reason
aux <- tree(G3 ~ reason, data=train)
plot(aux)
text(aux, pretty=0)

train2$reason <- train$reason %in% c("course", "home")
test2$reason <- test$reason %in% c("course", "home")

# freetime
aux <- tree(G3 ~ freetime, data=train)
plot(aux)
text(aux, pretty=0)

train2$freetime <- train$freetime < 2.5
test2$freetime <- test$freetime < 2.5

# Dalc
aux <- tree(G3 ~ Dalc, data=train)
plot(aux)
text(aux, pretty=0)

train2$Dalc <- train$Dalc < 1.5
test2$Dalc <- test$Dalc < 1.5

# health
aux <- tree(G3 ~ health, data=train)
plot(aux)
text(aux, pretty=0)

train2$health <- train$health < 1.5
test2$health <- test$health < 1.5

# absences
aux <- tree(G3 ~ absences, data=train)
plot(aux)
text(aux, pretty=0)

train2$absences <- train$absences < 0.5
test2$absences <- test$absences < 0.5

## Fitteo con las columnas nuevas ----
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
p <- ncol(train2)-1

comparaciones <- agregar_modelo(
  "Cols generadas de árboles", predict(nuevo_modelo2, test2), "...")
# Mejora muchísimo.
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

#fwd.fit <- lm(G3 ~ G1+G2+absences, data=train2)
fwd.fit <- lm(G3 ~ age+Fedu+studytime+failures+G1+G2+
                K1+Dalc+absences+J1+J2, data=train2)
y.hat <- predict(fwd.fit, test2)
comparaciones <- agregar_modelo("FWD selection 2", 
                                y.hat, "G1+G2+absences")

### Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(nuevo_modelo2, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test2)
comparaciones <- agregar_modelo("MIXED selection AIC", y.hat,
"age, studytime, G1, G2, Dalc, absences")

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
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=x.test)
comparaciones <- agregar_modelo("Lasso", lasso.pred, "...")
