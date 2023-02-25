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
ECM_modelo_nulo <- calcular_ecm(y.hat) # 14.92127

comparaciones <- data.frame(Modelo="Nulo", "ECM test"=ECM_modelo_nulo)
agregar_modelo <- function(nombre_modelo, resultado) {
  return(
    rbind(comparaciones, data.frame(Modelo=nombre_modelo, "ECM test"=resultado))
  )
}

## Regresión con todos los predictores ----
lm.fit <- lm(G3 ~ ., data=train)
summary(lm.fit)
"
Cosas importantes a destacar:
- p-valor de la regresión: 0.01 (prácticamente 0). Regresión es significativa,
entonces tiene sentido plantear un modelo lineal con estos predictores.
- R^2 ajustado: 0.8506 -> Explica muy bien la varianza del modelo.
- No hay prácticamente predictores significativos por su cuenta, 
a excepción de G1 y G2. Pero, puede que alguno de ellos se deba a un error
(aunque es muy difícil puesto que el p-valor de la regresión es casi 0).
"

y.hat <- predict(lm.fit, test)
ECM_reg_lineal1 <- calcular_ecm(y.hat)
comparaciones <- agregar_modelo("Reg. lineal 1", ECM_reg_lineal1)
# 2.383666
## Analizo con modelo lineal cada una por separado ----
# 1. School:
school <- lm(G3 ~ school, data=train)
summary(school) # 0.001018

# 2. Sex:
sex <- lm(G3 ~ sex, data=train)
summary(sex) # 0.9822

# 3. address:
address <- lm(G3 ~ address, data=train)
summary(address) # 0.0002915

# 4. famsize:
famsize <- lm(G3 ~ famsize, data=train)
summary(famsize) # 0.2398

# 5. Pstatus:
Pstatus <- lm(G3 ~ Pstatus, data=train)
summary(Pstatus) # 0.3585

# 6. Medu:
Medu <- lm(G3 ~ Medu, data=train)
summary(Medu) # 1.4e-07

# 7. Fedu:
Fedu <- lm(G3 ~ Fedu, data=train)
summary(Fedu) # 9.549e-05

# 8. Mjob:
Mjob <- lm(G3 ~ Mjob, data=train)
summary(Mjob) # 0.0001244

# 9. Fjob:
Fjob <- lm(G3 ~ Fjob, data=train)
summary(Fjob) # 0.02104

# 10. reason:
reason <- lm(G3 ~ reason, data=train)
summary(reason) # 0.01054

# 11. guardian:
guardian <- lm(G3 ~ guardian, data=train)
summary(guardian) # 0.01098

# 12. traveltime:
traveltime <- lm(G3 ~ traveltime, data=train)
summary(traveltime) # 0.007925

# 13. freetime:
freetime <- lm(G3 ~ freetime, data=train)
summary(freetime) # 0.06549

# 14. studytime:
studytime <- lm(G3 ~ studytime, data=train)
summary(studytime) # 0.00011

# 15. schoolsup:
schoolsup <- lm(G3 ~ schoolsup, data=train)
summary(schoolsup) # 0.01827

# 16. famsup:
famsup <- lm(G3 ~ famsup, data=train)
summary(famsup) # 0.5534

# 17. paid:
paid <- lm(G3 ~ paid, data=train)
summary(paid) # 0.1035

# 18. activities:
activities <- lm(G3 ~ activities, data=train)
summary(activities) # 0.7379

# 19. nursery:
nursery <- lm(G3 ~ nursery, data=train)
summary(nursery) # 0.1673

# 20. higher:
higher <- lm(G3 ~ higher, data=train)
summary(higher) # 1.191e-07

# 21. internet:
internet <- lm(G3 ~ internet, data=train)
summary(internet) # 0.002842

# 22. romantic:
romantic <- lm(G3 ~ romantic, data=train)
summary(romantic) # 0.01542

# 23. famrel:
famrel <- lm(G3 ~ famrel, data=train)
summary(famrel) # 0.6447

# 24. goout:
goout <- lm(G3 ~ goout, data=train)
summary(goout) # 0.01057

# 25. Dalc:
Dalc <- lm(G3 ~ Dalc, data=train)
summary(Dalc) # 0.0007654

# 26. Walc:
Walc <- lm(G3 ~ Walc, data=train)
summary(Walc) # 0.00205

# 27. health:
health <- lm(G3 ~ health, data=train)
summary(health) # 0.01414

## Empiezo quitando las que dan alto ----
"
Se puede generar una columna analizando a mano los features,
o haciendo un PCA solo en esas columnas.
"
train <- train[, !(names(train) %in% c("sex", "famsize",
                                       "Pstatus", "freetime",
                                       "famsup", "paid", "activities",
                                       "nursery","famrel"))]
test <- test[, !(names(test) %in% c("sex", "famsize",
                                    "Pstatus", "freetime",
                                    "famsup", "paid", "activities",
                                    "nursery","famrel"))]

new_model <- lm(G3 ~ ., data=train)

ECM_nuevo <- calcular_ecm(predict(new_model, test)) # 2.319875 -> ya lo mejora.
comparaciones <- agregar_modelo("Sacando columnas no sig", ECM_nuevo)

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
mean.cv.errors.fwd

par(mfrow = c(1, 1))
plot(mean.cv.errors.fwd, type = "b", pch=20)
coef_fwd <- which.min(mean.cv.errors.fwd) # 2
coef(regfit.fwd, coef_fwd) # se queda solo con G1 y G2.

fwd.fit <- lm(G3 ~ G1 + G2, data=train)
y.hat <- predict(fwd.fit, test)
ECM_rl_fwd <- calcular_ecm(y.hat)
comparaciones <- agregar_modelo("FWD selection", ECM_rl_fwd)

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
mean.cv.errors.bwd

par(mfrow = c(1, 1))
plot(mean.cv.errors.bwd, type = "b", pch=20)
coef_bwd <- which.min(mean.cv.errors.bwd) # 2
coef(regfit.bwd, coef_bwd) # se queda solo con G1 y G2.

ECM_rl_bwd <- ECM_rl_fwd
comparaciones <- agregar_modelo("BWD selection", ECM_rl_bwd)

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

par(mfrow = c(1, 1))
plot(mean.cv.errors.mix, type = "b", pch=20)
coef_mix <- which.min(mean.cv.errors.mix) # 2
coef(regfit.mix, coef_mix) # se queda solo con G1 y G2.

ECM_rl_mix <- ECM_rl_fwd
comparaciones <- agregar_modelo("MIXED selection", ECM_rl_mix)

### Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(new_model, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test)
ECM_mix_aic <- calcular_ecm(y.hat)
comparaciones <- agregar_modelo("MIXED selection AIC", ECM_mix_aic)

## RIDGE ----
x.train <- model.matrix(G3 ~ ., train)[, -1]
y.train <- train$G3
x.test <- model.matrix(G3 ~ ., test)[, -1]
y.test <- test$G3

grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x.train, y.train, alpha=0, lambda=grid, thresh=1e-12)

set.seed(9)
cv.out <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.out)
best_lambda <- cv.out$lambda.min

ridge.pred <- predict(ridge.mod, s=best_lambda, newx=x.test)
ECM_ridge <- calcular_ecm(ridge.pred)
comparaciones <- agregar_modelo("Ridge", ECM_ridge)

## LASSO ----
lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(9)
cv.out <- cv.glmnet(x.train, y.train, alpha=1)
plot(cv.out)

best_lambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=x.test)
ECM_lasso <-calcular_ecm(lasso.pred)
comparaciones <- agregar_modelo("Lasso", ECM_lasso)

## Árbol de decisión sin G1 y G2 ----
longtree.fit <- tree(G3 ~ .-G1-G2, train,
                     control=tree.control(nobs = nrow(train), mindev = 0))
summary(longtree.fit)
## Árbol podado ----
prunning.fit <- prune.tree(longtree.fit, best=5)
plot(prunning.fit)
text(prunning.fit, pretty=0)

## Uso esa info para generar nuevas columnas ----
train$failures_tree <- train$failures == 0
train$absences_tree <- train$absences == 0
train$health_tree <- train$health < 5
train$age_tree <- train$age < 17

test$failures_tree <- test$failures == 0
test$absences_tree <- test$absences == 0
test$health_tree <- test$health < 5
test$age_tree <- test$age < 17

longtree.fit2 <- tree(G3 ~ .
                      -G1-G2, train,
                      control=tree.control(nobs = nrow(train), mindev = 0))
prunning.fit2 <- prune.tree(longtree.fit2, best=5)
plot(prunning.fit2)
text(prunning.fit2, pretty=0)

new_model2 <- lm(G3 ~ ., data=train)
summary(new_model2)
comparaciones <- 
  agregar_modelo("VAs agregadas con árboles",
    calcular_ecm(predict(new_model2, test))) # 2.213529 -> tremendo
# xq es con todas las variables.

## Qué pasa si ahora tiro las que no son buenas?
new_model3 <- lm(G3 ~ .
                 -age-address-Medu-Fedu-Mjob-Fjob
                 -reason-guardian-traveltime-studytime
                 -health
                 , data=train)
summary(new_model3)
comparaciones <- agregar_modelo("Quito VAs que no uso", 
                                calcular_ecm(predict(new_model3, test)))


## Fitteo un árbol para cada variable a ver cuál es su split más significativo ----
# 1. G1 y G2:
g1_g2_tree <- tree(G3 ~ G1+G2, data=train)
plot(g1_g2_tree)
text(g1_g2_tree, pretty=0) # G2 < 11.5

# 2. Medu:
medu_tree <- tree(G3 ~ Medu, data=train)
plot(medu_tree)
text(medu_tree, pretty=0) # 1,2,3

# 3. Fedu:
fedu_tree <- tree(G3 ~ Fedu, data=train)
plot(fedu_tree)
text(fedu_tree, pretty=0) # 0,1,2,3

# 4. Mjob:
Mjob_tree <- tree(G3 ~ Mjob, data=train)
plot(Mjob_tree)
text(Mjob_tree, pretty=0) # at_home, other

# 5. Fjob:
Fjob_tree <- tree(G3 ~ Fjob, data=train)
plot(Fjob_tree)
text(Fjob_tree, pretty=0) # at_home, other, services

# 6. reason:
reason_tree <- tree(G3 ~ reason, data=train)
plot(reason_tree)
text(reason_tree, pretty=0) # course, home, other

# 7. guardian:
guardian_tree <- tree(G3 ~ guardian, data=train)
plot(guardian_tree)
text(guardian_tree, pretty=0) # other

# 8. traveltime, studytime:
time_tree <- tree(G3 ~ traveltime + studytime, data=train)
plot(time_tree)
text(time_tree, pretty=0) # studytime < 2.5, traveltime < 1.5

# 9. goout:
goout_tree <- tree(G3 ~ goout, data=train)
plot(goout_tree)
text(goout_tree, pretty=0) # goout < 4.5

# 10. Dalc, Walc:
alc <- tree(G3 ~ Dalc+Walc+health, data=train)
plot(alc)
text(alc, pretty=0) # Dalc < 1.5

# 11. absences y G1:
rend <- tree(G3 ~ absences+G1, data=train)
plot(rend)
text(rend, pretty=0) # G1 < 10.5

train$G2_tree <- train$G2 < 11.5
train$Medu_tree <- train$Medu == 1 | train$Medu == 2 | train$Medu == 3
train$Fedu_tree <- train$Fedu == 1 | train$Fedu == 2 | train$Fedu == 3 | train$Fedu == 0
train$reason_tree <- train$reason == "other"
train$Mjob_tree <- train$Mjob %in% c("at_home", "other")
train$Fjob_tree <- train$Fjob %in% c("at_home", "other", "services")
train$studytime_tree <- train$studytime < 2.5
train$traveltime_tree <- train$traveltime < 1.5
train$goout_tree <- train$goout < 4.5
train$Dalc_tree <- train$Dalc < 1.5
train$G1_tree <- train$G1 < 10.5

test$G2_tree <- test$G2 < 11.5
test$Medu_tree <- test$Medu == 1 | test$Medu == 2 | test$Medu == 3
test$Fedu_tree <- test$Fedu == 1 | test$Fedu == 2 | test$Fedu == 3 | test$Fedu == 0
test$reason_tree <- test$reason == "other"
test$Mjob_tree <- test$Mjob %in% c("at_home", "other")
test$Fjob_tree <- test$Fjob %in% c("at_home", "other", "services")
test$studytime_tree <- test$studytime < 2.5
test$traveltime_tree <- test$traveltime < 1.5
test$goout_tree <- test$goout < 4.5
test$Dalc_tree <- test$Dalc < 1.5
test$G1_tree <- test$G1 < 10.5

to_drop <- c("age", "address","Medu", "Fedu", "Mjob",
             "Fjob", "reason", "guardian","traveltime", "studytime",
             "health", "failures", "schoolsup", "internet",
             "goout", "Dalc", "Walc", "absences")
train <- train[, !(names(train) %in% to_drop)]
test <- test[, !(names(test) %in% to_drop)]

## Fitteo de modelos ----
## Modelo Lineal
arbol_modelo <- lm(G3 ~ ., data=train)
summary(arbol_modelo)
comparaciones <- agregar_modelo("Agregando VAs fitteando árbol x c/u", 
                calcular_ecm(predict(arbol_modelo, test)) # 2.129542
                                )
## Foward-Selection
p <- ncol(train)-1

regfit.fwd <- regsubsets(G3 ~ ., data=train, nvmax=p, method="forward")
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
coef_fwd <- which.min(mean.cv.errors.fwd) # 2
coef(regfit.fwd, coef_fwd) # se queda solo con G1, G2, course_type, absences_tree

fwd.fit <- lm(G3 ~ G1 + G2 + course_type + absences_tree, data=train)
y.hat <- predict(fwd.fit, test)
ECM_rl_fwd <- calcular_ecm(y.hat)
comparaciones <- agregar_modelo("Foward-Selection sobre anterior", ECM_rl_fwd)

## Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(arbol_modelo, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test)
ECM_mix_aic <- calcular_ecm(y.hat) # 2.104369
comparaciones <- agregar_modelo("MIXED selection AIC sobre anterior", ECM_mix_aic)

## LASSO
x.train <- model.matrix(G3 ~ ., train)[, -1]
y.train <- train$G3
x.test <- model.matrix(G3 ~ ., test)[, -1]
y.test <- test$G3
lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)
set.seed(9)
cv.out <- cv.glmnet(x.train, y.train, alpha=1)

best_lambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=x.test)
ECM_lasso <-calcular_ecm(lasso.pred)
comparaciones <- agregar_modelo("Lasso sobre anterior", ECM_lasso)