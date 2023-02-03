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

# Análisis de las variables ----
# Entrenamiento y comparación de modelos ----
"
En las siguientes secciones entrenaremos diversos modelos
para evaluar cuál de ellos nos dan:
- Un menor Error Cuadrático Medio (ECM): Esto estará determinado
por la varianza que presente cada modelo, y por su sesgo.
- La interpretabilidad del modelo: Un modelo puede servirnos para
confirmar la relación entre variables o para entender su importancia,
más allá de tener o no un ECM que compita con otros modelos.

Cabe destacar que utilizaremos regresión en lugar de clasificación porque,
si bien se puede ver a la nota final (de 1 a 20) como 20 grupos, estos grupos
están ordenados (nota 1 < nota 2 < ... < nota 20) y la distancia entre las
notas se mantienen, por lo tanto utilizaremos regresión y, como criterio
de error de nuestro modelo utilizaremos el Error Cuadrático Medio (ECM).

Los modelos que utilizaremos serán:
1. Regresión lineal.
2. Árboles de decisión.
3. Ensambles de árboles.
"

## Regresión lineal ----
"
En la siguiente sección entrenamos diversos modelos de regresión lineal:
1. Un modelo nulo sobre el cual comparar todos los demás, es decir, uno
donde Beta_1 = ... = Beta_p = 0. Que se corresponde con la media de las Y.
2. Un modelo con regresión lineal (todas los predictores).
3. Realizamos selección de modelos usando FS, BS y Mixed-Selection.
4. Regularización: Ridge y Lasso.
"
### Modelo nulo ----
y.hat <- mean(train$G3)
calcular_ecm <- function(y.hat) {
  return(mean((y.hat - test$G3)^2))
}
ECM_modelo_nulo <- calcular_ecm(y.hat) # 23.15902

comparaciones <- data.frame(Modelo="Nulo", "ECM test"=ECM_modelo_nulo)
agregar_modelo <- function(nombre_modelo, resultado) {
    return(
      rbind(comparaciones, data.frame(Modelo=nombre_modelo, "ECM test"=resultado))
    )
}

### Regresión lineal con todos los predictores ----
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

## Análisis del modelo lineal:
### Análisis de supuestos
vif(lm.fit) # no presenta colinealidad.

### Regresión lineal sin G1 ni G2 ----
"
En este caso, puesto que ninguna variable dio significativa,
queremos comprobar si solo G1 y G2 son las que obtienen los buenos resultados.
"
lm.fit2 <- update(lm.fit, ~ . - G1 - G2)
summary(lm.fit2) # el test es significativo, por lo tanto
# hay otras variables significativas en este modelo que solo G1 y G2.
calcular_ecm(predict(lm.fit2, test)) # da gigante: 21.08093

### Selección de modelos ----
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
step.model <- stepAIC(lm.fit, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test)
ECM_mix_aic <- calcular_ecm(y.hat)
comparaciones <- agregar_modelo("MIXED selection AIC", ECM_mix_aic)

### RIDGE ----
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

### LASSO ----
lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(9)
cv.out <- cv.glmnet(x.train, y.train, alpha=1)
plot(cv.out)

best_lambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=x.test)
ECM_lasso <-calcular_ecm(lasso.pred)
comparaciones <- agregar_modelo("Lasso", ECM_lasso)

### Regresión lineal agregando términos polinómicos al mejor modelo ----
# El mejor modelo es con G1+G2+course_type solamente
lm.fit3 <- lm(G3 ~ G1+G2+course_type, data=train)
y.hat.lm3 <- predict(lm.fit3, test)
calcular_ecm(y.hat.lm3) # 2.169996

lm.fit4 <- lm(G3 ~ G1*G2 + G1*course_type + G2*course_type, data=train)
y.hat.lm4 <- predict(lm.fit4, test)
calcular_ecm(y.hat.lm4) # 2.154735

anova(lm.fit3, lm.fit4) # se queda con este

lm.fit5 <- lm(G3 ~ poly(G1, 3) + poly(G2, 3) + course_type, data=train)
y.hat.lm5 <- predict(lm.fit5, test)
calcular_ecm(y.hat.lm5) # 2.17548
## Árboles ----
"
Los modelos de árboles que utilizaremos serán:
1. Árbol de decisión (lo dejaremos crecer).
2. Árbol de decisión podado.
3. Bagging.
4. Random Forest.
5. Boosting.
6. BART.
"
### Árboles de decisión ----
#### Árbol crecido ----
longtree.fit <- tree(G3 ~ ., train,
                        control=tree.control(nobs = nrow(train), mindev = 0))
summary(longtree.fit)

y.hat.tree <- predict(longtree.fit, test)
ECM_arbol_total <- calcular_ecm(y.hat.tree)
comparaciones <- agregar_modelo("Árbol crecido", ECM_arbol_total)

#### Árbol podado ----
prunning.fit <- prune.tree(longtree.fit, best=5)
plot(prunning.fit)
text(prunning.fit, pretty=0)

y.hat.prunning <- predict(prunning.fit, test)
ECM_prunning <- calcular_ecm(y.hat.prunning)
comparaciones <- agregar_modelo("Árbol podado", ECM_prunning)

### Ensambles ----
#### Bagging ----
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
#### Random Forest ----
set.seed(9)
rf.fit <- randomForest(G3 ~ ., data=train, mtry=p/3, importance=TRUE)

y.hat.rf <- predict(rf.fit, test)
ECM_rf <- calcular_ecm(y.hat.rf)
comparaciones <- agregar_modelo("Random Forest", ECM_rf)

#### Boosting ----
boost.fit <- gbm(G3 ~ ., data=train,
                 distribution="gaussian", n.trees=5000)
summary(boost.fit)

y.hat.boost <- predict(boost.fit, test)
ECM_boost <- calcular_ecm(y.hat.boost)
comparaciones <- agregar_modelo("Boosting", ECM_boost)

#### BART ----
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
                     