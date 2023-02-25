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
mat <- read.table("student-mat.csv",sep=",",header=TRUE)
por <- read.table("student-por.csv",sep=",",header=TRUE)

mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)
por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)

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


# Modelos a usar ----
full.M <- lm(G3 ~ ., data=train.M)
full.P <- lm(G3 ~ ., data=train.P)

model.M <- lm(G3 ~ G2, data=train.M)
model.P <- lm(G3 ~ G2, data=train.P)
# Validación de supuestos ----
## 1. Los errores tienen media 0 ----
"
Equivale a que la relación sea lineal.
Acá poner el p-valor de los modelos nuevamente.
"
# Mat
residuals.M <- model.M$residuals
mean(residuals.M)
plot(residuals.M, pch=20)
abline(h=mean(residuals.M), lwd=2, col="red")
# Por
residuals.P <- model.P$residuals
mean(residuals.P)
plot(residuals.P, pch=20)
abline(h=mean(residuals.P), lwd=2, col="red")
## 2. Supuesto de homocedasticidad (los errores tienen var constante) ----
"
Si no se verifica, los intervalos de confianza y los tests de
hipótesis que realicé no me sirven.
"
# Mat
leveragePlots(model.M, pch=20)
plot(model.M, 1, pch=20) # no hay patrones.

# Por
leveragePlots(model.P, pch=20)
plot(model.P, 1, pch=20) # no hay patrones.
## 3. Multicolinealidad ----
"
Durbin Watson test:
H0: No hay correlación entre los residuos.
H1: Hay correlación.

Entonces si p-valor < 0.05 rechazo, y hay autocorrelación.
"

# Mat
durbinWatsonTest(model.M) # p-valor = 0.42
# no existen evidencias suficientes para rechazar H0.
durbinWatsonTest(full.M)

vif(full.M)

# Por
durbinWatsonTest(model.P) # p-valor = 0.75
# no existen evidencias suficientes para rechazar H0.
durbinWatsonTest(full.P)

vif(full.P)
## 4. Normalidad de los errores ----
library(Hmisc)
# Mat
hist(residuals.M, freq=F, ylim=c(0, 0.65),
     main="Histograma residuos: Matemática") # Parece una distribución normal
dens <- density(residuals.M)
lines(dens, lwd=2, col="red")
curve(dnorm(x, mean=0, sd=1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
legend("topleft", legend=c("KDE", "Densidad normal"),
       col=c("red", "blue"), lty=1:1, cex=0.65, box.lty=0)

describe(residuals.M)
plot(model.M, 2, pch=20) # No es normal
plot(model.M$fitted.values, rstudent(model.M), pch=20, xlab="valor ajustado", 
     ylab="residuo estudientizado")
abline(h=0, lwd=2, col="red")

shapiro.test(residuals.M) # p-valor < 2.2e-16
# Por
hist(residuals.P, freq=F, ylim=c(0, 0.8)) # Parece una distribución normal
dens <- density(residuals.P)
lines(dens, lwd=2, col="red")
curve(dnorm(x, mean=0, sd=1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
legend("topleft", legend=c("KDE", "Densidad normal"),
       col=c("red", "blue"), lty=1:1, cex=0.55, box.lty=0)

describe(residuals.M)

plot(model.P, 2, pch=20) # No es normal
plot(model.P$fitted.values, rstudent(model.P), pch=20, xlab="valor ajustado", 
     ylab="residuo estudientizado")
abline(h=0, lwd=2, col="red")

shapiro.test(residuals.P) # p-valor < 2.2e-16

# Identificación de outliers y transformaciones ----
"
Comprobando cada outlier método de residuos studentizados y
con distancia de Cook, se encuentra que remover uno por uno trae
nuevos outliers cuando se performa el modelo completo.
Esto es porque o se tiene más ausencias, o se tiene distintas notas.
La mayor parte de los outliers identificados con aquellos con nota final == 0.
Esta nota no se corresponde con una posible en Portugal, entonces quitamos
este grupo y luego tratamos de entender por separado por qué pasa esto.

Para el resto de los outliers, los dejaremos en el set de datos, intentando
que, luego con los mejores modelos encontrados y distintos tipos de
sets de datos, estos dejen de ser outliers y nuestros errores tengan 
distribución normal.
"
## Removiendo G3 == 0 ----
aux.mat <- mat
aux.por <- por

### A. Mat: ----
train.M <- train.M[train.M$G3 > 0, ]
test.M <- test.M[test.M$G3 > 0, ]
model.M <- lm(G3 ~ G2, data=train.M)
full.M <- lm(G3 ~ ., data=train.M)
shapiro.test(residuals(model.M)) # p-value = 1.359e-12
shapiro.test(residuals(full.M)) # p-value = 0.2726

mat <- mat[mat$G3 > 0, ]
transf <- BoxCoxTrans(mat$G3)
new_G3 <- predict(transf, mat$G3)
mat$G3 <- new_G3

aux_train <- sample(1:nrow(mat), nrow(mat)*0.8)
aux_test <- (-aux_train)
train.M <- mat[aux_train, ]
test.M <- mat[aux_test, ]

model.M <- lm(G3 ~ G2, data=train.M)
full.M <- lm(G3 ~ ., data=train.M)
shapiro.test(residuals(model.M)) # p-value = 4.972e-08
shapiro.test(residuals(full.M)) # p-value = 0.3165

plot(model.M, 2, pch=20) # se acerca mucho más
plot(full.M, 2, pch=20) # se acerca mucho más

write.csv(mat, "mat_transf.csv")
### B. Por: ----
train.P <- train.P[train.P$G3 > 0, ]
test.P <- test.P[test.P$G3 > 0, ]
model.P <- lm(G3 ~ G2, data=train.P)
full.P <- lm(G3 ~ ., data=train.P)
shapiro.test(residuals(model.P)) # p-value < 2.2e-16
shapiro.test(residuals(full.P)) # p-value < 2.2e-16

por <- por[por$G3 > 0, ]
transf <- BoxCoxTrans(por$G3)
new_G3 <- predict(transf, por$G3)
por$G3 <- new_G3

aux_train <- sample(1:nrow(por), nrow(por)*0.8)
aux_test <- (-aux_train)
train.P <- por[aux_train, ]
test.P <- por[aux_test, ]

model.P <- lm(G3 ~ G2, data=train.P)
full.P <- lm(G3 ~ ., data=train.P)
shapiro.test(residuals(model.P)) # p-value = 3.519e-16
shapiro.test(residuals(full.P)) # p-value = 9.245e-10

plot(model.P, 2, pch=20) # se acerca mucho más
plot(full.P, 2, pch=20) # se acerca mucho más

write.csv(por, "por_transf.csv")
