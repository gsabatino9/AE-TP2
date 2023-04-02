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
  library(Hmisc)
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

aux.mat <- mat
aux.por <- por

# Split train-test ----
set.seed(9)
i_train.M <- sample(1:nrow(mat), nrow(mat)*0.8)
i_test.M <- (-i_train.M)
train.M <- mat[i_train.M, ]
test.M <- mat[i_test.M, ]

set.seed(9)
i_train.P <- sample(1:nrow(por), nrow(por)*0.8)
i_test.P <- (-i_train.P)
train.P <- por[i_train.P, ]
test.P <- por[i_test.P, ]

# Mejores modelos sin G1 y G2 ----
# ecm = 1.813582
best.M <- lm(G3 ~ school+sex+age+address+Medu+Fedu+Mjob+
               Fjob+guardian+studytime+failures+schoolsup+famsup+
               paid+activities+internet+romantic+goout+Dalc+Walc+
               health+absences, data=train.M)
# ecm = 6.745228
best.P <- lm(G3 ~ school+sex+age+Medu+Fedu+Mjob+Fjob+reason+
               guardian+traveltime+studytime+failures+schoolsup+
               famsup+paid+activities+nursery+higher+internet+romantic+
               famrel+freetime+goout+Dalc+health+absences, data=train.P)

best_ecms <- c(1.813582, 6.745228)

ecm <- function(modelo, a_testear) {
  y.hat <- predict(modelo, a_testear)
  return(mean((y.hat - a_testear$G3)^2))
}

plot.M <- function(col) {
  aux <- tree(G3 ~ col, data=train.M)
  plot(aux)
  text(aux, pretty=0)
}

plot.P <- function(col) {
  aux <- tree(G3 ~ col, data=train.P)
  plot(aux)
  text(aux, pretty=0)
}
## Ausencias ----
# Mat
hist(x = mat$absences, breaks = seq(min(mat$absences), max(mat$absences), 1), 
     xlim = c(min(mat$absences), max(mat$absences)), prob=T, xaxt = "n",
     main="Histograma absences: Matemática")
axis(1, at = seq(min(mat$absences), max(mat$absences), 1), 
     labels = seq(min(mat$absences), max(mat$absences), 1))
curve(dnorm(x, mean = mean(mat$absences), sd = sd(mat$absences)), col="red", add = T)
describe(mat$absences)

# Por
hist(x = por$absences, breaks = seq(min(por$absences), max(por$absences), 1), 
     xlim = c(min(por$absences), max(por$absences)), prob=T, xaxt = "n",
     main="Histograma absences: Portugués")
axis(1, at = seq(min(por$absences), max(por$absences), 1), 
     labels = seq(min(por$absences), max(por$absences), 1))
curve(dnorm(x, mean = mean(por$absences), sd = sd(por$absences)), col="red", add = T)
describe(por$absences)
### 1. Escalamiento ----
# Mat
mat$absences1 <- sqrt(mat$absences)

hist(x = mat$absences1, 
     xlim = c(min(mat$absences1), max(mat$absences1)), prob=T, xaxt = "n",
     main="Histograma absences escaladas: Matemática")
axis(1, at = seq(min(mat$absences1), max(mat$absences1), 1), 
     labels = seq(min(mat$absences1), max(mat$absences1), 1))
curve(dnorm(x, mean = mean(mat$absences1), sd = sd(mat$absences1)), col="red", add = T)

describe(mat$absences1)

# Por
por$absences1 <- sqrt(por$absences)

hist(x = por$absences1, 
     xlim = c(min(por$absences1), max(por$absences1)), prob=T, xaxt = "n",
     main="Histograma absences escaladas: Portugués")
axis(1, at = seq(min(por$absences1), max(por$absences1), 1), 
     labels = seq(min(por$absences1), max(por$absences1), 1))
curve(dnorm(x, mean = mean(por$absences1), sd = sd(por$absences1)), col="red", add = T)

describe(por$absences1)

### 2. Niveles ----
# Mat
describe(mat$absences)

aux <- tree(G3 ~ absences, data=train.M)
plot(aux)
text(aux, pretty=0)

mat$absences2 <- mat$absences < 13.5

# Por
describe(por$absences)

aux <- tree(G3 ~ absences, data=train.P)
plot(aux)
text(aux, pretty=0)

por$absences2 <- por$absences < 7.5

### Prueba ----
# Mat
aux <- update(best.M, .~.-abscences+absences1, data=mat[i_train.M, ])
ecm(aux, mat[i_test.M, ]) # ecm = 1.808295

aux2 <- update(best.M, .~.-abscences+absences2, data=mat[i_train.M, ])
ecm(aux2, mat[i_test.M, ]) # ecm = 1.813541

mat$absences <- mat$absences1
mat <- mat[, !(names(mat) %in% c("absences1", "absences2"))]

best.M <- update(best.M, .~., data=mat[i_train.M, ])
# Por
aux <- update(best.P, .~.-abscences+absences1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # ecm = 6.631567

aux2 <- update(best.P, .~.-abscences+absences2, data=por[i_train.P, ])
ecm(aux2, por[i_test.P, ]) # ecm = 6.750767

por$absences <- por$absences1
por <- por[, !(names(por) %in% c("absences1", "absences2"))]

best.P <- update(best.P, .~., data=por[i_train.P, ])
## Age ----
# Mat: No demostró ser importante
# Por
aux <- tree(G3 ~ age, data=train.P)
plot(aux)
text(aux, pretty=0)

por$age1 <- por$age < 18.5

aux <- update(best.P, .~.-age+age1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # 6.437903
por$age <- por$age1
por <- por[, !(names(por) %in% c("age1"))]
best.P <- update(best.P, .~., data=por[i_train.P, ])
## Medu y Fedu ----
# Mat
mat$FMedu <- mat$Fedu + mat$Medu
train.M$FMedu <- train.M$Fedu + train.M$Medu
plot.M(train.M$FMedu)
mat$FMedu1 <- mat$FMedu < 6.5

aux3 <- update(best.M, .~.-Fedu-Medu+FMedu, data=mat[i_train.M, ])
ecm(aux3, mat[i_test.M, ]) # 1.803521
aux4 <- update(best.M, .~.-Fedu-Medu+FMedu1, data=mat[i_train.M, ])
ecm(aux4, mat[i_test.M, ]) # 1.795793

mat$FMedu <- mat$FMedu1
mat <- mat[, !(names(mat) %in% c("FMedu1", "Medu", "Fedu"))]

best.M <- update(best.M, .~.-Medu-Fedu+FMedu, data=mat[i_train.M, ])
# Por
por$FMedu <- por$Fedu + por$Medu

train.P$FMedu <- train.P$Fedu + train.P$Medu
aux <- tree(G3 ~ FMedu, data=train.P)
plot(aux)
text(aux, pretty=0)
por$FMedu1 <- por$FMedu < 5.5

aux3 <- update(best.P, .~.-Fedu-Medu+FMedu, data=por[i_train.P, ])
ecm(aux3, por[i_test.P, ]) # 6.446719
aux4 <- update(best.P, .~.-Fedu-Medu+FMedu1, data=por[i_train.P, ])
ecm(aux4, por[i_test.P, ]) # 6.440613
## Mjob y Fjob ----
# Mat
mat$Fjob1 <- mat$Fjob == "teacher"

aux <- update(best.M, .~.-Fjob+Fjob1, data=mat[i_train.M, ])
ecm(aux, mat[i_test.M, ]) # 1.771323

# Por
plot.P(train.P$Mjob)
por$Mjob1 <- por$Mjob == "teacher"
aux <- update(best.P, .~.-Mjob+Mjob1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # 6.396797

plot.P(train.P$Fjob)
por$Fjob1 <- por$Fjob == "teacher"
aux <- update(best.P, .~.-Fjob+Fjob1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # 6.345318

## studytime ----
# Mat
plot.M(train.M$studytime)
mat$studytime1 <- mat$studytime < 2.5

aux <- update(best.M, .~.-studytime+studytime1, data=mat[i_train.M, ])
ecm(aux, mat[i_test.M, ]) # 1.778755

## goout ----
# Por
plot.P(train.P$goout)
por$goout1 <- por$goout < 4.5

aux <- update(best.P, .~.-goout+goout1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # 6.352564

## Dalc y Walc ----
# Mat
plot.M(train.M$Dalc)
mat$Dalc1 <- mat$Dalc < 1.5

plot.M(train.M$Walc)
mat$Walc1 <- mat$Walc < 2.5

aux <- update(best.M, .~.-Dalc+Dalc1, data=mat[i_train.M, ])
ecm(aux, mat[i_test.M, ]) # 1.793211

aux <- update(best.M, .~.-Walc+Walc1, data=mat[i_train.M, ])
ecm(aux, mat[i_test.M, ]) # 1.792206

# Por
plot.P(train.P$Dalc)
por$Dalc1 <- por$Dalc < 1.5

aux <- update(best.P, .~.-Dalc+Dalc1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # 6.428079

## health ----
# Por
plot.P(train.P$health)
por$health1 <- por$health < 4.5

aux <- update(best.P, .~.-health+health1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # 6.380522

## freetime ----
# Por
plot.P(train.P$freetime)
por$freetime1 <- por$freetime < 3.5

aux <- update(best.P, .~.-freetime+freetime1, data=por[i_train.P, ])
ecm(aux, por[i_test.P, ]) # 6.436016
