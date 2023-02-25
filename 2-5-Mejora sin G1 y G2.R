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

# Modelos ----
best.M <- lm(G3 ~ school+sex+age+address+FMedu+Mjob+
               Fjob+guardian+studytime+failures+schoolsup+famsup+
               paid+activities+internet+romantic+goout+Dalc+Walc+
               health+absences, data=train.M)

best.P <- lm(G3 ~ school+sex+age+FMedu+Mjob+Fjob+reason+
               guardian+traveltime+studytime+failures+schoolsup+
               famsup+paid+activities+nursery+higher+internet+romantic+
               famrel+freetime+goout+Dalc+health+absences, data=train.P)

ecm <- function(modelo, a_testear) {
  y.hat <- predict(modelo, a_testear)
  return(mean((y.hat - a_testear$G3)^2))
}

ecm(best.M, test.M) # 1.757575
ecm(best.P, test.P) # 6.261734
