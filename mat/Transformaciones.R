# MATEMÁTICA ----
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

mat$freetime <- as.numeric(mat$freetime)
mat$famrel <- as.numeric(mat$famrel)
mat$goout <- as.numeric(mat$goout)
mat$Dalc <- as.numeric(mat$Dalc)
mat$Walc <- as.numeric(mat$Walc)
mat$health <- as.numeric(mat$health)
mat$Medu <- as.numeric(mat$Medu)
mat$Fedu <- as.numeric(mat$Fedu)




# Split train-test ----
set.seed(9)
aux_train <- sample(1:nrow(mat), nrow(mat)*0.7)
aux_test <- (-aux_train)

train <- mat[aux_train, ]
test <- mat[aux_test, ]


n <- nrow(train)
p <- ncol(train)-1
# Análisis ----
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)
library(egg)

train <- train[train$G3 != 0, ]
tgt=function(txt) #Visualize the target
{ds=ggplot(train,aes(G3))+geom_histogram(color='black',fill=txt,bins = 50) #Skew? Kurtosis?
bxp=ggplot(train,aes(G3))+geom_boxplot(color='black',fill=txt) #Outliers?
qq= ggplot(train, aes(sample=G3))+ stat_qq() + stat_qq_line() #Normal distribution?
ggarrange(ds, qq, bxp,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
}
tgt('red')

train$G3 <- log(train$G3) #or box cox transformation
tgt('blue')

lambda  <- forecast::BoxCox.lambda(train$G3)
train_bc_y <- forecast::BoxCox(train$G3, lambda)
test_bc_y <- forecast::BoxCox(test$G3, lambda)

hist(train_bc_y)

library(caret)
features <- setdiff(names(train), "G3")
pre_process <- preProcess(
  x      = train[, features],
  method = c("center", "scale")    
)
train_x <- predict(pre_process, train[, features])
test_x <- predict(pre_process, test[, features])
# Más sobre esto acá: http://uc-r.github.io/regression_preparation

train_new <- train_x
train_new$G3 <- train_bc_y

lm.fit <- lm(G3 ~ ., data=train_new)
summary(lm.fit)

calcular_ecm <- function(y.hat) {
  return(mean((y.hat - test_bc_y)^2))
}

calcular_ecm(predict(lm.fit, test_x))
