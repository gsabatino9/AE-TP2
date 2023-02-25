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




# Análisis de variables ----
library(stargazer)
stargazer(mat, type="html", out="1.html")
stargazer(mat, type = "html", nobs = FALSE, mean.sd = TRUE, median = TRUE,
          iqr = TRUE, out="2.html")

library(psych)
describeBy(mat$G1, mat$school, mat = T, digits = 2)
describeBy(mat$G2, mat$school, mat = T, digits = 2)
describeBy(mat$G3, mat$school, mat = T, digits = 2)

hist(x = mat$G3, breaks = seq(min(mat$G3), max(mat$G3), 1), 
     xlim = c(min(mat$G3), max(mat$G3)), prob=T, xaxt = "n")
axis(1, at = seq(min(mat$G3), max(mat$G3), 1), 
     labels = seq(min(mat$G3), max(mat$G3), 1))
curve(dnorm(x, mean = mean(mat$G3), sd = sd(mat$G3)), col="red", add = T)

hist(x = mat[mat$school == "MS", ]$G3, breaks = seq(min(mat[mat$school == "MS", ]$G3), max(mat[mat$school == "MS", ]$G3), 1), 
     xlim = c(min(mat[mat$school == "MS", ]$G3), max(mat[mat$school == "MS", ]$G3)), prob=T, xaxt = "n")
axis(1, at = seq(min(mat[mat$school == "MS", ]$G3), max(mat[mat$school == "MS", ]$G3), 1), 
     labels = seq(min(mat[mat$school == "MS", ]$G3), max(mat[mat$school == "MS", ]$G3), 1))
curve(dnorm(x, mean = mean(mat[mat$school == "MS", ]$G3), sd = sd(mat[mat$school == "MS", ]$G3)), col="red", add = T)

hist(x = mat[mat$school == "GP", ]$G3, breaks = seq(min(mat[mat$school == "GP", ]$G3), max(mat[mat$school == "GP", ]$G3), 1), 
     xlim = c(min(mat[mat$school == "GP", ]$G3), max(mat[mat$school == "GP", ]$G3)), prob=T, xaxt = "n")
axis(1, at = seq(min(mat[mat$school == "GP", ]$G3), max(mat[mat$school == "GP", ]$G3), 1), 
     labels = seq(min(mat[mat$school == "GP", ]$G3), max(mat[mat$school == "GP", ]$G3), 1))
curve(dnorm(x, mean = mean(mat[mat$school == "GP", ]$G3), sd = sd(mat[mat$school == "GP", ]$G3)), col="red", add = T)

qqnorm(mat[mat$school == "GP", ]$G3); qqline(mat[mat$school == "GP", ]$G3, col = "red")
qqnorm(mat[mat$school == "MS", ]$G3); qqline(mat[mat$school == "MS", ]$G3, col = "red")

ggplot(data = mat, aes(x=school, y = G1)) +
  geom_boxplot(aes(fill=school)) +
  labs(title ="Means of the initial grade in two schools", y = "Initial grade (G1)")+
  theme(plot.title = element_text(size = 15, hjust=0.5))+
  scale_fill_manual(values=c("#0097BF", "#204FFF")) +
  scale_y_continuous(breaks=c(1:20))

ggplot(data = mat, aes(x=school, y = G2)) +
  geom_boxplot(aes(fill=school)) +
  labs(title ="Means of the second grade in two schools", y = "Second grade (G2)")+
  theme(plot.title = element_text(size = 15, hjust=0.5))+
  scale_fill_manual(values=c("#0097BF", "#204FFF")) +
  scale_y_continuous(breaks=c(1:20))

ggplot(data = mat, aes(x=school, y = G3)) +
  geom_boxplot(aes(fill=school)) +
  labs(title ="Means of the final grade in two schools", y = "Final grade (G3)")+
  theme(plot.title = element_text(size = 15, hjust=0.5))+
  scale_fill_manual(values=c("#0097BF", "#204FFF")) +
  scale_y_continuous(breaks=c(1:20))

## Escalo absences
hist(x = mat$absences, breaks = seq(min(mat$absences), max(mat$absences), 1), 
     xlim = c(min(mat$absences), max(mat$absences)), prob=T, xaxt = "n")
axis(1, at = seq(min(mat$absences), max(mat$absences), 1), 
     labels = seq(min(mat$absences), max(mat$absences), 1))
curve(dnorm(x, mean = mean(mat$absences), sd = sd(mat$absences)), col="red", add = T)
describe(mat$absences)

## Tomo raíz cuadrada
mat$absences <- sqrt(mat$absences)

hist(x = mat$absences, 
     xlim = c(min(mat$absences), max(mat$absences)), prob=T, xaxt = "n")
axis(1, at = seq(min(mat$absences), max(mat$absences), 1), 
     labels = seq(min(mat$absences), max(mat$absences), 1))
curve(dnorm(x, mean = mean(mat$absences), sd = sd(mat$absences)), col="red", add = T)

describe(mat$absences)

# Split train-test ----
set.seed(9)
aux_train <- sample(1:nrow(mat), nrow(mat)*0.7)
aux_test <- (-aux_train)

train <- mat[aux_train, ]
test <- mat[aux_test, ]


n <- nrow(train)
p <- ncol(train)-1
# Un modelo de prueba ----
calcular_ecm <- function(y.hat) {
  return(mean((y.hat - test$G3)^2))
}

lm.fit <- lm(G3 ~ ., data=train)
calcular_ecm(predict(lm.fit, test))

step(lm.fit, direction = 'backward')

lm.back <- lm(formula = G3 ~ school + age + nursery + internet + famrel + 
                absences + G1 + G2, data = train)
calcular_ecm(predict(lm.back, test))

selection(x=subset(train, select=names(train),
                   y = mat$G3, q=5, seconds = T, nmodels = 2))

# Análisis para el mejor modelo
anova(lm.fit, lm.back) # pongp todos los modelos
leveragePlots(lm.fit) # analizo el que selecciono
hist(residuals(lm.fit))

describe(residuals(lm.fit))
durbinWatsonTest(lm.fit)
vif(lm.fit)

# Otro análisis ----

library(Hmisc)
RawDataMatrix <- subset(train, select=c(age, Medu, Fedu, traveltime, studytime, failures,famrel, freetime, goout, Dalc, Walc, health, absences, G1, G2, G3))
corMatrix2 <- rcorr(as.matrix(RawDataMatrix), 
                    type = c("pearson","spearman"))

corMatrix2$r <- round(corMatrix2$r, 2)
corMatrix2$P

library(corrplot)
corrplot(corMatrix2$r, p.mat = corMatrix2$P, sig.level = 0.05, insig = "blank")
corrplot(corMatrix2$r, p.mat = corMatrix2$P, sig.level = 0.05, insig = "blank", 
         diag=F, type="lower")
corrplot(corMatrix2$r, p.mat = corMatrix2$P, sig.level = 0.05, insig = "blank",
         order="hclust", addrect=2)

# LASSO ----
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

sst <- sum((test$G3 - mean(test$G3))^2)
sse <- sum((lasso.pred - test$G3)^2)
rsq <- 1 - (sse * (n-1))/(sst * (n-p))

lm2 <- lm(G3 ~ age+studytime+failures+romantic+absences+G1+G2,
          data=train2)
y.h <- predict(lm2, test2)
mean((y.h - test2$G3)^2)

# Saco presuntos outliers ----
# Forma 1: Cambio los 0s por 1s:
train$G3 <- ifelse(train$G3 == 0, 1, train$G3)
train$G2 <- ifelse(train$G2 == 0, 1, train$G2)
train$G1 <- ifelse(train$G1 == 0, 1, train$G1)

test$G3 <- ifelse(test$G3 == 0, 1, test$G3)
test$G2 <- ifelse(test$G2 == 0, 1, test$G2)
test$G1 <- ifelse(test$G1 == 0, 1, test$G1)

# Forma 2: Los remuevo directamente:
train <- train[train$G1 & train$G2 & train$G3, ]
test <- test[test$G1 & test$G2 & test$G3, ]

n <- nrow(train)
