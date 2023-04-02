# MATEMÁTICA ----
# Lectura de datos ----
## Importo librerías
load_libraries <- function() {
  library(MASS)
  library(jtools)
  library(broom.mixed)
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

mat[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(mat[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], as.numeric)
por[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(por[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], as.numeric)

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

# Análisis de variables ----
#library(stargazer)
#stargazer(mat, type="html", out="1.html")
#stargazer(mat, type = "html", nobs = FALSE, mean.sd = TRUE, median = TRUE,
#         iqr = TRUE, out="2.html")

## Descripción inicial ----
describe(mat$G3)

hist(x = mat$G3, breaks = seq(min(mat$G3), max(mat$G3), 1), 
     xlim = c(min(mat$G3), max(mat$G3)), prob=T, xaxt = "n")
axis(1, at = seq(min(mat$G3), max(mat$G3), 1), 
     labels = seq(min(mat$G3), max(mat$G3), 1))
curve(dnorm(x, mean = mean(mat$G3), sd = sd(mat$G3)), col="red", add = T)

## Descripción de escuelas ----
library(psych)
describeBy(mat$G1, mat$school, mat = T, digits = 2)
describeBy(mat$G2, mat$school, mat = T, digits = 2)
describeBy(mat$G3, mat$school, mat = T, digits = 2)

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

qqnorm(mat[mat$school == "GP", ]$G3, pch=20); qqline(mat[mat$school == "GP", ]$G3, col = "red")
qqnorm(mat[mat$school == "MS", ]$G3, pch=20); qqline(mat[mat$school == "MS", ]$G3, col = "red")

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

## Correlación ----
library(Hmisc)
# Mat
RawDataMatrix <- subset(train.M, select=c(age, Medu, Fedu, traveltime, studytime, failures,famrel, freetime, goout, Dalc, Walc, health, absences, G1, G2, G3))
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
