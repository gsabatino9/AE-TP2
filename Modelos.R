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
  library(Hmisc) # corr
  library(corrplot) ## corr
  print("MASS, ISLR2, carData, car, boot, leaps, glmnet, tree, randomForest, gbm, BART")
}

load_libraries()
setwd("~/Desktop/AE-FINAL-TP/AE-TP2")
mat <- read.table("student-mat.csv",sep=",",header=TRUE)
por <- read.table("student-por.csv",sep=",",header=TRUE)
length(c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33))
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
describe(por$G3)

hist(x = mat$G3, breaks = seq(min(mat$G3), max(mat$G3), 1), 
     xlim = c(min(mat$G3), max(mat$G3)), prob=T, xaxt = "n", 
     main = "Distribución de G3, dataset Matemática",
     xlab = "G3", ylab = "Frecuencia relativa")
axis(1, at = seq(min(mat$G3), max(mat$G3), 1), 
     labels = seq(min(mat$G3), max(mat$G3), 1))
curve(dnorm(x, mean = mean(mat$G3), sd = sd(mat$G3)), col="red", add = T)

# idem but with "por":
hist(x = por$G3, breaks = seq(min(por$G3), max(por$G3), 1), 
     xlim = c(min(por$G3), max(por$G3)), prob=T, xaxt = "n", 
     main = "Distribución de G3, dataset Portugués",
     xlab = "G3", ylab = "Frecuencia relativa")
axis(1, at = seq(min(por$G3), max(por$G3), 1),
      labels = seq(min(por$G3), max(por$G3), 1))
curve(dnorm(x, mean = mean(por$G3), sd = sd(por$G3)), col="red", add = T)

# idem but variable [absences]
hist(x = mat$absences, breaks = seq(min(mat$absences), max(mat$absences), 1), 
     xlim = c(min(mat$absences), max(mat$absences)), prob=T, xaxt = "n", 
     main = "Distribución de absences, dataset Matemática",
     xlab = "absences", ylab = "Frecuencia relativa")
axis(1, at = seq(min(mat$absences), max(mat$absences), 1), 
     labels = seq(min(mat$absences), max(mat$absences), 1))
curve(dgamma(x, shape = 1.5, scale = 1/0.5), col="blue", add = T)

# idem but variable [absences] in dataset "por"
hist(x = por$absences, breaks = seq(min(por$absences), max(por$absences), 1), 
     xlim = c(min(por$absences), max(por$absences)), prob=T, xaxt = "n", 
     main = "Distribución de absences, dataset Portugués",
     xlab = "absences", ylab = "Frecuencia relativa")
axis(1, at = seq(min(por$absences), max(por$absences), 1),
     labels = seq(min(por$absences), max(por$absences), 1))
curve(dgamma(x, shape = 1.5, scale = 1/0.5), col="blue", add = T)

describe(por$absences)
describe(mat$absences)



## Descripción de escuelas ----
library(psych)
describeBy(mat$G1, mat$school, mat = T, digits = 2)
describeBy(mat$G2, mat$school, mat = T, digits = 2)
describeBy(mat$G3, mat$school, mat = T, digits = 2)
ap <- describeBy(mat$G3, mat$school, mat = T, digits = 2)
png("mat6.png", width=600, height=100)
p<-tableGrob(ap)
grid.arrange(p)
dev.off()
describeBy(mat$G3, mat$school, mat = T, digits = 2)
describeBy(por$G3, por$school, mat = T, digits = 2)
ap <- describeBy(por$G3, por$school, mat = T, digits = 2, skew=F)
png("mat6.png", width=600, height=100)
p<-tableGrob(ap)
grid.arrange(p)
dev.off()

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

ggplot(data = por, aes(x=school, y = G3)) +
  geom_boxplot(aes(fill=school)) +
  labs(title ="Means of the final grade in two schools", y = "Final grade (G3)")+
  theme(plot.title = element_text(size = 15, hjust=0.5))+
  scale_fill_manual(values=c("#0097BF", "#204FFF")) +
  scale_y_continuous(breaks=c(1:20))

## Correlación ----
# Matw
RawDataMatrix <- subset(train.M, select=c(age, Medu, Fedu, traveltime, studytime, failures,famrel, freetime, goout, Dalc, Walc, health, absences, G1, G2, G3))
corMatrix2 <- rcorr(as.matrix(RawDataMatrix), 
                    type = c("pearson","spearman"))
corMatrix2$r <- round(corMatrix2$r, 2)
corMatrix2$P

corrplot(corMatrix2$r, p.mat = corMatrix2$P, sig.level = 0.05, insig = "blank")
corrplot(corMatrix2$r, p.mat = corMatrix2$P, sig.level = 0.05, insig = "blank", 
         diag=F, type="lower")
corrplot(corMatrix2$r, p.mat = corMatrix2$P, sig.level = 0.05, insig = "blank",
         order="hclust", addrect=2)

# Por
RawDataMatrix <- subset(train.P, select=c(age, Medu, Fedu, traveltime, studytime, failures,famrel, freetime, goout, Dalc, Walc, health, absences, G1, G2, G3))
corMatrix2 <- rcorr(as.matrix(RawDataMatrix), 
                    type = c("pearson","spearman"))
corMatrix2$r <- round(corMatrix2$r, 2)
corMatrix2$P
corrplot(corMatrix2$r, p.mat = corMatrix2$P, sig.level = 0.05, insig = "blank")
