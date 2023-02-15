# PORTUGUÉS ----
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
  library("tidyverse")
  library("caret")
  print("MASS, ISLR2, carData, car, boot, leaps, glmnet, tree, randomForest, gbm, BART")
}

load_libraries()
por <- read.table("student-por.csv",sep=",",header=TRUE)

por[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(por[-c(3,13,14,15,30,31,32,33)], factor)

aux <- por
por <- data.frame(lapply(por, as.numeric))

# Calculo binario: Aprobado / Desaprobado ----
por$G3 <- ifelse(por$G3 >= 12, 1 ,0)
por$G1 <- ifelse(por$G1 >= 12, 1 ,0)
por$G2 <- ifelse(por$G2 >= 12, 1 ,0)
clusters_bin <- data.frame(as.numeric(row.names(por)))

## Clustering por fila ----
### Clustering jerárquico ----
hc.complete <- hclust(dist(por), method="complete")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)
abline(h=20, col="red")
abline(h=15, col="chocolate")
abline(h=13, col="blue")

clusters_bin$JC1 <- cutree(hc.complete, h=20)
clusters_bin$JC2 <- cutree(hc.complete, h=15)
clusters_bin$JC3 <- cutree(hc.complete, h=13)

dd <- as.dist(1-cor(t(por)))
hc.complete <- hclust(dd, method="complete")
plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)
abline(h=0.3, col="red")
abline(h=0.2, col="chocolate")
abline(h=0.15, col="blue")

clusters_bin$JCor1 <- cutree(hc.complete, h=0.3)
clusters_bin$JCor2 <- cutree(hc.complete, h=0.2)
clusters_bin$JCor3 <- cutree(hc.complete, h=0.15)

xsc <- scale(por)
scaled.hc <- hclust(dist(xsc), method = "complete")
plot(scaled.hc,
     main = "Hierarchical Clustering with Scaled Features")
abline(h=13, col="red")
abline(h=12, col="chocolate")
abline(h=10, col="blue")
abline(h=9, col="deepskyblue")

clusters_bin$JC4 <- cutree(scaled.hc, h=13)
clusters_bin$JC5 <- cutree(scaled.hc, h=12)
clusters_bin$JC6 <- cutree(scaled.hc, h=10)
clusters_bin$JC7 <- cutree(scaled.hc, h=9)


### K-means ----
xsd <- scale(por)
set.seed(9)
# pongo 4 para que tenga la misma cantidad que jerárquico
km.out <- kmeans(xsd, 4, nstart=20)
set.seed(9)
clusters_bin$K1 <- kmeans(xsd, 4, nstart=20)$cluster
set.seed(9)
clusters_bin$K2 <- kmeans(xsd, 5, nstart=20)$cluster
set.seed(9)
clusters_bin$K3 <- kmeans(xsd, 6, nstart=20)$cluster
set.seed(9)
clusters_bin$K4 <- kmeans(xsd, 7, nstart=20)$cluster
set.seed(9)
clusters_bin$K5 <- kmeans(xsd, 8, nstart=20)$cluster

### PCA + jerárquico ----
pr.out <- prcomp(por, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9) # mucho más balanceado.
abline(h=10, col="red")
abline(h=9, col="blue")
abline(h=8, col="chocolate")
abline(h=7, col="deepskyblue")
abline(h=6, col="green")
abline(h=4, col="pink", lwd=2)

clusters_bin$JC8 <- cutree(hc.out, h=10)
clusters_bin$JC9 <- cutree(hc.out, h=9)
clusters_bin$JC10 <- cutree(hc.out, h=8)
clusters_bin$JC11 <- cutree(hc.out, h=7)
clusters_bin$JC12 <- cutree(hc.out, h=6)
clusters_bin$JC13 <- cutree(hc.out, h=4)

## Clustering por columna ----
por <- t(por)
### Jerárquico ----
hc.complete <- hclust(dist(por), method="complete")
hc.average <- hclust(dist(por), method="average")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9)

hc.scale <- hclust(dist(scale(por)), method="complete")
plot(hc.scale, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)

### Con distancia: Correlación ----
dd <- as.dist(1 - cor(t(por)))
hc.complete <- hclust(dd, method="complete")
hc.average <- hclust(dd, method="average")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9)
### PCA + jerárquico ----
pr.out <- prcomp(por, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9)
