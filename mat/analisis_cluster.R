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
  library("tidyverse")
  library("caret")
  print("MASS, ISLR2, carData, car, boot, leaps, glmnet, tree, randomForest, gbm, BART")
}

load_libraries()
mat <- read.table("student-mat.csv",sep=",",header=TRUE)

mat[-c(3,13,14,15,30,31,32,33)] <- 
  lapply(mat[-c(3,13,14,15,30,31,32,33)], factor)

mat <- data.frame(lapply(mat, as.numeric))
aux <- mat

# Calculo binario: Aprobado / Desaprobado ----
mat$G3 <- ifelse(mat$G3 >= 12, 1 ,0)
mat$G1 <- ifelse(mat$G1 >= 12, 1 ,0)
mat$G2 <- ifelse(mat$G2 >= 12, 1 ,0)
clusters_bin <- data.frame(as.numeric(row.names(mat)))

## Clustering por fila ----
### Clustering jerárquico ----
hc.complete <- hclust(dist(mat), method="complete")
hc.average <- hclust(dist(mat), method="average")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)
abline(h=40, col="red")
abline(h=30, col="chocolate")
abline(h=20, col="blue")
abline(h=17, col="orange")

clusters_bin$JC1 <- cutree(hc.complete, h=40)
clusters_bin$JC2 <- cutree(hc.complete, h=30)
clusters_bin$JC3 <- cutree(hc.complete, h=20)
clusters_bin$JC4 <- cutree(hc.complete, h=17)

xsc <- scale(mat)
scaled.hc <- hclust(dist(xsc), method = "complete")
plot(scaled.hc,
     main = "Hierarchical Clustering with Scaled Features")
abline(h=11.5, col="blue")
abline(h=10, col="chocolate")
abline(h=11, col="red")

clusters_bin$JC5 <- cutree(scaled.hc, h=11)
clusters_bin$JC6 <- cutree(scaled.hc, h=11.5)
clusters_bin$JC7 <- cutree(scaled.hc, h=10)

# h = 12.5 -> 4 grupos
groups <- cutree(scaled.hc, h=12.5)
table(groups)

aux2 <- map_df(aux, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

aux2$age <- aux$age
aux2$G1 <- aux$G1
aux2$G2 <- aux$G2
aux2$G3 <- aux$G3
aux2$absences <- aux$absences
aux2$failures <- aux$failures
aux2$traveltime <- aux$traveltime
aux2$studytime <- aux$studytime

# para comparar las medias:
cmp <- data.frame("col"=names(aux))

for (i in 1:max(groups)) {
  a <- apply(aux2[groups==i, ], 2, mean)
  cmp <- cbind(cmp, a)
}
group <- function(i) {
  return(aux2[groups==i, ])
}

View(cmp)
### K-means ----
xsd <- scale(mat)
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

km.clusters <- km.out$cluster

K1 <- km.clusters
g1 <- aux[km.clusters == 1, ]
g2 <- aux[km.clusters == 2, ]
g3 <- aux[km.clusters == 3, ]
g4 <- aux[km.clusters == 4, ]

g1.2 <- map_df(g1, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
g2.2 <- map_df(g2, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
g3.2 <- map_df(g3, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
g4.2 <- map_df(g4, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

g1.2$age <- g1$age
g1.2$G1 <- g1$G1
g1.2$G2 <- g1$G2
g1.2$G3 <- g1$G3
g1.2$absences <- g1$absences
g1.2$failures <- g1$failures
g1.2$traveltime <- g1$traveltime
g1.2$studytime <- g1$studytime

g2.2$age <- g2$age
g2.2$G1 <- g2$G1
g2.2$G2 <- g2$G2
g2.2$G3 <- g2$G3
g2.2$absences <- g2$absences
g2.2$failures <- g2$failures
g2.2$traveltime <- g2$traveltime
g2.2$studytime <- g2$studytime

g3.2$age <- g3$age
g3.2$G1 <- g3$G1
g3.2$G2 <- g3$G2
g3.2$G3 <- g3$G3
g3.2$absences <- g3$absences
g3.2$failures <- g3$failures
g3.2$traveltime <- g3$traveltime
g3.2$studytime <- g3$studytime

g4.2$age <- g4$age
g4.2$G1 <- g4$G1
g4.2$G2 <- g4$G2
g4.2$G3 <- g4$G3
g4.2$absences <- g4$absences
g4.2$failures <- g4$failures
g4.2$traveltime <- g4$traveltime
g4.2$studytime <- g4$studytime

# para comparar las medias:
cmp <- data.frame("col"=names(aux))
cmp$g1 <- apply(g1.2, 2, mean)
cmp$g2 <- apply(g2.2, 2, mean)
cmp$g3 <- apply(g3.2, 2, mean)
cmp$g4 <- apply(g4.2, 2, mean)
View(cmp)
### PCA + jerárquico ----
pr.out <- prcomp(mat, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9) # mucho más balanceado.
abline(h=9, col="red")
abline(h=8, col="blue")
abline(h=7, col="chocolate")
abline(h=6, col="deepskyblue")

clusters_bin$JC8 <- cutree(hc.out, h=9)
clusters_bin$JC9 <- cutree(hc.out, h=8)
clusters_bin$JC10 <- cutree(hc.out, h=7)
clusters_bin$JC11 <- cutree(hc.out, h=6)

groups <- cutree(hc.out, 5)
g1 <- aux[groups == 1, ]
g2 <- aux[groups == 2, ]
g3 <- aux[groups == 3, ]
g4 <- aux[groups == 4, ]
g5 <- aux[groups == 5, ]

g1.2 <- map_df(g1, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
g2.2 <- map_df(g2, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
g3.2 <- map_df(g3, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
g4.2 <- map_df(g4, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})
g5.2 <- map_df(g5, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

g1.2$age <- g1$age
g1.2$G1 <- g1$G1
g1.2$G2 <- g1$G2
g1.2$G3 <- g1$G3
g1.2$absences <- g1$absences
g1.2$failures <- g1$failures
g1.2$traveltime <- g1$traveltime
g1.2$studytime <- g1$studytime

g2.2$age <- g2$age
g2.2$G1 <- g2$G1
g2.2$G2 <- g2$G2
g2.2$G3 <- g2$G3
g2.2$absences <- g2$absences
g2.2$failures <- g2$failures
g2.2$traveltime <- g2$traveltime
g2.2$studytime <- g2$studytime

g3.2$age <- g3$age
g3.2$G1 <- g3$G1
g3.2$G2 <- g3$G2
g3.2$G3 <- g3$G3
g3.2$absences <- g3$absences
g3.2$failures <- g3$failures
g3.2$traveltime <- g3$traveltime
g3.2$studytime <- g3$studytime

g4.2$age <- g4$age
g4.2$G1 <- g4$G1
g4.2$G2 <- g4$G2
g4.2$G3 <- g4$G3
g4.2$absences <- g4$absences
g4.2$failures <- g4$failures
g4.2$traveltime <- g4$traveltime
g4.2$studytime <- g4$studytime

g5.2$age <- g5$age
g5.2$G1 <- g5$G1
g5.2$G2 <- g5$G2
g5.2$G3 <- g5$G3
g5.2$absences <- g5$absences
g5.2$failures <- g5$failures
g5.2$traveltime <- g5$traveltime
g5.2$studytime <- g5$studytime

# para comparar las medias:
cmp <- data.frame("col"=names(aux))
cmp$g1 <- apply(g1.2, 2, mean)
cmp$g2 <- apply(g2.2, 2, mean)
cmp$g3 <- apply(g3.2, 2, mean)
cmp$g4 <- apply(g4.2, 2, mean)
cmp$g5 <- apply(g5.2, 2, mean)
View(cmp)

## Clustering por columna ----
mat <- t(mat)
### Jerárquico ----
hc.complete <- hclust(dist(mat), method="complete")
hc.average <- hclust(dist(mat), method="average")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9) # tremendo
plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9)

hc.scale <- hclust(dist(scale(mat)), method="complete")
plot(hc.scale, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)

### PCA + jerárquico ----
pr.out <- prcomp(mat, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9)

# Calculo 4 niveles: MM, M, B, E ----
mat <- aux
mat <- data.frame(lapply(mat, as.numeric))

mat$G1 <- ifelse(mat$G1 > 16, 3, 
                 ifelse(mat$G1 > 11, 2, 
                        ifelse(mat$G1 > 6, 1, 0)))
mat$G2 <- ifelse(mat$G2 > 16, 3, 
                 ifelse(mat$G2 > 11, 2, 
                        ifelse(mat$G2 > 6, 1, 0)))
mat$G3 <- ifelse(mat$G3 > 16, 3, 
                 ifelse(mat$G3 > 11, 2, 
                        ifelse(mat$G3 > 6, 1, 0)))

clusters_4L <- data.frame(as.numeric(row.names(mat)))
## Clustering por fila ----
### Clustering jerárquico ----
hc.complete <- hclust(dist(mat), method="complete")
hc.average <- hclust(dist(mat), method="average")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)
abline(h=20, col="red")
abline(h=12, col="chocolate")

clusters_4L$JC1 <- cutree(hc.complete, h=20)
clusters_4L$JC2 <- cutree(hc.complete, h=12)

plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9) # muy similar

xsc <- scale(mat)
scaled.hc <- hclust(dist(xsc), method = "complete")
plot(scaled.hc,
     main = "Hierarchical Clustering with Scaled Features")
abline(h=12, col="blue")
abline(h=11, col="red")
abline(h=10, col="chocolate")
abline(h=8, col="deepskyblue")

clusters_4L$JC3 <- cutree(scaled.hc, h=12)
clusters_4L$JC4 <- cutree(scaled.hc, h=11)
clusters_4L$JC5 <- cutree(scaled.hc, h=10)
clusters_4L$JC6 <- cutree(scaled.hc, h=8)

### K-means ----
xsd <- scale(mat)

set.seed(9)
clusters_4L$K1 <- kmeans(xsd, 4, nstart=20)$cluster
set.seed(9)
clusters_4L$K2 <- kmeans(xsd, 5, nstart=20)$cluster
set.seed(9)
clusters_4L$K3 <- kmeans(xsd, 6, nstart=20)$cluster
set.seed(9)
clusters_4L$K4 <- kmeans(xsd, 7, nstart=20)$cluster
set.seed(9)
clusters_4L$K5 <- kmeans(xsd, 8, nstart=20)$cluster
### PCA + jerárquico ----
pr.out <- prcomp(mat, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9) # mucho más balanceado.
abline(h=9, col="red")
abline(h=8, col="blue")
abline(h=7, col="chocolate")
abline(h=6, col="deepskyblue")
abline(h=5, col="orange")

clusters_4L$JC7 <- cutree(hc.out, h=9)
clusters_4L$JC8 <- cutree(hc.out, h=8)
clusters_4L$JC9 <- cutree(hc.out, h=7)
clusters_4L$JC10 <- cutree(hc.out, h=6)
clusters_4L$JC11 <- cutree(hc.out, h=5)

## Clustering por columna ----
mat <- t(mat)
### Jerárquico ----
hc.complete <- hclust(dist(mat), method="complete")
hc.average <- hclust(dist(mat), method="average")

plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9) # tremendo
plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9)

hc.scale <- hclust(dist(scale(mat)), method="complete")
plot(hc.scale, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9)

### PCA + jerárquico ----
pr.out <- prcomp(mat, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9)
