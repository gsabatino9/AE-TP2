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
     xlab = "", sub = "", cex = .9, label=F)
abline(h=20, col="red")
abline(h=15, col="chocolate")
abline(h=18, col="blue")

hc.average <- hclust(dist(por), method="average")

plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9, label=F)
abline(h=11, col="red")
abline(h=12, col="chocolate")
abline(h=13, col="blue")

dd <- as.dist(1-cor(t(por)))
hc.complete <- hclust(dd, method="complete")
plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9, label=F)
abline(h=0.3, col="red")
abline(h=0.2, col="chocolate")
abline(h=0.15, col="blue")

xsc <- scale(por)
scaled.hc <- hclust(dist(xsc), method = "complete")
plot(scaled.hc,
     main = "Hierarchical Clustering with Scaled Features")
abline(h=13, col="red")
abline(h=12, col="chocolate")
abline(h=10, col="blue")
abline(h=9, col="deepskyblue")


### K-means ----
wssplot <- function(data, nc=15, seed=9){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group",pch=20)}

wssplot(por, nc = 20) # nos quedamos con 4.

set.seed(9)
km.out <- kmeans(por, 5, nstart=20)

## Analizamos el conjunto:
## -----
groups <- km.out$cluster
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
  a <- round(apply(aux2[groups==i, ], 2, mean), digits=4)
  cmp <- cbind(cmp, a)
}
group <- function(i) {
  return(aux2[groups==i, ])
}

cmp <- cmp[, -1]
colnames(cmp) <- c('g1', 'g2', 'g3', 'g4', 'g5')
View(cmp)
View(cmp[c("school", "studytime", "failures", "Dalc", "absences", "G1",
             "G2", "G3"), ])

library(gridExtra)
png("por1.png", width=400, height=200)
p<-tableGrob(cmp[c("school", "studytime", "failures", "Dalc", "absences", "G1",
                   "G2", "G3"), ])
grid.arrange(p)
dev.off()

## Vemos qué pasa en school == "MS":
set.seed(9)
t <- por[por$school == 2, ]
km.out <- kmeans(t, 3, nstart=20)

groups <- km.out$cluster
#groups <- cutree(hc.complete, h=18)
table(groups)

aux3 <- aux[aux$school == "MS", ]
aux2 <- map_df(aux3, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

aux2$age <- aux3$age
aux2$G1 <- aux3$G1
aux2$G2 <- aux3$G2
aux2$G3 <- aux3$G3
aux2$absences <- aux3$absences
aux2$failures <- aux3$failures
aux2$traveltime <- aux3$traveltime
aux2$studytime <- aux3$studytime

# para comparar las medias:
cmp <- data.frame("col"=names(aux3))

for (i in 1:max(groups)) {
  a <- round(apply(aux2[groups==i, ], 2, mean), digits=4)
  cmp <- cbind(cmp, a)
}
group <- function(i) {
  return(aux2[groups==i, ])
}

cmp <- cmp[, -1]
cmp <- cmp[-1, ]
colnames(cmp) <- c('g1', 'g2', 'g3')
View(cmp)

png("por2.png", width=350, height=300)
p<-tableGrob(cmp[c("age", "higher", "studytime", "failures", "freetime", 
                   "Dalc", "Walc", "absences", 
                   "G1", "G2", "G3"), ])
grid.arrange(p)
dev.off()

### PCA + jerárquico ----
pr.out <- prcomp(por, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9, label=F) # mucho más balanceado.
abline(h=10, col="red")
abline(h=9, col="blue")
abline(h=8, col="chocolate")
abline(h=7, col="deepskyblue")
abline(h=6, col="green")
abline(h=4, col="pink", lwd=2)


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

plot(hc.complete, main = "Clustering por columnas: Portugués",
     xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9)
### PCA + jerárquico ----
pr.out <- prcomp(por, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9)
