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

aux <- mat
mat <- data.frame(lapply(mat, as.numeric))

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

xsc <- scale(mat)
scaled.hc <- hclust(dist(xsc), method = "complete")
plot(scaled.hc,
     main = "Hierarchical Clustering with Scaled Features")
abline(h=11.5, col="blue")
abline(h=10, col="chocolate")
abline(h=11, col="red")

##### Por correlación ----
dd <- as.dist(1-cor(t(mat)))
hc.complete <- hclust(dd, method="complete")
plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9, label=F)
rect.hclust(hc.complete , h = 0.1, border = 2:15, )
abline(h=0.1, col="red")
abline(h=0.2, col="chocolate")
abline(h=0.15, col="blue")

### K-means ----
# Método del codo:
wssplot <- function(data, nc=15, seed=9){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group",pch=20)}

wssplot(mat, nc = 20) # nos quedamos con 4.
xsd <- scale(mat)
set.seed(9)
# pongo 4 para que tenga la misma cantidad que jerárquico
km.out <- kmeans(xsd, 4, nstart=20)
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
cmp2 <- data.frame("col"=names(aux))


for (i in 1:max(groups)) {
  a <- round(apply(aux2[groups==i, ], 2, mean), digits=4)
  cmp2 <- cbind(cmp2, a)
}
group <- function(i) {
  return(aux2[groups==i, ])
}

cmp2 <- cmp2[, -1]
colnames(cmp2) <- c('g1', 'g2', 'g3', 'g4')
View(cmp2)

library(gridExtra)
png("mat1.png", width=300, height=200)
p<-tableGrob(cmp2[c("G1", "G2", "G3"), ])
grid.arrange(p)
dev.off()

aux3 <- mat
aux3$cluster <- as.factor(groups)
p <- ggparcoord(data = aux3[, c("G1", "G2", "G3", "cluster")], groupColumn="cluster",
) + labs(x="", y="", title="Clustering")
ggplotly(p)

png("mat2.png", width=300, height=200)
p<-tableGrob(cmp2[c("school", "sex", "address", "famsize", "age", "G3"), ])
grid.arrange(p)
dev.off()
p <- ggparcoord(data = aux3[, c("school", "sex", "address", "famsize", "age", "G3", "cluster")], groupColumn="cluster",
) + labs(x="", y="", title="Clustering")
ggplotly(p)

png("mat3.png", width=300, height=200)
p<-tableGrob(cmp2[c("Medu", "Fedu", "G3"), ])
grid.arrange(p)
dev.off()
p <- ggparcoord(data = aux3[, c("Medu", "Fedu", "G3", "cluster")], groupColumn="cluster",
) + labs(x="", y="", title="Clustering")
ggplotly(p)

png("mat4.png", width=300, height=200)
p<-tableGrob(cmp2[c("studytime", "traveltime", "freetime", "G3"), ])
grid.arrange(p)
dev.off()

png("mat5.png", width=300, height=200)
p<-tableGrob(cmp2[c("Dalc", "Walc", "goout", "G3"), ])
grid.arrange(p)
dev.off()

png("mat6.png", width=300, height=200)
p<-tableGrob(cmp2[c("failures", "absences", "higher", "G3"), ])
grid.arrange(p)
dev.off()

png("mat7.png", width=300, height=200)
p<-tableGrob(cmp2[c("health", "internet", "schoolsup", "G3"), ])
grid.arrange(p)
dev.off()

png("mat_completo.png", width=300, height=800)
p<-tableGrob(cmp2)
grid.arrange(p)
dev.off()
### PCA + jerárquico ----
pr.out <- prcomp(mat, scale=TRUE)
# tendría que ver cuál explica más la varianza, no solo 5
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, main = "Complete Linkage + PCA",
     xlab = "", sub = "", cex = .9, labels=FALSE) # mucho más balanceado.
rect.hclust(hc.out , h = 7, border = 2:15, )
abline(h=9, col="red")
abline(h=8, col="blue")
abline(h=7, col="chocolate")
abline(h=6, col="deepskyblue")

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
