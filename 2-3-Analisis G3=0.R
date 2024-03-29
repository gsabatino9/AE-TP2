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
mat <- read.table("student-mat.csv",sep=",",header=TRUE)
por <- read.table("student-por.csv",sep=",",header=TRUE)

mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(mat[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)
por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)] <- 
  lapply(por[-c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)], factor)


aux.mat <- mat
aux.por <- por

mat <- mat[,-c(31,32,33)]
por <- por[,-c(31,32,33)]

mat <- data.frame(lapply(mat, as.numeric))
por <- data.frame(lapply(por, as.numeric))

# K-Means ----
wssplot <- function(data, nc=15, seed=9) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group",pch=20)
}

wssplot(mat, nc = 20) # nos quedamos con 3.
set.seed(9)
km.out <- kmeans(mat, 3, nstart=20)
groups <- km.out$cluster
table(groups)

mat$group <- groups

mat$G3 <- aux.mat$G3
View(mat[mat$group==1, ])
View(mat[mat$group==2, ])
View(mat[mat$group==3, ])
View(mat[mat$group==4, ])
library(psych)
describeBy(mat$G3, mat$group, mat = T, digits = 2)

# Jerárquico ----
hc.complete <- hclust(dist(mat), method="complete")
plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9, label=F)
abline(h=20, col="red")
abline(h=13, col="blue")

groups <- cutree(hc.complete, h=13)
mat$group <- groups
mat$G3 <- aux.mat$G3

describeBy(mat$G3, mat$group, mat = T, digits = 2)

# Itero sobre el grupo donde están todos
aux <- mat[mat$group == 2, ]
aux <- aux[, -32]

hc.complete <- hclust(dist(aux), method="complete")
plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub = "", cex = .9, label=F)
abline(h=7, col="blue")

groups <- cutree(hc.complete, h=7)
aux$group <- groups
aux$G3 <- mat[mat$group == 2, ]$G3

describeBy(aux$G3, aux$group, mat = T, digits = 2)

"
Conclusión: No hay un patrón para los del grupo 0.
"
