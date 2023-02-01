# Proceso dfs para generar uno solo ----

math <- read.table("student-mat.csv",sep=",",header=TRUE)
por <- read.table("student-por.csv",sep=",",header=TRUE)

common <- merge(d1, d2,
          by=c("school","sex","age","address","famsize","Pstatus","Medu",
               "Fedu","Mjob","Fjob","reason","nursery","internet")
          )
print(nrow(common)) # 382 students

dim(math)
dim(por)
dim(common)

## Veo si las notas de ambos cursos están correlacionadas:
notas_com <- common[, c("G3.x", "G3.y")]
cor(notas_com) # la correlación es aprox. 0.5.

## concateno ambos sets de datos y enchufo una columna para decir si es de mate
## o de portugués:
math["course_type"] <- rep("M", nrow(math))
por["course_type"] <- rep("P", nrow(por))

df <- rbind(math, por)
dim(df)



write.csv(df, "~/Documents/AE/tp2/df.csv", row.names=TRUE)
# LEYENDO DFS E IMPORTANDO FUNCIONES ----
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
  print("MASS, ISLR2, carData, car, boot, leaps, glmnet, tree, randomForest, gbm, BART")
}
load_libraries()

df <- read.table("df.csv", sep="," , header=TRUE)
n <- nrow(df)     # 1044
p <- ncol(df)-1   # 33
