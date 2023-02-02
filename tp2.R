# Lectura de datos ----
## Me quedo con portugués como train y matemática como test.
train <- read.table("student-por.csv",sep=",",header=TRUE)
test <- read.table("student-mat.csv",sep=",",header=TRUE)

dim(train) # 649 obs. -> el 62% de observaciones totales.
dim(test) # 395 obs.

n <- nrow(train)
p <- ncol(train)-1

# Análisis de las variables ----
# Fitteo de modelos ----
## Regresión lineal ----