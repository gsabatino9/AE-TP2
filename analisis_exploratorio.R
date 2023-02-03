## Análisis exploratorio de variables
por[-c(3,13,14,15,30,31,32,33,21,23,24,25)] <- 
  lapply(por[-c(3,13,14,15,30,31,32,33,21,23,24,25)], factor)
mat[-c(3,13,14,15,30,31,32,33,21,23,24,25)] <- 
  lapply(mat[-c(3,13,14,15,30,31,32,33,21,23,24,25)], factor)


por$Dalc <- as.numeric(por$Dalc)
por$Walc <- as.numeric(por$Walc)

mat$Dalc <- as.numeric(mat$Dalc)
mat$Walc <- as.numeric(mat$Walc)

train$freetime <- as.numeric(train$freetime)
train$famrel <- as.numeric(train$famrel)
train$health <- as.numeric(train$health)

test$freetime <- as.numeric(test$freetime)
test$famrel <- as.numeric(test$famrel)
test$health <- as.numeric(test$health)

library(GGally)
ggpairs(train[, c("G1", "G2", "G3", "absences", "failures")])

ggplot(train, aes(x = G2, y = G3)) +
  geom_point() +
  xlab("G2") +
  ylab("G3") +
  ggtitle("G2 vs G3") +
  theme_light()

ggplot(train, aes(x = G1, y = G3)) +
  geom_point() +
  xlab("G1") +
  ylab("G3") +
  ggtitle("G1 vs G3") +
  theme_light()

train2 <- map_df(train, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

library(psych)
corPlot(train2[, c("G1", "G2", "G3", "failures", "absences", "Dalc", "Walc",
                   "studytime", "activities", "schoolsup")],
        min.length = 3)

corPlot(train2[, c("G1", "G2", "G3", "sex", "age", "Fedu", "Medu")],
        min.length = 3)

any(is.na(train))
apply(is.na(train), 2, sum)

str(train)
