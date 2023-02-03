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

################################### probando quitar algunas categoricas!
modelo <- lm(G3 ~ ., data=train)

train_new <- train
test_new <- test
train_new$agebest <- ifelse(train_new$age == 20 & train_new$course_type == 'M', 1, 0)
test_new$agebest <- ifelse(test_new$age == 20 & test_new$course_type == 'M', 1, 0)
modelo_new <- lm(G3 ~ ., data=train_new)

train_new$sexbest <- ifelse((train_new$sex == 'F' & train_new$course_type == 'P') | (train_new$sex == 'M' & train_new$course_type == 'M'), 1, 0)
test_new$sexbest <- ifelse((test_new$sex == 'F' & test_new$course_type == 'P') | (test_new$sex == 'M' & test_new$course_type == 'M'), 1, 0)
modelo_new2 <- lm(G3 ~ ., data=train_new)

train_new$FM_edu <- as.numeric(train_new$Fedu) + as.numeric(train_new$Medu) - 2
test_new$FM_edu <- as.numeric(test_new$Fedu) + as.numeric(test_new$Medu) - 2
train_new <- train_new[,!names(train_new) %in% c("Fedu","Medu")]
test_new <- test_new[,!names(test_new) %in% c("Fedu","Medu")]
modelo_new3 <- lm(G3 ~ ., data=train_new)

calcular_ecm(predict(modelo, test))
calcular_ecm(predict(modelo_new, test_new))
calcular_ecm(predict(modelo_new2, test_new))
calcular_ecm(predict(modelo_new3, test_new))

