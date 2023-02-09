# Menos significativas
sacar <- c("famsize", "Pstatus", "Fjob", "guardian",
           "schoolsup", "famsup", "paid", "activities",
           "nursery", "famrel", "goout", "absences")

train2 <- train[, !(names(train) %in% sacar)]
test2 <- test[, !(names(test) %in% sacar)]

# Dejando las mas significativas:
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #0.9661508 : "Quitando VAs rl simple"

# Cols generadas de árboles: 0.9697322 ECM
# ... deberiamos buscar mejor del 0.9661508 ECM

# Con las menos significativas el modelo de regresion lineal no es tan bueno..
nuevo_modelo2 <- lm(G3 ~ famsize + Pstatus + Fjob + guardian +
              schoolsup + famsup + paid + activities + 
              nursery + famrel + goout + absences,
            data=train)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test)) # 7.720118

# Encontrando relaciones entre mas de una feature con un arbol de decision usando las 
# features menos significativas:
aux <- tree(G3 ~ famsize + Pstatus + Fjob + guardian +
              schoolsup + famsup + paid + activities + 
              nursery + famrel + goout + absences, 
            data=train)
plot(aux)
text(aux, pretty=0)
# goout < 4.5
    # Fjob: services
    # goout < 1.5

boxplot( train$G3 ~ train$goout < 4.5 & train$Fjob == "services")
boxplot( train$G3 ~ train$goout < 1.5)

train2$goout_Fjob <- train$goout < 4.5 & train$Fjob == "services"
test2$goout_Fjob <- test$goout < 4.5 & test$Fjob == "services"
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) # 0.9662724

train2$goout_Fjob <- train$goout < 1.5
test2$goout_Fjob <- test$goout < 1.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #0.9661017
# ninguna significativa, no nos sirve!

train2 <- train2[, !names(train2) %in% c("goout_Fjob")]
test2 <- test2[, !names(test2) %in% c("goout_Fjob")]

# Busquemos mas combinaciones pero quitando las feature mas importante usada de "goout"
aux <- tree(G3 ~ famsize + Pstatus + Fjob + guardian +
              schoolsup + famsup + paid + activities + 
              nursery + famrel + absences, 
            data=train)
plot(aux)
text(aux, pretty=0)
# Fjob: at_home, other, services
    # activies: no
    # famrel < 2.5

boxplot( train$G3 ~ train$Fjob %in% c("at_home", "other", "services") & train$activities == "no")
boxplot( train$G3 ~ (!(train$Fjob %in% c("at_home", "other", "services") )) & train$famrel < 2.5)
# el seugndo boxplot parece interesante...

train2$fjob_famrel <- train$Fjob %in% c("at_home", "other", "services") & train$activities == "no"
test2$fjob_famrel <- test$Fjob %in% c("at_home", "other", "services") & test$activities == "no"
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) # 0.9671071

train2$fjob_famrel <- (!(train$Fjob %in% c("at_home", "other", "services") )) & train$famrel < 2.5
test2$fjob_famrel <- (!(test$Fjob %in% c("at_home", "other", "services") )) & test$famrel < 2.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) # 0.968517

train2 <- train2[, !names(train2) %in% c("fjob_famrel")]
test2 <- test2[, !names(test2) %in% c("fjob_famrel")]

# En ningun caso nos dio significativa, quitamosla aun asi del proximo arbol a analizar:

aux <- tree(G3 ~ famsize + Pstatus + guardian +
              schoolsup + famsup + paid + activities + 
              nursery + famrel + absences, 
            data=train)
plot(aux)
text(aux, pretty=0)
# famrel < 3.5 

boxplot( train$G3 ~ train$famrel < 3.5)

train2$famrel <-train$famrel < 3.5
test2$famrel <- test$famrel < 3.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #0.9664904


train2 <- train2[, !names(train2) %in% c("famrel")]
test2 <- test2[, !names(test2) %in% c("famrel")]
# no mejoro! ,sigamos quitando

aux <- tree(G3 ~ famsize + Pstatus + guardian +
              schoolsup + famsup + paid + activities + 
              nursery + absences, 
            data=train)
plot(aux)
text(aux, pretty=0)

# de nuevo mismo error!! 
# y no hay ninguna feature combinada improtante que de una mejora significativa para los estudiante de portugues


