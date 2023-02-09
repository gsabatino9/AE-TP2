# Menos significativas
sacar <- c("school", "sex", "address", "famsize", "Pstatus", 
           "Medu", "Mjob", "Fjob", "reason", "guardian", 
           "traveltime", "freetime", "schoolsup", "famsup", 
           "paid", "activities", "nursery", "internet", 
           "famrel", "Dalc", "Walc", "health", "absences")

train2 <- train[, !(names(train) %in% sacar)]
test2 <- test[, !(names(test) %in% sacar)]

# Dejando las mas significativas:
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #3.144766 : "Quitando VAs rl simple"

# Cols generadas de árboles: 2.665349 ECM
# Deberiamos buscar alguna relacion de features que mejore la obtenida con las columnas generadas
# por los arboles usando una sola feature

# Con las menos significativas el modelo de regresion lineal no es viable, p-value mayor 0.05:
nuevo_modelo2 <- lm(G3 ~ school + sex + address + famsize + Pstatus + 
              Medu + Mjob + Fjob + reason + guardian + 
              traveltime + freetime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Dalc + Walc + health + absences, 
            data=train)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test)) # 18.70798 

# Encontrando relaciones entre mas de una feature con un arbol de decision usando las 
# features menos significativas:
aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Medu + Mjob + Fjob + reason + guardian + 
              traveltime + freetime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Dalc + Walc + health + absences, 
            data=train)
plot(aux)
text(aux, pretty=0)
# absences < 0.5
    # schoolsup: no
    # famrel < 3.5

boxplot( train$G3 ~ train$absences < 0.5 & train$schoolsup == "no")
boxplot( train$G3 ~ train$absences > 0.5 & train$famrel > 3.5)
# Parecerise que fuese significativa la de absences < 0.5 & schoolsup == "no"

train2$absences_schoolsup_famrel <- train$absences < 0.5 & train$famrel > 3.5
test2$absences_schoolsup_famrel <- test$absences < 0.5 & test$famrel > 3.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) # 2.881628

train2$absences_schoolsup_famrel <- train$absences < 0.5 & train$schoolsup == "no"
test2$absences_schoolsup_famrel <- test$absences < 0.5 & test$schoolsup == "no"
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #2.617654
# Mejoro mucho, dicha combinacion de feature es significativa para la regresion inclusive.

# Busquemos mas combinaciones pero quitando las feature mas importante usada de "absences"
aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Medu + Mjob + Fjob + reason + guardian + 
              traveltime + freetime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Dalc + Walc + health, 
            data=train)
plot(aux)
text(aux, pretty=0)
# Medu: 1, 2, 3
    # Dalc < 1.5
    # famsup: no

boxplot( train$G3 ~ train$Medu %in% c(1,2,3) & train$Dalc < 1.5)
boxplot( train$G3 ~ (!(train$Medu %in% c(1,2,3))) & train$famsup == "no")
# no parecerise que fuese significativa.

train2$Medu_Dalc_famsup <- train$Medu %in% c(1,2,3) & train$Dalc < 1.5
test2$Medu_Dalc_famsup <- test$Medu %in% c(1,2,3) & test$Dalc < 1.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) # 2.62091

train2$Medu_Dalc_famsup <- !(train$Medu %in% c(1,2,3)) & train$famsup == "no"
test2$Medu_Dalc_famsup <- !(test$Medu %in% c(1,2,3)) & test$famsup == "no"
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) # 2.629229

train2 <- train2[, !names(train2) %in% c("Medu_Dalc_famsup")]
test2 <- test2[, !names(test2) %in% c("Medu_Dalc_famsup")]

# En ningun caso nos dio significativa, quitamosla aun asi del proximo arbol a analizar:

aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Mjob + Fjob + reason + guardian + 
              traveltime + freetime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Dalc + Walc + health, 
            data=train)
plot(aux)
text(aux, pretty=0)
# Mjob: at_home, other
    # Dalc < 1.5
    # famsup: no

boxplot( train$G3 ~ train$Mjob %in% c("at_home", "other") & train$Dalc < 1.5)
boxplot( train$G3 ~ (!(train$Mjob %in% c("at_home", "other"))) & train$famsup == "no")

train2$Mjob_Dalc_famsup <- train$Mjob %in% c("at_home", "other") & train$Dalc < 1.5
test2$Mjob_Dalc_famsup <- test$Mjob %in% c("at_home", "other") & test$Dalc < 1.5 
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #2.663229

train2$Mjob_Dalc_famsup <- (!(train$Mjob %in% c("at_home", "other"))) & train$famsup == "no"
test2$Mjob_Dalc_famsup <- (!(test$Mjob %in% c("at_home", "other"))) & test$famsup == "no"
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #2.603009
# hay una mejora, quedamos con la segunda.

# ahora quitamos la feature "Mjob" del arbol:
aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Fjob + reason + guardian + 
              traveltime + freetime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Dalc + Walc + health, 
            data=train)
plot(aux)
text(aux, pretty=0)
# freetime < 2.5
    # famrel < 3.5
    # activities: no

boxplot( train$G3 ~ train$freetime < 2.5 & train$famrel < 3.5)
boxplot( train$G3 ~ train$freetime > 2.5 & train$activities == "no")

train2$freetime_activities_famrel <- train$freetime < 2.5 & train$famrel < 3.5
test2$freetime_activities_famrel <- test$freetime < 2.5 & test$famrel < 3.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #2.604452

train2$freetime_activities_famrel <- train$freetime > 2.5 & train$activities == "no"
test2$freetime_activities_famrel <- test$freetime > 2.5 & test$activities == "no" 
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #2.567217
# hay una mejora, quedamos con la segunda.

# sigamos quitando del arbol:

aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Fjob + reason + guardian + 
              traveltime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Dalc + Walc + health, 
            data=train)
plot(aux)
text(aux, pretty=0)
# Dalc < 1.5
    # sex: F
    # health < 1.5

boxplot( train$G3 ~ train$Dalc < 1.5 & train$health < 1.5)
boxplot( train$G3 ~ train$Dalc > 1.5 & train$sex == "F")

train2$dalc_Sex_heal <- train$Dalc < 1.5 & train$health < 1.5
test2$dalc_Sex_heal <- test$Dalc < 1.5 & test$health < 1.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2) 
calcular_ecm(predict(nuevo_modelo2, test2)) # 2.563637


train2$dalc_Sex_heal <- train$Dalc > 1.5 & train$sex == "F"
test2$dalc_Sex_heal <- test$Dalc > 1.5 & test$sex == "F"
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) # 2.527964
# Buenisima mejora, sigamos:

aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Fjob + reason + guardian + 
              traveltime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Walc + health, 
            data=train)
plot(aux)
text(aux, pretty=0)
# health < 1.5
    # health < 4.5

train2$health_traveltime_Fjob <- train$health < 1.5
test2$health_traveltime_Fjob <- test$health < 1.5 
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #2.531371


train2$health_traveltime_Fjob <- train$health < 4.5
test2$health_traveltime_Fjob <- test$health < 4.5 
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
summary(nuevo_modelo2)
calcular_ecm(predict(nuevo_modelo2, test2)) #2.674182
# horrible, no nos sirve.

train2 <- train2[, !names(train2) %in% c("health_traveltime_Fjob")]
test2 <- test2[, !names(test2) %in% c("health_traveltime_Fjob")]


aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Fjob + reason + guardian + 
              traveltime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Walc, 
            data=train)
plot(aux)
text(aux, pretty=0)
# reason: course, home
    # Walc < 1.5

train2$reason_Walc <- (train$reason == "course" | train$reason == "home") & train$Walc < 1.5
test2$reason_Walc <- (test$reason == "course" | test$reason == "home") & test$Walc < 1.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
calcular_ecm(predict(nuevo_modelo2, test2))  # 2.527311

#2.527964 vs  2.527311..es unaleve mejora insignificativa, no la tendremos en cuenta.
train2 <- train2[, !names(train2) %in% c("reason_Walc")]
test2 <- test2[, !names(test2) %in% c("reason_Walc")]


aux <- tree(G3 ~ school + sex + address + famsize + Pstatus + 
              Fjob + guardian + 
              traveltime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Walc, 
            data=train)
plot(aux)
text(aux, pretty=0)
# sex: F
    # Walc < 1.5
    # famsize: GT3

train2$sex_famsize_Walc <- train$sex == "F" & train$Walc < 1.5
test2$sex_famsize_Walc <- test$sex == "F" & test$Walc < 1.5
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
calcular_ecm(predict(nuevo_modelo2, test2))  # 2.52594

train2$sex_famsize_Walc <- train$sex != "F" & train$famsize == "GT3"
test2$sex_famsize_Walc <- test$sex != "F" & test$famsize == "GT3"
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
calcular_ecm(predict(nuevo_modelo2, test2))  # 2.521212

# me quedo con esta ultima! 
 ## #2.527964 vs 2.521212

aux <- tree(G3 ~ school + address + famsize + Pstatus + 
              Fjob + guardian + 
              traveltime + schoolsup + famsup + 
              paid + activities + nursery + internet + 
              famrel + Walc, 
            data=train)
plot(aux)
text(aux, pretty=0)
# Error in text.tree(aux, pretty = 0) : cannot plot singlenode tree
# y ya no tiene sentido seguir, porque el arbol no tiene forma de seguir diviendo los datos.
# es decir, estas son las peores variables que tiene un arbol de deceision para nuestro dataset.
# ya no puede seguir podando el arbol


## en conclusion nos quedan:
summary(nuevo_modelo2)
names(train2[, 11:15])
nuevo_modelo2 <- lm(G3 ~ ., data=train2)
calcular_ecm(predict(nuevo_modelo2, test2))  # 2.521212
