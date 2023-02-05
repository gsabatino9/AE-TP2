## Puedo fittear modelos y sacar las variables significativas
## para cada modelo.
## Acá voy a empezar sacando:
## G1, G2, absences, failures, course_type.

train$freetime <- as.numeric(train$freetime)
train$famrel <- as.numeric(train$famrel)
train$goout <- as.numeric(train$goout)
train$Dalc <- as.numeric(train$Dalc)
train$Walc <- as.numeric(train$Walc)
train$health <- as.numeric(train$health)

test$freetime <- as.numeric(test$freetime)
test$famrel <- as.numeric(test$famrel)
test$goout <- as.numeric(test$goout)
test$Dalc <- as.numeric(test$Dalc)
test$Walc <- as.numeric(test$Walc)
test$health <- as.numeric(test$health)

# Analizo con modelo lineal cada una por separado ----
# 1. School:
school <- lm(G3 ~ school, data=train)
summary(school) # 0.001018

# 2. Sex:
sex <- lm(G3 ~ sex, data=train)
summary(sex) # 0.9822

# 3. address:
address <- lm(G3 ~ address, data=train)
summary(address) # 0.0002915

# 4. famsize:
famsize <- lm(G3 ~ famsize, data=train)
summary(famsize) # 0.2398

# 5. Pstatus:
Pstatus <- lm(G3 ~ Pstatus, data=train)
summary(Pstatus) # 0.3585

# 6. Medu:
Medu <- lm(G3 ~ Medu, data=train)
summary(Medu) # 1.4e-07

# 7. Fedu:
Fedu <- lm(G3 ~ Fedu, data=train)
summary(Fedu) # 9.549e-05

# 8. Mjob:
Mjob <- lm(G3 ~ Mjob, data=train)
summary(Mjob) # 0.0001244

# 9. Fjob:
Fjob <- lm(G3 ~ Fjob, data=train)
summary(Fjob) # 0.02104

# 10. reason:
reason <- lm(G3 ~ reason, data=train)
summary(reason) # 0.01054

# 11. guardian:
guardian <- lm(G3 ~ guardian, data=train)
summary(guardian) # 0.01098

# 12. traveltime:
traveltime <- lm(G3 ~ traveltime, data=train)
summary(traveltime) # 0.007925

# 13. freetime:
freetime <- lm(G3 ~ freetime, data=train)
summary(freetime) # 0.06549

# 14. studytime:
studytime <- lm(G3 ~ studytime, data=train)
summary(studytime) # 0.00011

# 15. schoolsup:
schoolsup <- lm(G3 ~ schoolsup, data=train)
summary(schoolsup) # 0.01827

# 16. famsup:
famsup <- lm(G3 ~ famsup, data=train)
summary(famsup) # 0.5534

# 17. paid:
paid <- lm(G3 ~ paid, data=train)
summary(paid) # 0.1035

# 18. activities:
activities <- lm(G3 ~ activities, data=train)
summary(activities) # 0.7379

# 19. nursery:
nursery <- lm(G3 ~ nursery, data=train)
summary(nursery) # 0.1673

# 20. higher:
higher <- lm(G3 ~ higher, data=train)
summary(higher) # 1.191e-07

# 21. internet:
internet <- lm(G3 ~ internet, data=train)
summary(internet) # 0.002842

# 22. romantic:
romantic <- lm(G3 ~ romantic, data=train)
summary(romantic) # 0.01542

# 23. famrel:
famrel <- lm(G3 ~ famrel, data=train)
summary(famrel) # 0.6447

# 24. goout:
goout <- lm(G3 ~ goout, data=train)
summary(goout) # 0.01057

# 25. Dalc:
Dalc <- lm(G3 ~ Dalc, data=train)
summary(Dalc) # 0.0007654

# 26. Walc:
Walc <- lm(G3 ~ Walc, data=train)
summary(Walc) # 0.00205

# 27. health:
health <- lm(G3 ~ health, data=train)
summary(health) # 0.01414

# Empiezo quitando las que dan alto ----
"
Se puede generar una columna analizando a mano los features,
o haciendo un PCA solo en esas columnas.
"
train <- train[, !(names(train) %in% c("sex", "famsize",
                                              "Pstatus", "freetime",
                                              "famsup", "paid", "activities",
                                              "nursery","famrel"))]
test <- test[, !(names(test) %in% c("sex", "famsize",
                                       "Pstatus", "freetime",
                                       "famsup", "paid", "activities",
                                       "nursery","famrel"))]

new_model <- lm(G3 ~ ., data=train)

calcular_ecm(predict(new_model, test)) # 2.319875 -> ya lo mejora.
