## Fitteo un árbol para cada variable a ver cuál es su split más
## significativo:

# Genero las VAs ----
# 1. G1 y G2:
g1_g2_tree <- tree(G3 ~ G1+G2, data=train)
plot(g1_g2_tree)
text(g1_g2_tree, pretty=0) # G2 < 11.5

# 2. Medu:
medu_tree <- tree(G3 ~ Medu, data=train)
plot(medu_tree)
text(medu_tree, pretty=0) # 1,2,3

# 3. Fedu:
fedu_tree <- tree(G3 ~ Fedu, data=train)
plot(fedu_tree)
text(fedu_tree, pretty=0) # 0,1,2,3

# 4. Mjob:
Mjob_tree <- tree(G3 ~ Mjob, data=train)
plot(Mjob_tree)
text(Mjob_tree, pretty=0) # at_home, other

# 5. Fjob:
Fjob_tree <- tree(G3 ~ Fjob, data=train)
plot(Fjob_tree)
text(Fjob_tree, pretty=0) # at_home, other, services

# 6. reason:
reason_tree <- tree(G3 ~ reason, data=train)
plot(reason_tree)
text(reason_tree, pretty=0) # course, home, other

# 7. guardian:
guardian_tree <- tree(G3 ~ guardian, data=train)
plot(guardian_tree)
text(guardian_tree, pretty=0) # other

# 8. traveltime, studytime, freetime:
time_tree <- tree(G3 ~ traveltime + studytime + freetime, data=train)
plot(time_tree)
text(time_tree, pretty=0) # studytime < 2.5, traveltime < 1.5

# 9. goout:
goout_tree <- tree(G3 ~ goout, data=train)
plot(goout_tree)
text(goout_tree, pretty=0) # goout < 4.5

# 10. Dalc, Walc:
alc <- tree(G3 ~ Dalc+Walc+health, data=train)
plot(alc)
text(alc, pretty=0) # Dalc < 1.5

# 11. absences y G1:
rend <- tree(G3 ~ absences+G1, data=train)
plot(rend)
text(rend, pretty=0) # G1 < 10.5

train$G2_tree <- train$G2 < 11.5
train$Medu_tree <- train$Medu == 1 | train$Medu == 2 | train$Medu == 3
train$Fedu_tree <- train$Fedu == 1 | train$Fedu == 2 | train$Fedu == 3 | train$Fedu == 0
train$reason_tree <- train$reason == "other"
train$Mjob_tree <- train$Mjob %in% c("at_home", "other")
train$Fjob_tree <- train$Fjob %in% c("at_home", "other", "services")
train$studytime_tree <- train$studytime < 2.5
train$traveltime_tree <- train$traveltime < 1.5
train$goout_tree <- train$goout < 4.5
train$Dalc_tree <- train$Dalc < 1.5
train$G1_tree <- train$G1 < 10.5

test$G2_tree <- test$G2 < 11.5
test$Medu_tree <- test$Medu == 1 | test$Medu == 2 | test$Medu == 3
test$Fedu_tree <- test$Fedu == 1 | test$Fedu == 2 | test$Fedu == 3 | test$Fedu == 0
test$reason_tree <- test$reason == "other"
test$Mjob_tree <- test$Mjob %in% c("at_home", "other")
test$Fjob_tree <- test$Fjob %in% c("at_home", "other", "services")
test$studytime_tree <- test$studytime < 2.5
test$traveltime_tree <- test$traveltime < 1.5
test$goout_tree <- test$goout < 4.5
test$Dalc_tree <- test$Dalc < 1.5
test$G1_tree <- test$G1 < 10.5

to_drop <- c("age", "address","Medu", "Fedu", "Mjob",
             "Fjob", "reason", "guardian","traveltime", "studytime",
             "health", "failures", "schoolsup", "internet",
             "goout", "Dalc", "Walc", "absences")
train <- train[, !(names(train) %in% to_drop)]
test <- test[, !(names(test) %in% to_drop)]

# Fitteo modelos ----
## ML
arbol_modelo <- lm(G3 ~ ., data=train)
summary(arbol_modelo)
calcular_ecm(predict(arbol_modelo, test)) # 2.129542

## FS
p <- ncol(train)-1

regfit.fwd <- regsubsets(G3 ~ ., data=train, nvmax=p, method="forward")
k <- 10
set.seed(9)
folds <- sample(rep(1:k, length=n))

cv.errors <- matrix(NA, k, p, 
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(G3 ~ ., data=train[folds != j, ], 
                         nvmax=p, method="forward")
  for (i in 1:p) {
    pred <- predict(best.fit, train[folds == j, ], id=i)
    cv.errors[j, i] <- mean((train$G3[folds == j] - pred)^2)
  }
}

mean.cv.errors.fwd <- apply(cv.errors, 2, mean)
mean.cv.errors.fwd

par(mfrow = c(1, 1))
plot(mean.cv.errors.fwd, type = "b", pch=20)
coef_fwd <- which.min(mean.cv.errors.fwd) # 2
coef(regfit.fwd, coef_fwd) # se queda solo con G1 y G2.

fwd.fit <- lm(G3 ~ G1 + G2, data=train)
y.hat <- predict(fwd.fit, test)
ECM_rl_fwd <- calcular_ecm(y.hat)

### Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(arbol_modelo, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test)
ECM_mix_aic <- calcular_ecm(y.hat) # 2.104369
comparaciones <- agregar_modelo("MIXED selection AIC árbol", ECM_mix_aic)
