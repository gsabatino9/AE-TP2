### Selección de modelos ----
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

#### Forward-Selection:
regfit.fwd <- regsubsets(G3 ~ ., data=train, nvmax=p, method="forward")
summary(regfit.fwd)

# Me quedo con el mejor a partir de cv con K=10:
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
comparaciones <- agregar_modelo("FWD selection", ECM_rl_fwd)

#### Backward-Selection:
regfit.bwd <- regsubsets(G3 ~ ., data=train, nvmax=p, method="backward")
summary(regfit.bwd)

# Me quedo con el mejor a partir de cv con K=10:
set.seed(9)
folds <- sample(rep(1:k, length=n))

cv.errors <- matrix(NA, k, p, 
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(G3 ~ ., data=train[folds != j, ], 
                         nvmax=p, method="backward")
  for (i in 1:p) {
    pred <- predict(best.fit, train[folds == j, ], id=i)
    cv.errors[j, i] <- mean((train$G3[folds == j] - pred)^2)
  }
}

mean.cv.errors.bwd <- apply(cv.errors, 2, mean)
mean.cv.errors.bwd

par(mfrow = c(1, 1))
plot(mean.cv.errors.bwd, type = "b", pch=20)
coef_bwd <- which.min(mean.cv.errors.bwd) # 2
coef(regfit.bwd, coef_bwd) # se queda solo con G1 y G2.

ECM_rl_bwd <- ECM_rl_fwd
comparaciones <- agregar_modelo("BWD selection", ECM_rl_bwd)

#### Mixed-Selection:
regfit.mix <- regsubsets(G3 ~ ., data=train, nvmax=p, method="seqrep")
summary(regfit.mix)

# Me quedo con el mejor a partir de cv con K=10:
set.seed(9)
folds <- sample(rep(1:k, length=n))

cv.errors <- matrix(NA, k, p, 
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(G3 ~ ., data=train[folds != j, ], 
                         nvmax=p, method="seqrep")
  for (i in 1:p) {
    pred <- predict(best.fit, train[folds == j, ], id=i)
    cv.errors[j, i] <- mean((train$G3[folds == j] - pred)^2)
  }
}

mean.cv.errors.mix <- apply(cv.errors, 2, mean)

par(mfrow = c(1, 1))
plot(mean.cv.errors.mix, type = "b", pch=20)
coef_mix <- which.min(mean.cv.errors.mix) # 2
coef(regfit.mix, coef_mix) # se queda solo con G1 y G2.

ECM_rl_mix <- ECM_rl_fwd
comparaciones <- agregar_modelo("MIXED selection", ECM_rl_mix)

### Mixed-Selection por AIC
set.seed(9)
step.model <- stepAIC(new_model, direction="both", trace=FALSE)
summary(step.model)

y.hat <- predict(step.model, test)
ECM_mix_aic <- calcular_ecm(y.hat)
comparaciones <- agregar_modelo("MIXED selection AIC", ECM_mix_aic)

### RIDGE ----
x.train <- model.matrix(G3 ~ ., train)[, -1]
y.train <- train$G3
x.test <- model.matrix(G3 ~ ., test)[, -1]
y.test <- test$G3

grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x.train, y.train, alpha=0, lambda=grid, thresh=1e-12)

set.seed(9)
cv.out <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.out)
best_lambda <- cv.out$lambda.min

ridge.pred <- predict(ridge.mod, s=best_lambda, newx=x.test)
ECM_ridge <- calcular_ecm(ridge.pred)
comparaciones <- agregar_modelo("Ridge", ECM_ridge)

### LASSO ----
lasso.mod <- glmnet(x.train, y.train, alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(9)
cv.out <- cv.glmnet(x.train, y.train, alpha=1)
plot(cv.out)

best_lambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=best_lambda, newx=x.test)
ECM_lasso <-calcular_ecm(lasso.pred)
comparaciones <- agregar_modelo("Lasso", ECM_lasso)

### Árbol de decisión sin G1 y G2 ----
longtree.fit <- tree(G3 ~ .-G1-G2, train,
                     control=tree.control(nobs = nrow(train), mindev = 0))
summary(longtree.fit)
### Árbol podado ----
prunning.fit <- prune.tree(longtree.fit, best=5)
plot(prunning.fit)
text(prunning.fit, pretty=0)

### Uso esa info para generar nuevas columnas ----
train$failures_tree <- train$failures == 0
train$absences_tree <- train$absences == 0
train$health_tree <- train$health < 5
train$age_tree <- train$age < 17

test$failures_tree <- test$failures == 0
test$absences_tree <- test$absences == 0
test$health_tree <- test$health < 5
test$age_tree <- test$age < 17

longtree.fit2 <- tree(G3 ~ .
                     -G1-G2, train,
                     control=tree.control(nobs = nrow(train), mindev = 0))
prunning.fit2 <- prune.tree(longtree.fit2, best=5)
plot(prunning.fit2)
text(prunning.fit2, pretty=0)

new_model2 <- lm(G3 ~ ., data=train)
summary(new_model2)
calcular_ecm(predict(new_model2, test)) # 2.213529 -> tremendo
# xq es con todas las variables.

## Qué pasa si ahora tiro las que no son buenas?
new_model3 <- lm(G3 ~ .
                 -age-address-Medu-Fedu-Mjob-Fjob
                 -reason-guardian-traveltime-studytime
                 -health
                 , data=train)
summary(new_model3)
calcular_ecm(predict(new_model3, test))
