"
1. Los errores tienen media 0. Supuesto de linealidad:
  Significancia del test (cumplimos).
  
2. Todos los errores tienen la misma varianza (constante) = sigmna^2. 
Homocedasticidad:
  Graficamos residuos vs predichos y no tenemos que observar patrones,
  ie, nube heterogénea.
  
3. Los errores son indeptes entre sí -> No lo podemos testear.

4. Los errores tienen distribución normal:
  Shapiro test.
"

############# 1.
modelo <- lm(G3 ~ G1 + G2 + course_type, data=train)
summary(modelo) # cumple

residuos <- modelo$residuals
mean(residuos)



############# 2.
lm.fit <- lm(G3 ~ ., data=train)
plot(modelo$fitted.values, residuos, pch=20)
plot(lm.fit$fitted.values, lm.fit$residuals, pch=20)

# stand
train_stand <- train %>% mutate_at(c('G1', 'G2', 'G3'), ~(scale(.) %>% as.vector))
test_stand <- test %>% mutate_at(c('G1', 'G2', 'G3'), ~(scale(.) %>% as.vector))
modelo_stand <- lm(G3 ~ G1 + G2 + course_type, data=train_stand)
rstud_stand <- rstudent(modelo_stand)
plot(modelo_stand$fitted.values, rstud_stand, pch=20)

#boxcox

train$G3[train$G3 == 0] <- 0.00001

bc<-boxcox(train$G3 ~ train$G1 + train$G2 + as.numeric(train$course_type))

#train$G1[train$G1 == 0] <- 0.00001
#train$G2[train$G2 == 0] <- 0.00001

lamda<-bc$x[which.max(bc$y)]
g3_boxcox<-(train$G3^lamda-1)/lamda
reg2<-lm(g3_boxcox ~ train$G1 + train$G2 + train$course_type)
plot(reg2$fitted.values, reg2$residuals, pch=20)

shapiro.test(reg2$residuals)

#################### quito outlier

res_stud<-rstudent(modelo)
plot(res_stud,pch=20)
abline(h=2.5)

sospechosos <- as.numeric(names(res_stud[abs(res_stud) > 2.5]))

lm.fit1 <- lm(G3 ~ G1+G2+course_type, data=train[!(as.numeric(row.names(train)) %in% sospechosos), ])

res_stud<-rstudent(lm.fit1)
plot(res_stud,pch=20)
abline(h=2.5)
shapiro.test(lm.fit1$residuals)

sospechosos2 <- as.numeric(names(res_stud[abs(res_stud) > 2.5]))

train_out <- train[!(as.numeric(row.names(train)) %in% sospechosos2) & !(as.numeric(row.names(train)) %in% sospechosos), ]
lm.fit2 <- lm(G3 ~ G1+G2+course_type, data=train_out)

res_stud<-rstudent(lm.fit2)
plot(res_stud,pch=20)
abline(h=2.5)
shapiro.test(lm.fit1$residuals)

## tiro box donde quitamos outliers:

train_out$G3[train_out$G3 == 0] <- 0.00001

bc<-boxcox(train_out$G3 ~ train_out$G1 + train_out$G2 + as.numeric(train_out$course_type))

lamda<-bc$x[which.max(bc$y)]
g3_boxcox<-(train_out$G3^lamda-1)/lamda
reg2<-lm(g3_boxcox ~ train_out$G1 + train_out$G2 + train_out$course_type)
plot(reg2$fitted.values, reg2$residuals, pch=20)
summary(reg2)


## INVESTIGAR POR QUE NO PUEDO CALCULAR EL ECM CON EL BOXCOX!!
test_boxcox <- test[, c('G1', 'G2', 'G3', 'course_type')]
test_boxcox[, 'course_type'] <- as.numeric(test_boxcox[, 'course_type'])
test_boxcox

length(predict(reg2, test_boxcox[, c('G1', 'G2', 'G3', 'course_type')]))
mean((predict(reg2, test) - test$G3)^2)

## 4.


residuos_boxcox <- reg2$residuals
qqnorm(residuos_boxcox, pch=20)
qqline(residuos_boxcox, lwd=3, col="red")

plot(reg2$fitted.values,rstudent(reg2), pch=20,col="darkblue",xlab="valor ajustado", 
     ylab="residuo estandarizado")
abline(h=0)

shapiro.test(residuos_boxcox) # no cumple!!!!!!! pero por que el qqnorm si?

#####################################################