"
Supuestos:
1. Los errores tienen media 0. Supuesto de linealidad:
  Significancia del test.
  
2. Todos los errores tienen la misma varianza (constante) = sigmna^2. 
Homocedasticidad:
  Graficamos residuos vs predichos y no tenemos que observar patrones,
  ie, nube heterogénea.
  
3. Los errores son indeptes entre sí.

4. Los errores tienen distribución normal:
  Shapiro test y qqplot con los residuos estudentizados.
"

# 1. Significancia del test ----
modelo <- lm(G3 ~ ., data=train)
summary(modelo) 
# La regresión es significativa: p-value: < 2.2e-16.

residuos <- modelo$residuals
mean(residuos)
# Los errores tienden a 0 en promedio.

# 2. Supuesto de homocedasticidad ----
plot(modelo$fitted.values, residuos, pch=20, col="deepskyblue")
abline(h=0, lwd=2) 
# no se observa un patrón de puntos, más allá de ciertos
# outliers.

# 3. Independencia de los errores ----
"
Lo suponemos, puesto que no es una serie temporal.
"

# 4. Los errores tienen distribución iid normal ----
"
Tenemos que graficar el qqnorm de los residuos estudentizados
y ver que sus colas son simétricas y no son pesadas.

Esto lo validaremos en el mejor modelo, puesto que tenemos
outliers que no nos permiten identificar bien ésto, pero
que nos servirá tener menos cantidad de columnas para identificarlos.
"
rest<-rstandard(modelo)
rstud<-rstudent(modelo)

par(mfrow=c(2,2))
qqnorm(residuos, pch=20)
qqline(residuos, lwd=2, col="red")

qqnorm(rest, pch=20)
qqline(rest, col="red")

qqnorm(rstud, pch=20)
qqline(rstud, col="red")

plot(modelo$fitted.values, rstud, pch=20,
     col="darkblue",xlab="valor ajustado", 
     ylab="residuo estudientizado")
abline(h=0, lwd=2)

#### Valido supuestos con el mejor modelo lineal
rest<-rstandard(step.model)
rstud<-rstudent(step.model)

par(mfrow=c(2,2))
qqnorm(residuos, pch=20)
qqline(residuos, lwd=2, col="red")

qqnorm(rest, pch=20)
qqline(rest, col="red")

qqnorm(rstud, pch=20)
qqline(rstud, col="red")

plot(step.model$fitted.values, rstud, pch=20,
     col="darkblue",xlab="valor ajustado", 
     ylab="residuo estudientizado")
abline(h=0, lwd=2)

cooksd <- cooks.distance(step.model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),
                   names(cooksd),""), col="red")

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])

View(train[row.names(train) %in% influential, ]) # se ve
# que, por lo general, los outliers son los que tienen nota G1 > 0,
# G2 > 0 y G3 == 0.

outliers <- car::outlierTest(step.model)
outliers[1] # c(914, 984, 946, 909, 173, 587, 960, 966, 983, 640)
View(train[row.names(train) %in% c(914, 984, 946, 909, 173, 587, 960, 966, 983, 640), ])
