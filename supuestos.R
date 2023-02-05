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