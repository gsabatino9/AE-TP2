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

# 1.
modelo <- lm(G3 ~ G1 + G2 + course_type, data=train)
summary(modelo) # cumple


# 2.
residuos <- modelo$residuals
rstud <- rstudent(modelo)

qqnorm(rstud, pch=20)
qqline(rstud, lwd=3, col="red")

sospechosos <- as.numeric(names(rstud[abs(rstud) > 2.5]))

lm.fit1 <- lm(G3 ~ G1+G2+course_type, data=train[!(as.numeric(row.names(train)) %in% sospechosos), ])
rstud1 <- rstudent(lm.fit1)
length(names(rstud1[abs(rstud1) > 2.5]))

qqnorm(rstud1, pch=20)
qqline(rstud1, col="red", lwd=3)

calcular_ecm(predict(modelo, test))

hats <- as.data.frame(hatvalues(modelo))
length(hats[hats[, 1] > (4)/n, ])

### Shapiro-test
rest <- rstandard(modelo)
qqnorm(rest, pch=20)
qqline(rest, lwd=3, col="red")

shapiro.test(rest)
