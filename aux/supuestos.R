# 1. Linealidad: ----
"
Se chequea viendo si residuos vs Y_sombrero no tiene ningún patrón.
"
plot(lm.fit, 1, pch=20)
# Cumple puesto que no se observa una tendencia significativa de los
# residuos r_i (estimación de los errores Epsilon_i) respecto de cada
# Y_sombrero_i.
termplot(lm.fit, partial.resid=T) # si quisiera ver alguna
# relación no lineal.

# 2. Normalidad: ----
"
Si el supuesto de linealidad u homocedasticidad no se
cumplen => puede que el supuesto de normalidad tampoco.
"

plot(lm.fit, 2, pch=20)
"
Observamos que en los extremos (sobre todo al comienzo), los
datos se apartan mucho de la distribución normal.
Esto seguramente será por los outliers que tenemos, puesto
que en el centro, se ajusta muy bien a la línea que indica
la distribución normal.
"

shapiro.test(lm.fit$residuals) # p-value < 2.2e-16
# Rechaza => No hay normalidad.

## Transformación de Yeo-Johnson:
"
Esta transformación es una generalización de Box-Cox que
sirve para datos con variable a predecir no-negativa. En
nuestro caso, G3 es no negativa puesto que existe la nota 0.

Otra solución es correr una constante todos los datos, entonces
estoy corriendo la media.
"
YJ <- car::powerTransform(lm(G3 ~ ., data=train), family = "yjPower")
(lambdaYJ <- YJ$lambda)

G3_trans <- car::yjPower(U = train$G3, lambda = lambdaYJ)

par(mfrow = c(1, 2))
hist(train$G3, freq = FALSE, breaks = 10, ylim = c(0, 0.3))
hist(G3_trans, freq = FALSE, breaks = 10, ylim = c(0, 0.3))

train2 <- train
train2$G3 <- G3_trans

head(train2)
par(mfrow=c(1,2))
plot(lm.fit, 2, pch=20)
plot(lm(G3 ~ ., data=train2), 2, pch=20)
# notamos menos cantidad de puntos a la izquierda,
# es decir, es ligeramente mejor.

# 3. Homocedasticidad: ----
"
Lo que espero es que en el gráfico de los residuos estandarizados
(escalados a raíz cuadrada), la línea roja que indica la media se
pegue bastante a 1, y que no se exhiban patrones no constantes
(como algo cuadrático por ejemplo).
"
plot(lm.fit, 3, pch=20)
# Está claro que está alrededor del 1 y que 
# no hay patrones no constantes.

# Un test para verificarla:
car::ncvTest(lm.fit)


# Outliers: ----
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
