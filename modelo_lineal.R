"
Pasos del modelo lineal:
1. Estimar Beta_i y sigma^2.
2. Hacer inferencia.
3. Validar supuestos.
4. Predecir.
5. Identificación de datos atípicos.
6. Selección de modelos óptimos.

Iterar.
"

"
Preguntas a responder después de obtener el mejor modelo:

1. Hay alguna relación entre Y y las VAs predictoras?
Para esto sirve el test de hipótesis con H0: B_1 = ... = B_p = 0.
2. Qué tan fuerte es esta relación? Estimación de sigma^2 (RSE).
3. Qué covariable está asociada a Y?
Lo veo con el p-valor del test conjunto.
4. Qué tan grande es la asociación de Y a cada covariable?
Me construyo los intervalos de confianza para los Beta_i
y, si incluye al 0, entonces esa variable no es significativa dadas
las otras.
Previo a esto debo verificar que no exista colinealidad.
Además, puedo fittear una regresión individual para cada
covariable que pienso que puede relacionarse con Y.
5. Con qué precisión puedo predecir una nueva observación (X_0, Y_0)?
Me construyo el intervalo de predicción para esa observación.
6. Es la relación lineal? Es decir, verificar el 1° supuesto del
modelo lineal. Esto lo hago viendo los residuos vs la estimación de Y
y concluyendo que no hay ningún patrón.
"