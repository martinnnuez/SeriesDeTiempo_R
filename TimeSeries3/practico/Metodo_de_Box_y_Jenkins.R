# MÉTODO DE BOX Y JENKINS

# 3 ETAPAS: 1) Identificación:  a) Graficar la serie (log?, términos determinísticos?, diferenciar?)
#                               b) Graficar acf y pacf muestrales (diferenciar?) (p y q tentativos)
#                               c) Prueba de Ljung & Box
#           2) Estimación: a) Principio de parsimonia (evitar overfitting)
#                          b) Verificar estacionariedad e invertibilidad
#                          c) Bondad de ajuste (AIC, SBC)
#           3) Residual Diagnostics: graficos de residuos, acf, pacf, Ljung & Box, Shapiro-wilks, etc
#           4) Predicción

load("sim.RData")
# Son datos reales que no samos de donde vinieron
# Queremos saber cual es el mejor modelo para realizar predicciones

# AGREGAR COMPONENTES HASTA QUE LOS RESIDUOS QUEDEN LIMPIOS, queden como ruido blanco y voy verificando, cuando querdan asi ya no agregar mas temrinos. 
# Principal ver correlogramas y las series que me van quedando.
# Lo proximo seria mas de raiz unitaria y darle cierre a esto. 


######################SIM1############################
y= sim1
# Graficamos
plot(y)
# Nos indica un poco de correlacion positiva, hay valores altos seguidos por altos y bajos por bajos.
# Parece centrarse en una media, estacionario en media.
# La varianza parece ser relativamente constante. Cuando se arma CONO es cuando esta creciendo la varianza, esto me preocuparia pq no se mantiene constante.
# La varianza es relativamente estable, si trazo dos lineas se me salen muy pocas observaciones.
# No plantearia ninguna transformacion, no veo una linea recta como para plantear recta.

mean(y) # media cero
# Media practicamente 0

# Identificación
acf(y)
# fas decae en forma suave pero no es muy lenta, entonces parece ser una serie estacionaria.
# Esto refuerza lo que deciamos sobre la media y la varianza.

pacf(y) # ar1
# primer valor es alto, el lag 1 es el unico que se sale de las bandas.

# De acuerdo a lo que vimos se trata de un proceso, cuando la simple decae suavemente y la parcial abrupta, pinta un AR(1)
# Tengo un sospechoso entonces procedemos a estimacion.

# Estimación
# Lo especifica sin media
# d = cantidad de diferencia, d = 0, es estimar un arma. d proviene de ARIMA, cuantas veces queremos diferenciar. 
# Si da no estacionario diferenciamos y modelamos con un ARMA. A MANO
# O bien ajustamos un ARIMA indicando d que diferenciada queremos. Y luego ajusta un ARMA para esa diferenciada.
# fsim1 <- fit1 <- arima(y, order=c(p=0, d=1, q=0), include.mean = F) # Devuelve la primera diferencia. y-zlag(y).
# res1 <- fit1$residuals # Esta es la serie diferenciada

fsim1 <- fit1 <- arima(y, order=c(p=1, d=0, q=0), include.mean = F)
fit1
?arima

res1 <- fit1$residuals
acf(res1)
# Ahora analizamos los residuos, y todos quedan adentro de las bandas entonces todos son significativamnte iguales a cero. 
# Los que se salen de las bandas, no tienen mucho sentido, el 12 podria haber sido algo de frecuencia anual, pero no es el caso.
# Nos parece bien el ajuste que empleamos. Se escapan pocos y por casi nada, pensamos que son 0. Siempre nos importan mucho mas los primeros.
# Hay variantes que van abriendo las bandas, si los que se escapan no son los primeros y no supongo ESTACIONALIDAD. 
# Entonces residuos del modelo son ruido blanco.
pacf(res1)

# Hemos aprovechado la estructura de autocorrelacion de la serie, ya la incluimos toda, aprovechamos toda la info que nuestra serie nos muestra. 
# Entocnes llegamos a residuos limpios ruido blanco, puedo cerrar la etapa de identificacion. Procedemos a validar un poco los supuestos. 


# Análisis diagnóstico

# Autocorrelación
Box.test(res1, lag = 10, type = "Ljung-Box", fitdf = 1)
# Test de Ljung-Box, es importanet el fitdf= 1, cuando aplicamos funcion sobre serie original fitdf=0, cuando es sobre los residuos es fitdf= quitando grados de libertad.
# Quito la cantidad de parametros que estime, AR(1), solo estime uno solo.
tsdiag(fit1)   #ok # En el grafico todos los p valores, el ultimo son significativos, no hay autocorrelacion.
# Ho: Me dice si las ro 1,2,....,10 son todas 0, coef de autocorrleacion. son 10. pq pongo lag10. 
# H1: al menos alguna de ellas es distinta de 0.
# pvalor alto, conf en Ho muy alta, NO rechazo Ho, al menos alguna de ellas es distinta de 0.
# Puede suceder encontrar para 1 y no para 2 y es pq es una prueba estadistica, no matematica.
# Hay autocorrelacion, supuesto mas importante, perfecto!

# Heterocedasticidad
library(aTSA)
# Esta libreria nos permite hacer 2 pruebas, una Pormanteau y Lagrange-Multiplier, para ver si hay heterocedasticidad
# Muy parecida Ljun-Box pero con residuos al cuadrado.

# Ho: Me dice si las ro de los residuos cuadrados son iguales a cero. Esto cambia. 
# H1: al menos alguna de ellas es distinta de 0.

arch.test(fit1, output = TRUE)
res1_cuadrado = res1^2 #correlogramas de los cuadrados de los residuos
acf(res1_cuadrado)
pacf(res1_cuadrado)
# Cuando los cuadrados de los resiudos estan correlacionados eso quiere decir que hay heterocedasticidad
# Cuadrado de los residuos son estimaciones de varianza residual.
#  Un residuo cuadrado mide varianza 
# Grafico de los residuos no patron, no hay heterocedasaticidad
# Embudo hay heterocedasticidad
# Grafico residuos al cuadrado, embudo se transforma en tendencia, esto es señal de autocorrelacion, que significa heterocedasticidad.

# Despues tenes el PQ prob, no encuentra heterocedasticidad pq no encuentra autocorrelacion entre residuos cuadrados.
# Esta el LM prob, que es:
# Lagrange-Multiplier test (ARCH LM), plantea regresion entre residuos cuadrados y retardo de los residuos cuadrados.
# Ve si hay autocorrelacion de una forma disinta, planteando una regresion. Este nos da como que potencialmente hay heterocedasticidad, pq caen dentro de las bandas.
# Las autocorrelaciones son distintas de 0, en los primreros retardos. 

# Punto central residuos cuadrados son estimacion de varianza residual, luego autocorrelacion en los residuos cuadrados, es señal de heterocedasticidad. 
# Esto se ve en LM que pone residuos cuadrados en funcion de retardos. 
# Autocorrelacion en residuos cuadrados, es una autocorrelacion en varianza. Que varianzas residual de un dato esta ligada a la de los datos que tiene cerca.
# Luego un cono nos indica, que hay correlacion, tendencia. 

# No lo paso de 10 pero no tenemos mucha mas herramienta.

# Normalidad
hist(fit1$res)
shapiro.test(fit1$residuals)
qqnorm(fit1$res)
qqline(fit1$res)

# Una vez hecha la serie estacionaria, tiene sentido ver histogramas y pruebas de normalidad. 

# Para cerrar, detectamos AR(1), no hace falta otro temrino, validamos, vimos si habia autocorrelacion, normalidad, nos quedan dudas heterocedasticidad.
# Luego de heterocedasticidad, arrancar de nuevo tomando log o ver si se debe a algo de dato atipico. 
# O un modelo con componente ARCH, mezclando ARIMA con ARCH para los errores de ese ARIMA.

# Al final vemos los pronósticos

# Estacionareidad. Verificar raiz unitaria, y el acf teine decaimiento suave pero no es para nada lento, esto nos indica de que no hay problema de estacionareidad.
# Luego tenemos pruebas de raiz unitarias mas formales. 




#######################SIM2#################
y = sim2
plot(y)
mean(y) # media cero
acf(y)  # ma1?
pacf(y) # ar2?


fit1 <- arima(y, order=c(p=0, d=0, q=1), include.mean = F)
fit1

res1 <- fit1$residuals
acf(res1)
pacf(res1)
tsdiag(fit1) # no da demasiado bien

fsim2 <- fit2 <- arima(y, order=c(p=2, d=0, q=1), include.mean = F)
fit2

res2 <- fit2$residuals
acf(res2)
pacf(res2)
tsdiag(fit2)  # da mejor el diagnóstico

c(AIC(fit1), BIC(fit1))
c(AIC(fit2), BIC(fit2))
# los criterios de información muestran resultados contradictorios 
# (cuanto menor valor, mejor modelo), pero el análisis de los residuos usando tsdiag
# indica que es mejor fit 2. 
# Luego, el modelo AR2 es seleccionado (aunque veremos que la simulación fue MA1).

# En la segunda serie postula dos modelos fit1 y fit2, aca es donde entran a servir los criterios de informacion. AIC,BIC mas chico mejor.
# Por lo general dan en la misma direccion. 

# Tambien se puede elegir por quien da mejores metricas.

# 1 mirar residuos ruido blanco.
# 2 criterios de informacion (criterio clasico)
# 3 metricas ajuste predichos.




###################SIM3#######################
y= sim3
plot(y)
mean(y) # media cero
acf(y) # decae suavemente
pacf(y) # decae suavemente   ARMA(1,1)


fsim3 <- fit1 <- arima(y, order=c(p=1, d=0, q=1), include.mean = F)
fit1

res1 <- fit1$residuals
acf(res1)
pacf(res1)
tsdiag(fit1) # ok

# Aqui decaimiento suave de las dos funciones, las hipotesis mejores es que es un ARMA(1,1), aqui se mezclan los dos decaiminetos y son ambos lentos. Esto es lo especial.
# Ambos lento solo un ARMA podria explicar.




############################SIM4#############################
y= sim4
plot(y)
mean(y) # media = 20
acf(y) # decaimiento suave
pacf(y) # ar1


fsim4 <- fit1 <- arima(y, order=c(p=1, d=0, q=0), include.mean = T)
fit1 #se ve que el intercepto es la media
#a0 sería 20*(1+0.8483)

res1 <- fit1$residuals
acf(res1)
pacf(res1)
tsdiag(fit1) # ok

# En este hay una ordenadoa, entonces le ponemos  include.mean = T, entonces incluye la media.





#################SIM5############################################
y= sim5
plot(y)
# Media no constante, sigue linea recta, entocnes duda de raiz unitaria.

mean(y) 
acf(y)
pacf(y) #ar1 luego de remover tendencia

# Uno remueve tendencia lineal, con el detrending. crea tt y hace el modelo.
# Solo funciona en los casos es que es una tendencia deterministica. 
# Sila tendencia es por raiz unitaria o estacionalidad si o si hay q diferenciar.

# detrending
tt=1:200
fit_aux <- lm(y~tt)
fit_aux <- lm(y~I(1:200))
fit_aux
fit_aux$coefficients[[2]]
y_det <- resid(fit_aux) # det por detrended
plot(y_det, type="l")
acf(y_det)
pacf(y_det)

fsim5_det <- fit1 <- arima(y_det, order=c(p=1, d=0, q=0), include.mean = F)
fit1

res1 <- fit1$residuals
acf(res1)
pacf(res1)


# o bien
tt= 1:200

fsim5_con_tendencia <- fit1 <- arima(y,  order=c(p=1, d=0, q=0), 
                                     include.mean = T, xreg= tt )
fit1

# Otra forma incluir xreg= tt, estima con tendencia todo de una. Asi se incluyen regresores exogenos en un ARIMA. 
# Tiene todos los elementos mas beta*tt. 

res1 <- fit1$residuals
acf(res1)
pacf(res1)

# notar que se podía estimar usando arima.auto, llegando al mismo resultado
library(forecast)
fit <- auto.arima(sim5, xreg=tt); fit
# Elige el que da mejor aic corregido.
# Quiza lo optimo a mejorar es que los residuos queden bien. OJO.


# Pronósticos
# Predicciones para cada uno de los modelos.
fit1=fsim1
y=sim1
predict(fit1, n.ahead=20)
plot(y, xlim=c(0,220))
lines(predict(fit1, n.ahead=20)$pred, col="red", lwd=1.5, type="l")
lines(predict(fit1, n.ahead=20)$pred-2*predict(fit1, n.ahead=20)$se, col="blue", lty="dashed", lwd=2)
lines(predict(fit1, n.ahead=20)$pred+2*predict(fit1, n.ahead=20)$se, col="blue", lty="dashed", lwd=2)

fit1=fsim1
for1 <- forecast(fit1, 20)
for1
plot(for1)

y=sim2
fit1=fsim2
for1 <- forecast(fit1, 20)
for1
plot(for1)

y=sim3
fit1=fsim3
for1 <- forecast(fit1, 20)
for1
plot(for1)

y=sim4
fit1=fsim4
for4 <- forecast(fsim4, 20)
for4
plot(for4)

###########################resta agregar la tendencia
y=sim5
fit1=fsim5_det
for1 <- forecast(fit1, 20)
for1
plot(for1)
############################

#se agrega la tendencia
fit1=fsim5_det
predict(fit1, n.ahead=20)
plot(y, xlim=c(0,220))
lines(predict(fit1, n.ahead=20)$pred +  fit_aux$coefficients[[1]] + 
        fit_aux$coefficients[[2]]*(201:220), col="red", lwd=1.5, type="l")

predict(fit1, n.ahead=20)
###############################

# pronóstico con tendencia, usando arima.auto
y=sim5
fit1=fsim5_con_tendencia; fit1

fit <- auto.arima(sim5, xreg=tt); fit

tnew <- matrix( c(201:220), nrow=20, ncol=1)
for1 <- forecast(fit,  xreg = tnew )
for1
plot(for1)
points(1:length(sim5),fitted(fit),type="l",col="green")

accuracy(for1)

######
# Pronóstico out of sample

y=sim5

# Dividimos en los primeros 100 y los ultimos 100.
y_train= sim5[1:100]
y_test = sim5[101:200]

fit_train <- auto.arima(y_train, xreg=1:100)

tnew <- matrix( c(101:200), nrow=100, ncol=1)
# Este le da los nuevos valores que yo quiero predecir.
# Destacar separacion entrenamiento y testeo y podemos calcular metricas para ver el poder predictivo del modelo. 

for_out <- forecast(fit_train,  xreg = tnew )
for_out
plot(for_out)
points(1:length(y_train),fitted(fit_train),type="l",col="green")

# métricas de precisión del pronóstico
accuracy(for_out)
accuracy(for_out, y_test)

# # De este modo se generaron las series
# set.seed(14)
# sim1 <- arima.sim(n=200, model=list(ar=0.8))
# sim2 <- arima.sim(n=200, model=list(ma=-0.7))
# sim3 <- arima.sim(n =200, list(ar = c(0.6), ma = c(.7)))
# sim4 <- arima.sim(n=200, model=list(ar=-0.8)) + 20
# sim5 <- arima.sim(n=200, model=list(ar=0.8)) + 20 + 0.04*(1:200)
# 
# save(sim1, sim2, sim3, sim4, sim5, file="sim.RData")

# Cuando hay pocos datos logica de entrnamiento y test, validacion depende de muy pocos datos y eso es problematico. 
# 