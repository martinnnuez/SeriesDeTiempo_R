########################################
# Simulación de Procesos AR, MA y ARMA #
########################################

library(TSA)

par(mfrow=c(2,2))

#EJEMPLO 1
# Simularemos 100 observaciones de modelo MA(1) con tita=-0.9 y graficamos la serie simulada
MA=arima.sim(list(ma=c(-0.9)), n=100)
plot(MA)

#Graficamos las funciones de autocorrelación simple y parcial estimadas e imprimimos los 30 primeros valores de estas funciones
#acf(MA, main="Autocorrelación Simple, MA(1),  tita=-0.9",ylim=c(-1,1),ci.col="black")#,ylab="", xlab="Retardo (Lag)")
#pacf(MA,main="Autocorrelación Parcial, MA(1), tita=-0.9",ylim=c(-1,1),ci.col="black")
autocs<-acf(MA,lag.max=30)
autocs
autocp<-pacf(MA,lag.max=30)
autocp
#Repetimos el Ejemplo 1, con distintas semillas, para cada uno de los siguientes modelos
# MA(2), tita1=-0.9, tita2=-0.5
# MA(2), tita1=0.9, tita2=-0.5, 
# MA(3), tita1=-0.9, tita2=-0.5, tita3= -0.3
# MA(3), tita1=0.9, tita2=0.5, tita3= 0.3
# MA(3), tita1=-0.9, tita2=0.5, tita3= -0.3
# MA(5), tita1=-0.9, tita2=-0.5, tita3=-0.3, tita4=-0.2., tita5=-0.1
# MA(5), tita1=0.9, tita2=-0.5, tita3=0.3, tita4=-0.2, tita5=0.1
# MA(5) y MA(6), con valores de los parámetros seleccionados por Usted.
# Obtenga conclusiones acerca del comportamiento de las funciones de autocorrelación simple y parcial de un proceso MA(q).

#EJEMPLO 2

# Simularemos 100 observaciones de modelo AR(1) con fi=0.6 y graficamos la serie simulada
AR=arima.sim(list(ar=0.6), n=100)
plot(AR)

#Graficamos las funciones de autocorrelación simple y parcial estimadas e imprimimos los 30 primeros valores de estas funciones
#acf(AR, main="Autocorrelación Simple, AR(1),  fi=0.6",ylim=c(-1,1),ci.col="black",ylab="", xlab="Retardo (Lag)")
#pacf(AR,main="Autocorrelación Parcial, AR(1), fi=0.6",ylim=c(-1,1),ci.col="black")
autocs<-acf(AR,lag.max=30)
autocs
autocp<-pacf(AR,lag.max=30)
autocp

#Repetimos el Ejemplo 2, con distintas semillas, para cada uno de los siguientes modelos
# AR(2), fi1=0.6, fi2=0.3
# AR(3), fi1=0.4, fi2=0.3, fi3=0.2
# AR(3), fi1=0.2, fi2=0.3, fi3=0.4
# AR(4), fi1=0.25, fi2=0.15, fi3=0.2, fi4=0.2
# AR(5), fi1=0.2, fi2=0.1, fi3=0.15, fi4=0.05, fi5=0.3
# AR(10), con valores de los parámetros f1=fi2=fi3=0.1, fi4=fi5=fi6=0.05, fi7= 0.15, fi8=0.015, fi9=0.15, fi10=0.1.
# Obtenga conclusiones acerca del comportamiento de las funciones de autocorrelación simple y parcial de un proceso AR(q).
# Ahora simule un AR(10) con parámetros a elección. Siguen sosteniéndose sus conclusiones?

#EJEMPLO 3

# Simularemos 100 observaciones de modelo ARMA(1,1) con fi=0.6 y tita=0.9, y graficamos la serie simulada
ARMA=arima.sim(list(ar=0.3, ma=0.6), n=100)
plot(ARMA)

#Graficamos las funciones de autocorrelación simple y parcial estimadas e imprimimos los 30 primeros valores de estas funciones
#acf(ARMA, main="Autocorrelación Simple, ARMA(1,1),  fi=0.6, tita=0.9",ylim=c(-1,1),ci.col="black",ylab="", xlab="Retardo (Lag)")
#pacf(AR,main="Autocorrelación Parcial, ARMA(1,1), fi=0.6, tita=0.9",ylim=c(-1,1),ci.col="black")
autocs<-acf(ARMA,lag.max=30)
autocs
autocp<-pacf(ARMA,lag.max=30)
autocp

#Repetimos el Ejemplo 3, para cada uno de los siguientes modelos
# ARMA(2,1), fi1=0.2, fi2=-0.3, tita=0.6
# AR(3,3), fi1=0.2, fi2=0.3, fi3=0.1, tita1=-0.9, tita2=0.4, tita3= 2.1
# AR(4,4), fi1=0.2, fi2=0.3, fi3=0.1, fi4=-0.05, tita1=-0.9, tita2=0.4, tita3= 2.1, tita4=-1.5
# AR(5,5), fi1=0.2, fi2=0.3, fi3=0.1, fi4=-0.05, fi5=-0.1, tita1=-0.9, tita2=0.4, tita3= 2.1, tita4=-1.5, tita5=0.8
# AR(10,10), con valores de los parámetros f1=fi2=fi3=0.1, fi4=fi5=fi6=0.05, fi7=fi8=fi9=fi10=0.15, y con valores de los parámetros tita's
# seleccionados por Usted
# Obtenga conclusiones acerca del comportamiento de las funciones de autocorrelación simple y parcial de un proceso AR(q).
# Ahora simule un AR(10,10) con parámetros a elección. Siguen sosteniéndose sus conclusiones?

