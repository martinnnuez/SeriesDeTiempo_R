# Practico 3

######### Ejericio 4

# Proceso = Yt = Et - 1.2 Et-1 + 0.35 Et-2
# fijación de semilla para tener las mismas simulaciones
set.seed(1234)
n = 100
e <-rnorm(n, mean=0 , sd=1)  # no necesariamente debe ser normal
# media cero
# varianza constante cualquier numero pero finito
# corrleacion, en rnorm son todas generacion independientes, entonces no estan correlacionados. 
plot(e, type="o", col="blue",main="Ruido blanco", xlab = " ")

# Simulamos el MA(2)
ma2 <-e
for (i in 3:n){
  ma2[i]<- e[i] - 1.2 * e[i-1] + 0.35 * e[i-2] 
}
acf(ma2)
x = ma2

# Verifico estacionalidad:
# Poliroot para verificar causalidad o estacionareidad colocar signo como alreves (parte del AR)
# Como es MA asumo que es estacionario.

# Verificamos si es invertible:
# Poliroot para verificar invertibilidad colocar signo como estan (parte del MA)
polyroot(c(1, -1.2, 0.35))
# 1.428571+0i 2.000000+0i caen fuera del circulo unitario por lo que es invertible

# Media y varianza:
mean(ma2) #-0.02337476

autocovarianza<-acf(ma2, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 2.0849

# Funcion de autocorrelacion simple del proceso:
acf(ma2)
# Coeficientes
head(acf(ma2, plot = F)$acf)
# Vemos el decaimineto abrupto caracteriztico de los MA.
# Al ser un MA2 es significativo hasta el segundo coeficiente. 

library(astsa)
# Calculo coeficientes del equivalente AR infinito:
# polinomio autorregresivo:
coef<-ARMAtoAR(ma=c(-1.2, 0.35),lag.max=50)
#Gr?fico de los coeficientes
plot.ts(ARMAtoAR(ma=c(-1.2, 0.35),lag.max=50),xlab="lag")
coef

# Aca calculamos los alfa coeficientes muy importante. 
# Es una sucesion entonces hay infinitos alfas.

which(abs(coef)>0.2)
# El septimo coeficiente ya es menor a 0,2.


######### Ejercicio 5
# Queda la duda de como poner los coeficientes en la funcion!.
arma=arima.sim(list(order=c(1,0,1), ar= c(-0.8), ma=c(-0.3)), n=10000)
#Es estacionario? Poner negativo
polyroot(c(1, -0.8)) # 1.25+0i

#Es invertible?
polyroot(c(1,-0.3)) # 3.333333+0i

# Media y varianza:
mean(arma) #0.0022385

autocovarianza<-acf(arma, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 4.140

# Funcion de autocorrelacion simple del proceso:
acf(arma)
# Coeficientes
head(acf(arma, plot = F)$acf)
# Decaimiento suave identifica arma

# Calculo coeficientes del equivalente AR infinito:
# polinomio autorregresivo:
coef<-ARMAtoMA(ar=c(-1.2, 0.35),lag.max=50)
#Gr?fico de los coeficientes
plot.ts(ARMAtoAR(ar= c(-0.8), ma=c(-0.3),lag.max=50),xlab="lag")
head(coef)

# Aca calculamos los alfa coeficientes muy importante. 
# Es una sucesion entonces hay infinitos alfas.



######### Ejercicio 6 No supe modelar con la constante
arma=arima.sim(list(order=c(2,0,2), ar= c(1,-0.2), ma=c(1,-0.3)), mean=25, n=100)

#Es estacionario?
polyroot(c(1,-0.2)) # -1.25+0i si

#Es invertible?
polyroot(c(1,-0.3)) # -3.333333+0i si

# Media y varianza:
# Me queda la duda! La media deberia ser 25.
mean(arma) #211.9607

autocovarianza<-acf(arma, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 10.1945    

# Funcion de autocorrelacion simple del proceso:
acf(arma)
# Coeficientes
head(acf(arma, plot = F)$acf)
pacf(arma)
# Decaimiento lento en ambos identifica arma

# Calculo coeficientes del equivalente AR infinito:
# polinomio autorregresivo:
coef<-ARMAtoMA(ar= c(1,-0.2), ma=c(1,-0.3),lag.max=50)
#Gr?fico de los coeficientes
plot.ts(ARMAtoAR(ar= c(1,-0.2), ma=c(1,-0.3),lag.max=50),xlab="lag")
head(coef)

# Primeros 5 coeficientes 2.00 1.50 1.10 0.80 0.58 0.42



################## Ejercicio 7

arma=arima.sim(list(order=c(1,0,1), ar= c(-0.8), ma=c(-0.3)), n=100)

??arima.sim







################### Ejercicio 4 practico 4
arma=arima.sim(list(order=c(2,0,0), ar= c(0.2,0.5)), mean=0, n=200)
arma1=arma[0:100]
arma2=arma[100:200]

# b)
# Media y varianza:
mean(arma1) #0.5609536

autocovarianza<-acf(arma1, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 1.02039    

# c)
# Media y varianza:
mean(arma2) #0.6752831

autocovarianza<-acf(arma2, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 1.24487    


# d)
# Media y varianza:
mean(arma) #0.619311

autocovarianza<-acf(arma, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 1.14197


#Es estacionario?
polyroot(c(1, 0.8)) # -1.25+0i si

#Es invertible?
polyroot(c(1,0.3)) # -3.333333+0i si

# Media y varianza:
mean(arma) #211.9607

autocovarianza<-acf(arma, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 6.0950  

# Funcion de autocorrelacion simple del proceso:
acf(arma)
# Coeficientes
head(acf(arma, plot = F)$acf)
pacf(arma)
# Decaimiento lento en ambos identifica arma

# Calculo coeficientes del equivalente AR infinito:
# polinomio autorregresivo:
coef<-ARMAtoMA(ar= c(1,-0.2), ma=c(1,-0.3),lag.max=50)
#Gr?fico de los coeficientes
plot.ts(ARMAtoAR(ar= c(1,-0.2), ma=c(1,-0.3),lag.max=50),xlab="lag")
head(coef)