library(TSA)
##########
#Ejemplo 1
##########
#
# An?lisis de la funci?n fas de un PROCESO AR(1). 
# Esta funci?n depende del valor del par?metro 
# fi del modelo y del 'lag' que lo denotamos con k.
#
# Para asegurar la estacionaridad del proceso,
# elegimos fi tal que su valor absoluto sea 
# menor que 1.
# 
#
# Supongamos que fi=0.9. Estudiemos la fas 
# del proceso. Esta funci?n depende s?lo de k:
#
f<-function(k)(0.9^k)
plot(f)
k<-c(0:9)
plot(k,f(k))
curve(f , 0, 10)
# Este gr?fico es el correlograma del proceso.
#
#
#########
#Ejemplo2
#########
# Los datos ar1.s han sido generados por un modelo
# AR(1) con par?metro fi=0.9 y ruido blanco gaussiano
#win.graph(width=4.875, height=3,pointsize=8)
data(ar1.s)
plot(ar1.s,ylab=expression(Y[t]),type='o')
#win.graph(width=3, height=3,pointsize=8)
plot(y=ar1.s,x=zlag(ar1.s),ylab=expression(Y[t]),xlab=expression(Y[t-1]),type='p')
plot(y=ar1.s,x=zlag(ar1.s,2),ylab=expression(Y[t]),xlab=expression(Y[t-2]),type='p')
plot(y=ar1.s,x=zlag(ar1.s,3),ylab=expression(Y[t]),xlab=expression(Y[t-3]),type='p')
acf(ar1.s)
# Este gr?fico es la estimaci?n del correlograma del
# proceso. (estimaci?n basada en la serie ar1.s)
#
#
##########
#Ejemplo 3
##########
#
# La siguiente sentencia genera una serie de 
# tama?o 1000 a partir de un proceso AR(1) con 
# par?metro fi=-0.9
ar1=arima.sim(model=list(ar=c(-0.9)),n=1000)
plot(ar1)
acf(ar1) # correlograma muestral 
# Correlograma del proceso que gener? la serie:
f<-function(k)((-0.9)^k)
win.graph(width=4.875, height=3,pointsize=8)
k<-c(0:100)
plot(k,f(k))
curve(f , 0, 100)
#
##########
#Ejercicios
##########
# a)Genere una serie a partir de un modelo AR(1),
#   con ruido blanco gaussiano y par?metro 0.7.
#   Obtenga el correlograma estimado.
#   Obtenga el correlograma te?rico
# b)Genere una serie a partir de un modelo AR(1),
#   con ruido blanco gaussiano y par?metro 7. 
#   Obtenga el correlograma te?rico y el estimado. 
#   ?Qu? ocurri??
# c)Genere una serie a partir de un modelo AR(1),
#   con ruido blanco exponencial y par?metro 0.7.
#   Obtenga el correlograma te?rico y el estimado.
