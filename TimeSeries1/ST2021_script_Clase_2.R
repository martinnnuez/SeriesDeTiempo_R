library(TSA)
##########
#Ejemplo 1
##########
#
#Proceso de ruido blanco gaussiano
#
y<-rnorm(100,0,1)
y
plot(y)
tiempo<-seq(1,100,by=1)
plot(tiempo,y)
#
#
# Para convertir los datos en un objeto tipo ts

ys = ts(y,frequency=1,start=c(1955,1))
plot(ys)
ys
# serie en la que hay una observación por unidad de tiempo
# y comienza en el mes 1 de 1995

ys = ts(y,frequency=12,start=c(1955,2))
plot(ys)
ys
# serie en la que hay 12 observación por unidad de tiempo
# y comienza en el mes 2 de 1995

#Tiene sentido analizar la correlación entre
#dos variables consecutivas del proceso.
# Un gráfico que da una idea de esta correlación 
# es el que muestra los pares (ys_t, ys_(t-1))
#plot(x=ys,y=zlag(ys),ylab=expression(ys[t]),xlab=expression(ys[t-1]),type='p')
plot(x=y,y=zlag(y),ylab=expression(ys[t]),xlab=expression(ys[t-1]),type='p')
#Ejerciico: Construya un ruido blanco con distribución uniforme
#
#
##########
#Ejemplo 2
##########
#
#Análisis de la función fas de un PROCESO MA(1) genérico
#Esta función depende del valor del parámetro tita del modelo
#y del 'lag' que lo denotamos con k.
#
#Para k=1 (corelación entre X_1 y X_(t+1)), la función depende
#sólo de tita:
f<-function(tita)(tita/(tita^2+1))
plot(f)
tita<-seq(-10,10,by=0.5)
plot(tita,f(tita))
curve(f , -10, 10)
abline(h=0, v=0, lty=3)

#fijado k=1, cuales son los valores extremos de esta función?
df<-function(tita)((-tita^2+1)/(tita^2+1)^2)
cerosdf = uniroot.all(df, c(-10,10), tol = 1e-15, maxiter=10)
install.packages('rootSolve')
library(rootSolve)
cerosdf = uniroot.all(df, c(-10,10), tol = 1e-15, maxiter=10)
cerosdf
f(1)
f(-1)
#Conclusión?
#Supongamos que tita vale 0.3. Entonce quedó:
f(0.3)
#Grafiquemos f como función de k, (tita=0.3)
x<-c(0:9)
x
y<-c(1,0.2752294,0,0,0,0,0,0,0,0)
plot(x,y)
sy<-ts(y)
plot(sy)
#¿qué nombre recibe este gráfico?
#Ahora con tita igual a 0.9
f(0.9)
x<-c(0:9)
x
y<-c(1,0.4972376,0,0,0,0,0,0,0,0)
plot(x,y)
sy<-ts(y)
plot(sy)
#
##########
#Ejemplo 3
##########
#
#Ejemplo de una SERIE generada mediante un proceso
#de medias móviles MA(1), con un parámetro tita=0.9
#y ruido blanco gaussiano. La serie tiene 120 observaciones.
win.graph(width=4.875, height=3,pointsize=8)
data(ma1.2.s)
plot(ma1.2.s,ylab=expression(Y[t]),type='o')
#
#
#A continuación se muestra el gráfico de una ESTIMACIÓN
#de la función de autocorrelación simple del proceso
#en base a la serie generada. 
acf(ma1.2.s)
#¿Es este gráfico el correlograma del proceso que generó a la serie?
# Ejercicio: Genere 100 observaciones de un proceso MA(1)
# con parámetro tita=-0.6
