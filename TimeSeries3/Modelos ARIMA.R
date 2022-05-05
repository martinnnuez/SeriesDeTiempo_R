#################
# Modelos ARIMA #
#################

install.packages("astsa")
require(astsa)

##########################

#Ejemplo 1: Método Clásico

data(AirPassengers)
z<-AirPassengers
#son datos de pasajeros de una aerolínea
z
plot(z)
# Vemos que se trata de una serie no estacionaria, tiene media y varianza creciente
acf(z)
# Decaimiento muy lento a cero y todos positivos.
pacf(z)
# Periodicamente aparecen coeficientes fuera de las bandas


d<-decompose(z) # Opcion que permite descomponer la serie
plot(d)
# serie, tendencia, componente estacional y proceso de ruido. Este es un enfoque, no vamos a trabajar con este, vamos a trabajar con box y jenquins.
# Aca se proponen distintos modelos y se restan y nos quedamos con el ruido y proponemos modelo para el ruido.
# En box jenquins trabajamos con las diferencias.

d[[1]]
plot(d[[1]]) # La serie

d[[2]]
plot(d[[2]]) # Comp estacional

d[[3]]
plot(d[[3]]) # componente de tendencia

d[[4]]
plot(d[[4]]) # Comp de ruido
# Si bien no trabjamos con esto esta descomposicion ayuda en el analisis.
##################################

# Ejemplo 2: Generamos series no estacionarias

y<-rnorm(70,0,1)
z = ts(y,frequency=12,start=c(1955,01))
plot(z)

t<-seq(70)
t
z1<-z+t
plot(z1)
# Creo datos, le doy estructura de serie y le sumo una secuencia. 

d1<-decompose(z1)
plot(d1)
# Descomponemos la serie

# Generamos otra serie no estacionaria adicionando t al cuadrado

t2<-t^2
z2<-z+t2
d2<-decompose(z2) # Hacemos descomposicion
plot(d2)

###########################
#
# Ejemplo 3: Diferenciamos una serie
# Ya se que son series no estacionarias, vamos a diferenciarlas.
# Serie que yo tengo 1:10, cumsum() va acumulando.

w<-cumsum(1:10)
w
plot(w) # Generamos serie que acumula 

x <- cumsum(cumsum(1:10)) # 2 veces acumula. 
x
plot(x)
diff(x)#(1-B) # La funcion dif, desacumula (resta el anterior). 
diff(x, differences = 2)#(1-B)^2 # Lo hace la primera vez y luego de vuelta. Hce diff a la diff de orden 1.
x
diff(x, lag = 2)#(1-B^2) hace dif pero en vez de restar el Zt-1 resta el Zt-2, el lag define eso. 

z1
z1di<-diff(z1)
z1di
dz1di<-decompose(z1di)
plot(dz1di)
z1[1]
z1rec<-diffinv(z1di, xi=z1[1])
z1rec


library(forecast)
ndiffs(z) #da el n°de dif. simples necesarias pora llegar a un proceso estac.
# Me dice hasta cuando diferenciar para llegar a una estacionaria, me da el k. 

nsdiffs(z) #da el n°de dif. estacionales necesarias pora llegar a un proceso estac.

ndiffs(z1)

# Si me dice 0 no tiene raices unitarias. Herramienta muy poderosa. Siempre entender que estoy haciendo.
# Aplicando operador (1-B) a la serie. 
# En los modelos donde tengamos raices unitarias, vamos a aplicar las diferencias y proponemos AR, MA, ARMA y definimos el ARIMA.
# Y me quedo con un modelo ARIMA(p,k,q)

# En ARIMA la diferencia es (1-B)^k
# Distinto SARIMA, en estos aplicamos otro tipo de diferencias. 
# La diferencia estacional es (1-B^k), para los modelos que presentan componente ciclico. 