# Practcio 2 
# Ejercicio 3

# Simulación de un ruido blanco
# Fijación de semilla para tener las mismas simulaciones
set.seed(1234)
n = 10000
e <-rnorm(n, mean=0 , sd=1)  # no necesariamente debe ser normal
# media cero
# varianza constante cualquier numero pero finito
# corrleacion, en rnorm son todas generacion independientes, entonces no estan correlacionados. 

# Simulemos MA(1) partiendo del ruido blanco e ya generado
# Mi forma
# embed(e,2)[,1] serie original y embed(e,2)[,2] primer retardo
ma1 <- (embed(e,2)[,1] + (embed(e,2)[,2]*0.5))
ma1 <- c(e[1],ma1)
x <- ma1
acf((x)) # El correlograma muestral da perfecto.


a=list()

for(i in 1:10000){
  n = 1000
  e <-rnorm(n, mean=0 , sd=1)  
  ma1 <- (embed(e,2)[,1] + (embed(e,2)[,2]*0.5))
  ma1 <- c(e[1],ma1)
  fas <- acf(ma1)
  a[[i]]<-fas$acf[2]
  }

ro<-do.call(c,a)

# El histograma muestra una clara distribucion normal
hist(ro)

# Test de normalidad
shapiro.test(ro[1:5000])# No la ejecuta por tener muchos datos
# Para 5000 datos rechaza la Ho, por lo que no tendria distribucion normal.

nortest::ad.test(ro)
# El test de anderson darling no rechaza la Ho, por lo que la muestra tendria distribucion normal con un 5% de confianza.

# Varianza: 
var(ro)
#0.0006051795 aproximadamente 0, con lo cual verifica.


# Ejercico 6:
## Proceso AR(2)
ar2 <- e
for (i in 3:n) {
  ar2[i] <- 1.5 * ar2[i-1] -  0.75 * ar2[i-2] + e[i]
  ar2
}
x <- ar2

# El AR 2 mira 2 pasados hacia atraz, por eso partimos del 3 para generar. 
# Grafico de la serie
plot(x, type="o", col="blue")

# Funcion de autocorrleacion simple:
acf(x, lag.max= 15)
head(acf(x, plot = F)$acf)
# Primeros 4 positivos, segundos 6 negativos y asi va alternando el signo. 


# Funciones parciales muestrales:
library(TSA)
lm(ar2 ~ -1 + zlag(ar2)) 
# 0.8604  
lm(ar2 ~ -1 + zlag(ar2) + zlag(ar2, 2))
# 1.4973       -0.7406  
lm(ar2 ~ -1 + zlag(ar2) + zlag(ar2, 2) + zlag(ar2, 3))
# 1.491344     -0.728733     -0.007917 

# alfa11 = 0.8604 SIGNIFICATIVO
# alfa22 = -0.7406 SIGNIFICATIVO
# alfa33 = -0.007917 No significativo, entonces se trata de un AR(2).

