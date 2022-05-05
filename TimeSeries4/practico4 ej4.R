# Practico 4
# Ejercicio 4: 
# carga de biblioteca TSA
library(TSA)
# Simulación de ruido blanco con varianza igual a 4.
set.seed(1234) 
n = 200
e <- rnorm(n, mean = 0, sd = 4) # no necesariamente debe ser normal
plot(e, type="o", col="blue",main="Ruido blanco", xlab = " ")

# Simulamos un AR(2)
ar1 <- e
for (i in 3:n) {
  ar1[i] <- 0.2 * ar1[i-1] + 0.5 * ar1[i-2] + e[i]
  ar1
}
x <- ar1

# Media y varianza:
mean(x[0:100]) # -2.387865
mean(x[101:200]) # 1.029579


autocovarianza<-acf(x[0:100], type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 34.469 

autocovarianza<-acf(x[101:200], type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 21.010

# Todas las observaciones
mean(x)#  -0.6791427

autocovarianza<-acf(x, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 30.66

# Difiere notablemente uno de otro. 


# PRUEBO SIMULANDO DE OTRA FORMA
library(astsa)
b <- c(0.2,0.5)
s <- 4
x=arima.sim(list(order = c(2,0,0), ar = b), n = 200, 
                rand.gen= rnorm, sd = s) 

# Media y varianza:
mean(x[0:100]) # -0.4852766
mean(x[101:200]) # -1.213458


autocovarianza<-acf(x[0:100], type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 27.5330

autocovarianza<-acf(x[101:200], type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 15.0344

# Todas las observaciones
mean(x)#  -0.8493674

autocovarianza<-acf(x, type ="covariance", plot = FALSE)
autocovarianza
#  lag 0 = 21.416