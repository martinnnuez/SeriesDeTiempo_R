#############################################
#Causalidad e Invertibilidad de Procesos ARMA
#############################################

# Simulemos 100 realizaciones de un proceso AR(3) con par?metros 2.1,-1.44, 0.32
set.seed(12345)
AR=arima.sim(list(order=c(3,0,0), ar= c(2.1, -1.44,0.32)), n=100)
plot(AR)

# Order 3 por el orden del proceso AR, 0 por procesos ARIMA todabia no lo vimos, 0 por el orden del proceso de medias moviles y es un AR 3 no tenemos componentes de medias moviles. 
# Luego le damos los coeficientes en ar = .....
# n = 100 entonces 100 relizaciones.

# Analicemos si el proceso que genero a esta serie es causal
# Vamos a buscar las reices y ver donde caen, recordar que los coeficientes del polinomio son los mismos que los del modelo
# Encontremos las raices del polinomio 

# Recordar el polinomio phi(z)= 1- phi1 -phi2 -phi3 -phi4= este seria el polinomio complejo la cual le calculo las raices.
polyroot(c(1, -2.1, 1.44, -0.32))
  #donde los numeros del argumento corresponden 
  #a los coeficientes del polinomio autorregresivo
  #(ordenados en forma creciente del grado del t?rmino
  #que acompañan).

# obtengo las raices, 1.25+0i 1.25-0i 2.00-0i
# Vemos que estan fuera del cirulo unitario por lo tanto ES CAUSAL.
# Y como es causal luego es ESTACIONARIO. 


# Para calcular y graficar los coeficientes del 
# desarrollo en serie de Laurent de la inversa del
# polinomio autorregresivo:
coef<-ARMAtoMA(ar=c(2.1, -1.44, 0.32),lag.max=50)
#Gr?fico de los coeficientes
plot.ts(ARMAtoMA(ar=c(2.1,1.44,0.32),lag.max=50),xlab="lag")
coef

# Aca calculamos los alfa coeficientes muy importante. 
# Es una sucesion entonces hay infinitos alfas.


#__________________________________________________
#Proceso AR(3) no CAUSAL:
AR=arima.sim(list(order=c(3,0,0), ar= c(-4.1, 2.44,-0.32)), n=100)
#si no es estacionario, no puede ser causal

# El R da un errorcito, porque cuando no es estacionario el proceso no lo deja simular ni graficar.

# Cuando busco las raices, hay una que cae dentro del circulo unitario y por lo tanto es NO CAUSAL.
polyroot(c(1, 4.1, -2.44, 0.32))
# La primera raiz cae dentro del circulo unitario no satisface la condicion, entonces el proceso es no causal, podria ser estacionario. 
# Si hubiese una raiz unitaria el proceso es no estacionario
# El R toma como si fuese no casual = no estacionario, y esto no es necesariamente asi. 
# Puede ser un proceso estacionario sin que sea causal.

# Ahora vemos lo mismo con la invertibilidad, es lo mismo pero se analiza la parte del modelo de medias moviles.

#__________________________________________________
#Invertibilidad de un Procesos MA

# Simulaci?n de un proceso MA(1) invertible

MA=arima.sim(list(order=c(0,0,1), ma= (0.5)), n=1000)
plot(MA)

# MA 1 order (0,0,1) 1 donde va el coeficiente del MA
# le pone el coeficiente y n=1000.

#Es invertible?
# Buscamos raices del plonomio complejo, coeficientes positivos. 
polyroot(c(1,0.5))

#_____________________________________
#Simulaci?n de un proceso ARMA(1,1)  

ARMA=arima.sim(list(order=c(1,0,1), ar= (0.3), ma=(0.2)), n=1000)
#Es estacionario?
polyroot(c(1, -0.3))
#Es invertible?
polyroot(c(1,0.2))

# Verificar que ocurre con el proceso ARMA(1,2) que sigue:  
ARMA=arima.sim(list(order=c(1,0,2), ar= (0.3), ma=c(0.2, -0.4)), n=1000)



ar1=arima.sim(model=list(ar=c(2)),n=1000)

plot(ar1)

polyroot(c(1, -2))



