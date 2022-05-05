#install.packages('tseries')
library(tseries)
#Ejemplos de datos temporales

# Ejemplo 1
data(bev)
plot(bev, xlab="", ylab="", xaxt="n")
x.pos<-seq(1500,1869)[c(seq(1,length(bev)-60,60),length(bev))]
x.pos<-c(1500, 1560, 1620, 1680, 1740, 1800, 1869)
axis(1, x.pos, x.pos)
title(xlab="Year", ylab="Wheat price index", line=3, cex.axis=1.2,cex.lab=1.2)

# Ejemplo 2:
datos<-read.csv("directorio deonde están los datos/tasa_mortalidad_infantil_1990_2019.csv", header=TRUE)
datos1<-datos$mortalidad_infantil_argentina
plot(datos1, xlab="", ylab="")
plot(datos1, xlab="", ylab="", type="o")
plot(datos1, xlab="", ylab="", type="l")
plot(datos1, xlab="", ylab="", type="l", xaxt="n")
x.pos<-c(1990, 1995, 2000, 2005, 2010, 2015, 2019)
axis(1, at=c(1,5,10,15,20,25,30), x.pos)
title(xlab="Año", ylab="Mort. Inf. Argentina", line=3, cex.axis=0.8,cex.lab=0.8)


#_______________________________________
install.packages('TSA')
library(TSA)
#Ejemplos de datos temporales

#Ejemplo 1:
#Datos de Nivel de precipitación anual (100 años), en Los ?ngeles
#win.graph(width=4.875,height=2.5,pointsize=8)
data(larain) 
plot(larain,ylab='Nivel de Precipitación (Pulgadas)',xlab='A?o',type='o')
#
#
#win.graph(width=3,height=3,pointsize=8)
plot(y=larain,x=zlag(larain),ylab='Nivel de Precipitación (Pulgadas)',xlab='Nivel de precipitación del año anterior (Pulgadas)')
#
#
#Ejemolo 2:
#Serie de tiempo referido a un proceso químico industrial.
#La variable medida es una propiedad de color,
#observada en lotes consecutivos en el proceso.
win.graph(width=4.875, height=2.5,pointsize=8)
data(color)
plot(color,ylab='Propiedad de color',xlab='Lote',type='o')
#
#
win.graph(width=3, height=3,pointsize=8)
plot(y=color,x=zlag(color),ylab='Propiedad de color',
xlab='Propiedad de color del lote previo')
#
#
#Ejemplo 3:
#Datos de promedio mensual de temperatura en una ciudad
win.graph(width=4.875, height=2.5,pointsize=8)
data(tempdub)
plot(tempdub,ylab='Temperatura mensual promedio',type='o')
#
#
#Ejemplo 4:
#Ventas mensuales a los concesionarios de un filtro 
#de aceite especial para equipos de construcci?n 
data(oilfilters)
plot(oilfilters,type='o',ylab='Ventas')
plot(oilfilters,type='l',ylab='Ventas')
#Ventas con caracteres especiales de julio a junio
points(y=oilfilters,x=time(oilfilters),pch=as.vector(season(oilfilters)))
#Ventas con caracteres especiales personalizados (en espa?ol) 
Month=c("Ju","A","S","O","N","D","E","F","M","A","My","J")
points(oilfilters,pch=Month)

