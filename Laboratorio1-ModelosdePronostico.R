rm(list=ls())

# Autor : André Chávez

# Librerias básicas para el estudio de series temporales

library(ggplot2)
library(TSA)
library(forecast)
library(scales)
library(stats)



#Si tenemos el archivo en formato SPSS
#serie<- read.spss("data.sav",use.value.labels = T,max.value.labels=TRUE,to.data.frame=TRUE,trim_values=T)

#Si tenemos el archivo en formato EXCEL
#serie <- read_excel("data.xlsx",col_names = T)


#Leer la serie de tiempo, desde un archivo csv.
serie=read.csv("data.csv")
View(serie)
#Convertir la serie en un set de datos
Yt=data.frame(serie[,1])
names(Yt)=c("serie")
#Convertir el set de datos en un archivo de serie temporal
Yt<-ts(Yt,start=c(2013,1),freq=12)
date<-seq(as.Date("2013/01/01"),as.Date("2017/05/01"),by="months")
data<-data.frame(Yt,date)

#############################################
########### Análisis de la Serie##############
##############################################

# Plot
plot(Yt,col="lightblue",
     main="DESEMBARQUE MENSUAL DE LA PESCA MARITIMA Y CONTINENTAL",
     ylab="",
     xlab="Tiempo")

# GRÁFICO DE CAJAS DE LA SERIE DE TIEMPO

boxplot(Yt ~ cycle(Yt))

# GRÁFICO DE AUTOCORRELACIONES
# ACF
acf(Yt,lag.max=198,
    xaxp=c(0,16,16),
    main="DESEMBARQUE MENSUAL DE LA PESCA MARITIMA Y CONTINENTAL",
    ylab="Autocorrelaciones",
    xlab="Retardos")

# GRÁFICO DE AUTOCORRELACIONES PARCIALES
# PACF
pacf(Yt,lag.max=198,
     xaxp=c(0,16,16),
     main="DESEMBARQUE MENSUAL DE LA PESCA MARITIMA Y CONTINENTAL",
     ylab="Autocorrelaciones parciales",
     xlab="Retardos")




# GRÁFICO DE DESCOMPOSICIÓN DE LA SERIE DE TIEMPO

Yt_desc = decompose(Yt)
plot(Yt_desc , xlab='Año')

## GRÁFICO DE DISPERSIÓN VS NIVEL

data$date1=as.character(data$date)

library(sqldf)
names(data)=c("serie","date","date1")
data1 <- sqldf("select substr(date1,1,4) as año, sqrt(serie) as disp,avg(serie) as prom
               from data
               group by substr(date1,1,4)")

plot(data1$prom,data1$disp,
     main="DESEMBARQUE MENSUAL DE LA PESCA MARITIMA Y CONTINENTAL",
     ylab="Dispersión",
     xlab="Nivel")
