##########################
##Series de Tiempo con R##
#########################

##Autor : André Chávez##
# Librerias básicas para el estudio de series temporales

library(ggplot2)
library(TSA)
library(forecast)
library(scales)
library(stats)


#Leer la serie de tiempo, desde un archivo csv.
setwd("C:/Users/Andre Chavez/Desktop/Data")
pbi=read.csv("pbi.csv",header = T,sep="",dec=",")
View(pbi)

##Creación de una serie de tiempo
pbits<-ts(pbi,start=c(1991,1),end=c(2009,12),frequency=12)
fechas = seq(as.Date("1991/1/1"), length.out = length(pbits), by = "months")
pbits
plot(fechas)

##Enfoque de Descomposición

# GRÁFICO DE DESCOMPOSICIÓN DE LA SERIE DE TIEMPO
Yt_desc = decompose(pbits,type = "multiplicative",filter = NULL)

#Gráfico de Descomposición de la Serie
plot(Yt_desc , xlab='Año')

#Serie Original
pbits_original<-Yt_desc$x
View(pbits_original)

#Coeficientes Estacionales
Coeficientes_Estacionales<-Yt_desc$seasonal
plot(Coeficientes_Estacionales)

#Tendencia de la serie
Yt_desc$trend

#Tipo de Modelo Aplicado
Yt_desc$type


###########################################################################
###########################################################################
#A la serie original,le quitamos la componente de estacionalidad, nos quedamos
#sólo con la tendencia.
Tendencia_pbi<-pbits_original/Coeficientes_Estacionales
plot(Tendencia_pbi)

Tendencia_pbi<-as.double(Tendencia_pbi)
#Viendo sólo la componente tendencia, le ajuste la curva que mejor modele su
#comportamiento o que mejor la ajuste.

T = length(Tendencia_pbi)
yi = Tendencia[1:T]


# Ajustar 4 modelos: Lineal, Cuadrático, Cúbico
t = seq(1:T)
t2 = t**2
t3 = t**3

# Ajuste de Polinomiales a la Componente Tendencia
mod.lin = lm(yi~t)
mod.cuad = lm(yi~t+t2)
mod.cub = lm(yi~t+t2+t3)


summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)



pr2 = predict(Tendencia_pbi,mod.cuad)
plot(Tendencia,pr2)

estimaciones<-pr2*CE
plot(estimaciones,col="red")
lines(pbits,col="green")
forecast(estimaciones,h=12)

estimaciones<-pr2*CE
plot(pbits,col="red")
lines(estimaciones,col="green")
forecast(estimaciones,h=12)





plot(forecast(pbi_fit,h=12),col="red")
lines(pbi_fit$fitted,col="green")
forecast()

# GRÁFICO DE AUTOCORRELACIONES
# ACF
acf(pbits,lag.max=90,xaxp=c(0,16,16),main="PBI",ylab="Autocorrelaciones",xlab="Retardos")

# GRÁFICO DE AUTOCORRELACIONES PARCIALES
# PACF
pacf(pbits,lag.max=90,xaxp=c(0,16,16),main="PBI",ylab="Autocorrelaciones parciales",xlab="Retardos")


#Prueba de Aleatoriedad
# H0: Los datos son registros aleatorios.
# H1: Los datos no son registros aleatorios.
library(randtests)
bartels.rank.test(pbits,alternative = "two.sided")

#Medias Móviles
plot(filter(pbits,filter=rep(1/3,3),sides=2,method="convolution"))
plot(filter(pbits,filter=rep(1/5,5),sides=2,method="convolution"))
plot(filter(pbits,filter=rep(1/9,9),sides=2,method="convolution")) 
#Medias Móviles
plot(pbits, main="PBI",ylab="PBI", xlab="Año")
lines(ma(pbits,10),col="red")

#Suavización Exponencial Simple
library(forecast)
library(fTrading)
fit1 <- ses(pbits, alpha=0.2, initial="simple", h=3)
fit2 <- ses(pbits, alpha=0.6, initial="simple", h=3)
fit3 <- ses(pbits, h=3)
plot(fit1, plot.conf=FALSE, ylab="pbi",
     xlab="Año", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)



# Suavización EXponencial de Holt y Winters


fit1 <- holt(pbits, alpha=0.8, beta=0.2, initial="simple", h=5) 
fit2 <- holt(pbits, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5) 
# Results for first model:
fit1$model$state
fitted(fit1)
fit1$mean



fit3 <- holt(pbits, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=5) 
plot(fit2, type="o", ylab="PBI", xlab="Año", 
     fcol="white", plot.conf=FALSE)
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft", lty=1, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))





#Predicciones Mediante Holt y Winters
pbi_Holt<-HoltWinters(pbits)
pre_pbi_Holt<-predict(pbi_Holt,50,prediction.interval = T)
plot(pbi_Holt,pre_pbi_Holt)
plot(pre_pbi_Holt)


# GRÁFICO DE DESCOMPOSICIÓN DE LA SERIE DE TIEMPO
#Descomposición hecha por Promedios móviles
pbi_desc = decompose(pbits,type = "multiplicative")
plot(pbi_desc , xlab='Año')













