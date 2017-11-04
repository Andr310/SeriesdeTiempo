################################################################################
####################### MODELOS ARIMA Y SARIMA EN R ############################
################################################################################

#Autor : Andre Chavez

#Presentacion de Modelos ARIMA y SARIMA en R
library(ggplot2)
library(TSA)
library(forecast)
library(scales)
library(stats)
library(urca)


#Leer la serie de tiempo, desde un archivo csv.

pea=read.csv("pea.csv",header = F,sep="",dec=",")
View(pea)
dim(pea)

##Creacion de una serie de tiempo
peats<-ts(pea,start=c(2002,1),end=c(2009,12),frequency=12)
peats
plot(peats)


auto.arima(peats)
################################################################################
################################ IDENTIFICACION ################################
################################################################################

############
# ORIGINAL #
############

# Plot
plot(peats,col="green",
     main="pea (Enero 1991 - Diciembre 2009)",
     ylab="pea",
     xlab="Años")

#Grafico de Autocorrelaciones Simples y Parciales
# ACF
acf(peats,lag.max=100,
    xaxp=c(0,27,27), # CONSIDERAR N/4
    main="ACF de la serie pea",
    ylab="Autocorrelaciones",
    xlab="Retardo")


# PACF
pacf(peats,lag.max=40,
     xaxp=c(0,27,27),
     main="PACF de la serie pea",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")
#Observamos que las autocorrelaciones de la serie original decaen muy lentamente, por otro lado
#la primera autocorrelacion es muy proxima a la unidad (0.988). Ambos resultados son indicios de 
#no estacionaridad por nivel. Por ende sera necesario aplicar diferencias finitas.


#Test de Dickey-Fuller para Estacionariedad
#Prueba para observar si una serie es estacionaria
#Ho: La serie no es estacionaria
#H1: La serie es estacionaria
adf.test(peats)

df.tendencia<-ur.df(log(peats), lags = 2, type = "trend")
summary(df.tendencia)


##############
# 1 DIFF REG #
##############


#Por lo visto anteriormente apliquemos una diferencia en la parte regular

# Diferencio la serie 1  vez regularmente
peats_d1r<-diff(peats,12,1)

# Plot
plot(peats_d1r,
     main="Grafico de la serie pea diferenciada una vez regularmente",
     ylab="pea",
     xlab="Tiempo")
# Se estabiliza medianamente en nivel pero no del todo en variabilidad
# No necesitaremos diferenciacion estacional

# ACF
acf(peats_d1r,lag.max=40,
    xaxp=c(0,27,27),
    main="ACF de la serie",
    ylab="Autocorrelaciones",
    xlab="Retardo")

# PACF
pacf(peats_d1r,lag.max=40,
     xaxp=c(0,32,32),
     main="PACF de la serie",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")


# Diferencio la serie 1  vez estacionalmente
peats_d1e<-diff(diff(peats_d1r),12,2)


# ACF
acf(peats_d1e,lag.max=30,
    xaxp=c(0,27,27),
    main="ACF de la serie",
    ylab="Autocorrelaciones",
    xlab="Retardo")

# PACF
pacf(peats_d1e,lag.max=30,
     xaxp=c(0,27,27),
     main="PACF de la serie",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")

# Parte regular: AR y MA 
# Parte estacional: AR y MA 
# (1,_,1)(1,_,1)[12]
##########################################################################
################################# AJUSTE #################################
##########################################################################

# Ajustamos un modelo SARIMA(0,1,1)(0,1,1)[12]
pea_fit<-Arima(pea,order=c(1,0,0),seasonal=list(order=c(2,2,0),period=12))
pea_fit
summary(pea_fit)
#Me da los indicadores del ajuste del Modelo, respecto al rango observado. 

############################################################################
################################ VALIDACI?N ################################
############################################################################

# Plot de residuales
plot(pea_fit$residuals,
     main="",
     ylab="",
     xlab="Tiempo", type="o")

# Normalidad de residuos
qqnorm(pea_fit$residuals)
qqline(pea_fit$residuals)
ks.test(pea_fit$residuals,"pnorm")
#Rechazamos la Normalidad de Residuos.


# ACF de residuales
acf(pea_fit$residuals,lag.max=40,
    xaxp=c(0,32,32),
    main="",   
    ylab="Autocorrelacion",
    xlab="Retardo")

pacf(pea_fit$residuals,lag.max=40,
     xaxp=c(0,32,32), #n/4
     main="PACF de la serie",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")

# Ljung-Box test
Box.test(pea_fit$residuals,lag=1,type="Ljung-Box",fitdf=0)
## H0: Los residuos se distribuyen de forma independiente

# Box y Pierce test
Box.test(pea_fit$residuals,lag=1,type="Box-Pierce",fitdf=0)
## H0: Ausencia de autocorrelaci?n en los residuos.

# Serie original, estimacion y pronosticos con ic
plot(forecast(pea_fit,h=12),col="red")
lines(pea_fit$fitted,col="green")
forecast(pea_fit,h=12)
sse=sum((pea-as.numeric(pea_fit$fitted))^2)
sse/228

#Medidas de Bondad de Ajuste
#SSE : Suma de Cuadrado de Errores
#MSE : Suma de Cuadrado de Errores/n
#MAD : Desviacion Absoluta Media, precis?n del pron?stico.
#MAPE: Eror Absoluto entre el valor real/n

