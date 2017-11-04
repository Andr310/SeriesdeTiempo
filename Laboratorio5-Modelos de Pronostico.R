#Simulación de Procesos ARIMA 

##  Código en R para simular un proceso de media móvil de orden 1 MA(1)
#Simulación de un proceso MA(1) con theta=0.5

ts.sim1=arima.sim(list(order=c(0,0,1), ma=.5), n=100)
#Después de simular el proceso, exportar el mismo en formato csv.
write.csv(ts.sim1, "C:/Users/Andre Chavez/Desktop/Curso Series Temporales/ts.sim1.csv")
plot(ts.sim1, ylab=" ", main=(expression(MA(1)~~~theta==+.5)))
acf(ts.sim1, main="Autocorrelación Simple",ylim=c(-.5,.5),ci.col="black",ylab="")
pacf(ts.sim1,main="Autocorrelación Parcial",ylim=c(-.5,.5),ci.col="black")


#Si queremos estimar los parámetros usamos la función arima
Est<-arima(ts.sim1,order = c(0,0,1))


#Por último para determinar las predicciones
forecast(Est,2)
plot(forecast(Est,h=3),col="red")


##  Codigo en R para simular un proceso auroregresivo de orden 1 AR(1)
# Simulacion de un proceso AR(1) con phi=0.4

ts.sim2=arima.sim(list(order=c(1,0,0), ar=.4), n=100)
#Después de simular el proceso, exportar el mismo en formato csv.
write.csv(ts.sim2, "C:/Users/Andre Chavez/Desktop/Curso Series Temporales/ts.sim2.csv")
plot(ts.sim2, ylab=" ", main=(expression(AR(1)~~~phi==+.4)))
acf(ts.sim2, main="Autocorrelación Simple",ylim=c(-1,1),ci.col="black",ylab="")
pacf(ts.sim2,main="Autocorrelación Parcial",ylim=c(-.5,.5),ci.col="black")


