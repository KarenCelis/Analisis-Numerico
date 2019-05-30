library(paqueteRadiacion)
rm(list=ls())
# TIPO 2: encabezado una linea + datos
datosParaFormulas <- read.table("/Users/davidlopez/desktop/ShinyExcel/DatosBogota.csv", skip = 0, header = TRUE, sep =',')
datosParaFormulas
datosOriginales <- read.table("/Users/davidlopez/desktop/ShinyExcel/RealesBogota.csv",skip = 0, header = TRUE, sep =',')
datosOriginales

meses <- c()
G=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
p=c() #albedo
G0=c() #extra-atmospheric
Gdh=c() #Difusa radiacion solar horizontal
Gh=c() #Total radiacion  horizontal
B=c() #angulo de inclinacion
te= c()#angulo entre el radio incidente
tez=c() #zenith
result=c()
resultOriginales=c()

result = paqueteRadiacion::Modelo(datosParaFormulas[1], datosParaFormulas[2], datosParaFormulas[3], datosParaFormulas[4], datosParaFormulas[5], datosParaFormulas[7], datosParaFormulas[8] )

#for para llena arreglo de datos originales
for (i in 1:12)
{
  resultOriginales[i] <- datosOriginales[i+3]
}
x<-c(1,2,3,4,5,6,7,8,9,10,11,12) #numero del mes
plot(x,result,pch=20) #grafica
error=c() #arreglo de error local
as.numeric(resultOriginales)
as.numeric(result)
for (i in 1:12)
{
  r <- as.numeric(resultOriginales[i])
  r1 <- as.numeric(result[i])
  err<-((r-r1)/r)*100
  print(err)
  error[i]<-err
}
error
result
resultOriginales
plot(x,resultOriginales,pch=20)
