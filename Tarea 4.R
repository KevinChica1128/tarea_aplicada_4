#Tarea 4 Aplicada II
#Kevin Garc�a - Alejandro Vargas
install.packages("readr")
library("readr")
cadata <- read.csv("~/GitHub/tarea_aplicada_4/cadata.txt", sep="")
View(cadata)
sample(1:20640,1)
## El n�mero aleatorio generado fue 15529 ##
# Eliminamos las filas que no necesitamos, nos quedamos con las filas desde la 15529 hasta la 16029 #
cadata<- cadata[-c(1:15528,16029:20640),]

#Ajuste del modelo completo:
Regresion<- lm(cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
summary(Regresion)
#Valors ajustados y residuales del modelo:
yajustados<-fitted(Regresion)
residuales<-residuals(Regresion)

#Matriz de correlaciones:
X = cbind(Ingreso=cadata$Ingreso_mediano,Edad=cadata$Edad_mediana_de_la_vivienda,Habitaciones=cadata$Total_de_habitaciones,
          Dormitorios=cadata$Total_de_dormitorios,Poblacion=cadata$Poblacion,Hogares=cadata$Hogares,Latitud=cadata$Latitud,Longitud=cadata$Longitud)
R = cor(X)
R

#Factor de inflaci�n de varianza:
library('fmsb')
#Ingreso:
modi<-lm(cadata$Ingreso_mediano ~ cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
summary(modi)
VIF(modi)   #Paquete
Ri2=0.3613       #A mano
VIFi=(1-Ri2)^(-1)
#Edad:
mode<-lm(cadata$Edad_mediana_de_la_vivienda ~ cadata$Ingreso_mediano+cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
summary(mode)
VIF(mode)   #Paquete
Re2=0.2136       #A mano
VIFe=(1-Re2)^(-1)
#Habitaciones:
modh<-lm(cadata$Total_de_habitaciones ~ cadata$Edad_mediana_de_la_vivienda+cadata$Ingreso_mediano+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares)
summary(modh)
VIF(modh)   #Paquete
Rh2=0.8781       #A mano
VIFh=(1-Rh2)^(-1)
#Dormitorios:
modd<-lm(cadata$Total_de_dormitorios ~ cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+cadata$Ingreso_mediano+cadata$Poblacion+cadata$Hogares)
summary(modd)
VIF(modd)   #Paquete
Rd2=0.9809       #A mano
VIFd=(1-Rd2)^(-1)
#Poblaci�n:
modp<-lm(cadata$Poblacion ~ cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Ingreso_mediano+cadata$Hogares)
summary(modp)
VIF(modp)   #Paquete
Rp2=0.8238       #A mano
VIFp=(1-Rp2)^(-1)
#Hogares:
modho<-lm(cadata$Hogares ~ cadata$Edad_mediana_de_la_vivienda+cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Ingreso_mediano+cadata$Poblacion)
summary(modho)
VIF(modho)   #Paquete
Rho2=0.9822       #A mano
VIFho=(1-Rho2)^(-1)