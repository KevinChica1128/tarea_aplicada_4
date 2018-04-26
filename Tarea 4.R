#Tarea 4 Aplicada II
#Kevin García - Alejandro Vargas
install.packages("readr")
library("readr")
cadata <- read.csv("~/GitHub/tarea_aplicada_4/cadata.txt", sep="")
View(cadata)
sample(1:20640,1)
## El número aleatorio generado fue 15529 ##
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
          Dormitorios=cadata$Total_de_dormitorios,Poblacion=cadata$Poblacion,Hogares=cadata$Hogares)
R = cor(X)
R

#Factor de inflación de varianza:
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
#Población:
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

#ANALISIS DE VALORES PROPIOS
eigen(R)$values
k=max(eigen(R)$values)/min(eigen(R)$values)
ki=c(max(eigen(R)$values)/eigen(R)$values)
#Determinante de la matriz de correlaciones:
det(R)
#METODO (PCR) componentes principales
install.packages("pls")
library(pls)
regresion_pcr=pcr(formula=cadata$Valor_mediano_de_la_casa ~ cadata$Ingreso_mediano+cadata$Edad_mediana_de_la_vivienda+
                  cadata$Total_de_habitaciones+cadata$Total_de_dormitorios+cadata$Poblacion+cadata$Hogares
                  )
summary(regresion_pcr)
fitted(regresion_pcr, comps = 1:4)
resid(regresion_pcr, comps = 1:4)
coef(regresion_pcr, comps = 1:6)

## Eigenvalues can be extracted
eigenvals(regresion_pcr)

## screeplot method
screeplot(regresion_pcr)


#Método (PCR) paso a paso:
#La matriz X es:
X 
#La matriz X* (reescalada) es:
x1<-cadata$Ingreso_mediano
x2<-cadata$Edad_mediana_de_la_vivienda
x3<-cadata$Total_de_habitaciones
x4<-cadata$Total_de_dormitorios
x5<-cadata$Poblacion
x6<-cadata$Hogares
X1<-(x1-mean(x1))/sqrt(sum(x1^2)-(length(x1)*mean(x1)^2))
X2<-(x2-mean(x2))/sqrt(sum(x2^2)-(length(x2)*mean(x2)^2))
X3<-(x3-mean(x3))/sqrt(sum(x3^2)-(length(x3)*mean(x3)^2))
X4<-(x4-mean(x4))/sqrt(sum(x4^2)-(length(x4)*mean(x4)^2))
X5<-(x5-mean(x5))/sqrt(sum(x5^2)-(length(x5)*mean(x5)^2))
X6<-(x6-mean(x6))/sqrt(sum(x6^2)-(length(x6)*mean(x6)^2))
Xr<-cbind(X1,X2,X3,X4,X5,X6)
Y<-(y-mean(y))/sqrt(sum(y^2)-(length(y)*mean(y)^2))
modeloreescalado<-lm(Y~X1+X2+X3+X4+X5+X6)
modeloreescalado$coefficients
summary(modeloreescalado)
#X*t X en forma de correlacion es:
Rr<-t(Xr)%*%Xr
det(Rr)
#Estimación por PCR:
y<-cadata$Valor_mediano_de_la_casa
eigen(Rr)
T<-eigen(Rr)$vectors
Z<-Xr%*%T
A<-t(Z)%*%Z
alfae<-solve(A)%*%t(Z)%*%Y
alfaec<-c(1,1,1,0,0,0)*alfae
Betaest<-T%*%alfaec


Rr1<-t(X)%*%X
T1<-eigen(Rr1)$vectors
Z1<-X%*%T1
A1<-t(Z1)%*%Z1
alfae1<-solve(A1)%*%t(Z1)%*%y
Betaest1<-T1%*%alfae1
