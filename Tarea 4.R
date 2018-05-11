#Tarea 4 Aplicada II
#Kevin García - Alejandro Vargas
library(MASS)
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
summary(Regresion)$sigma^2
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
eigen(R)
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
y<-cadata$Valor_mediano_de_la_casa
Y<-(y-mean(y))/sqrt(sum(y^2)-(length(y)*mean(y)^2))
modeloreescalado<-lm(Y~X1+X2+X3+X4+X5+X6)
modeloreescalado$coefficients
summary(modeloreescalado)
#X*t X en forma de correlacion es:
Rr<-t(Xr)%*%Xr
det(Rr)
#Estimación por PCR (Estandarizados):
eigen(Rr)
T<-eigen(Rr)$vectors
Z<-Xr%*%T
A<-t(Z)%*%Z
alfae<-solve(A)%*%t(Z)%*%Y
alfaec<-c(1,1,1,0,0,0)*alfae
Betaest<-T%*%alfaec
mean(Y)

#Estimación por PCR(escala original)
xbarra<-c(mean(x1),mean(x2),mean(x3),mean(x4),mean(x5),mean(x6))
Betaori<-c()
Betaori[1]=Betaest[1]*(sd(y)/sd(x1))
Betaori[2]=Betaest[2]*(sd(y)/sd(x2))
Betaori[3]=Betaest[3]*(sd(y)/sd(x3))
Betaori[4]=Betaest[4]*(sd(y)/sd(x4))
Betaori[5]=Betaest[5]*(sd(y)/sd(x5))
Betaori[6]=Betaest[6]*(sd(y)/sd(x6))

Beta0ori=mean(y)-(t(Betaori)%*%xbarra)
betas<-c(Beta0ori,Betaori)
sigma=(1/(500-7))*(t(y)%*%y-(t(betas)%*%t(X_1col)%*%y))
sqrt(sigma)

#varianza MCO (datos normales):
X_1col=cbind(1,X)
var_lm1=(1/(500-7))*((t(y)%*%y)-(t(Regresion$coefficients)%*%t(X_1col)%*%y))
summary(Regresion)$sigma^2
Vbetalm=solve(t(X_1col)%*%X_1col)*6724487923

#varianza PCR con tres componentes (datos normales):
var_pcr=(1/(500-7))*((t(y)%*%y)-(t(c(coef(regresion_pcr, comps = 3)))%*%t(X)%*%y))
VbetaPCR=(T%*%solve(A)%*%t(T))*114199000000
t(c(coef(regresion_pcr, comps = 3)))

#varianza MCO (datos estandarizados):
X_1cole=cbind(1,Xr)
var_lm1e=(1/(500-7))*((t(Y)%*%Y)-(t(modeloreescalado$coefficients)%*%t(X_1cole)%*%Y))
summary(modeloreescalado)$sigma^2
Vbetalme=solve(t(X_1cole)%*%X_1cole)*0.0009279279

#Varianza PCR con tres componentes (datos estandarizados):
var_pcre=(1/(500-7))*((t(Y)%*%Y)-((t(Betaestint))%*%t(X_1cole)%*%Y))
VbetaPCRe=(T%*%solve(A)%*%t(T))*0.001179825
Betaestint<-c(0,Betaest)

#R2 de os dos modelos (PCR y MCO):
X1d<-(x1-mean(x1))
X2d<-(x2-mean(x2))
X3d<-(x3-mean(x3))
X4d<-(x4-mean(x4))
X5d<-(x5-mean(x5))
X6d<-(x6-mean(x6))
Xd<-cbind(X1d,X2d,X3d,X4d,X5d,X6d)
yd<-(y-mean(y))
xdtxd<-t(Xd)%*%Xd
xdtyd<-t(Xd)%*%yd
B1M<-solve(t(Xd)%*%Xd)%*%t(Xd)%*%yd
B1P<-Betaori
SCRdM<-t(B1M)%*%t(Xd)%*%yd
SCRdP<-t(B1P)%*%t(Xd)%*%yd
SCTd<-t(yd)%*%yd
R2M<-SCRdM/SCTd
R2P<-SCRdP/SCTd


