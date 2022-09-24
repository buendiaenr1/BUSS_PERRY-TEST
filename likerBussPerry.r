
# Buss y Perry ************************************************************************
# Autor: 	Enrique Ricardo Pablo Buendia Lozada
# 		Facultad de Cultura Física, Benemérita Universidad Autónoma de Puebla
#		24 septiembre 2022	
# 
# *************************************************************************************

# Lee un archivo CSV con la informacion de la prueba BUSS y PERRY
# corrige posibles errores
# crea la evaluación correspondiente en puntos y crea la evaluación correspondiente
# en promedios

dat <- read.csv(file = 'hbp1.csv',sep=",")

# corregir captura y transcribir significado a numeros
for (j in 2:30){
dat[j]<- replace(dat[j],dat[j]=="1. Completamente falso",1)
dat[j]<- replace(dat[j],dat[j]=="1.Completamente falso",1)
dat[j]<- replace(dat[j],dat[j]=="l. Completamente falso",1)
dat[j]<- replace(dat[j],dat[j]=="2. Falso",2)
dat[j]<- replace(dat[j],dat[j]=="2.Falso",2)
dat[j]<- replace(dat[j],dat[j]=="2. falso",2)
dat[j]<- replace(dat[j],dat[j]=="3. Ni verdadero ni falso",3)
dat[j]<- replace(dat[j],dat[j]=="3.Ni verdadero ni falso",3)
dat[j]<- replace(dat[j],dat[j]=="4. Verdadero",4)
dat[j]<- replace(dat[j],dat[j]=="4.Verdadero",4)
dat[j]<- replace(dat[j],dat[j]=="5. Completamente Verdadero",5)
dat[j]<- replace(dat[j],dat[j]=="5.Completamente verdadero",5)
dat[j]<- replace(dat[j],dat[j]=="5. Completamente verdadero",5)
}
dat      # mostrar para verificar funcionamiento correcto

#sexo
dat[1] <- replace(dat[1],dat[1]=="h",0)
dat[1] <- replace(dat[1],dat[1]=="m",1)
dat[1]	# mostrar para verificar funcionamiento correcto



# Los ítems correspondientes por subescala son: 
# Ira (3, 7, 11, 15, 2, 22, 25), 
# Agresión física (1, 5, 9, 13, 17, 21, 24, 27, 29), 
# Agresión Verbal (6,18,10,14,19), 
# Hostilidad (4, 8, 12, 16, 20, 23, 26, 28) 
# (Buendia Lozada, Alatriste Flores, Islas Guerra, López Alonso, & Chávez Erives, 2019).

ira=c(3, 7, 11, 15, 2, 22, 25)
af=c(1, 5, 9, 13, 17, 21, 24, 27, 29)
av=c(6,18,10,14,19)
host=c(4, 8, 12, 16, 20, 23, 26, 28)
s1=0
s2=0
s3=0
s4=0
n=nrow(dat)
m1=0
m2=0
m3=0
m4=0
for (j in 1:n){
for (i in 2:30){
	try(if(which(ira  == i-1)) s1=s1+as.numeric(dat[j,i]),TRUE)
	try(if(which(af   == i-1)) s2=s2+as.numeric(dat[j,i]),TRUE)
	try(if(which(av   == i-1)) s3=s3+as.numeric(dat[j,i]),TRUE)
	try(if(which(host == i-1)) s4=s4+as.numeric(dat[j,i]),TRUE)
}
cat(" ira = ",s1," agresión física = ",s2," agresión verbal = ",s3," hostilidad = ",s4,"\n")
m1=m1+s1
m2=m2+s2
m3=m3+s3
m4=m4+s4
s1=0
s2=0
s3=0
s4=0
}
cat("\n Promedios = \n      ",m1/n," ",m2/n," ",m3/n," ",m4/n," \n")
