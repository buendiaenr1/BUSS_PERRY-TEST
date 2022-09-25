
# Buss y Perry ************************************************************************
# Autor: 	Enrique Ricardo Pablo Buendia Lozada
# 		Facultad de Cultura Física, Benemérita Universidad Autónoma de Puebla
#		25 septiembre 2022	
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
dat[j]<- replace(dat[j],dat[j]=="l. Completamente  falso",1)
dat[j]<- replace(dat[j],dat[j]=="1. Completamente  falso",1)
dat[j]<- replace(dat[j],dat[j]=="2. Falso",2)
dat[j]<- replace(dat[j],dat[j]=="2.Falso",2)
dat[j]<- replace(dat[j],dat[j]=="2. falso",2)
dat[j]<- replace(dat[j],dat[j]=="2.falso",2)
dat[j]<- replace(dat[j],dat[j]=="3. Ni verdadero ni falso",3)
dat[j]<- replace(dat[j],dat[j]=="3.Niverdadero ni falso",3)
dat[j]<- replace(dat[j],dat[j]=="3.Ni verdadero ni falso",3)
dat[j]<- replace(dat[j],dat[j]=="3.Ni verdadero Ni falso",3)
dat[j]<- replace(dat[j],dat[j]=="4. Verdadero",4)
dat[j]<- replace(dat[j],dat[j]=="4.Verdadero",4)
dat[j]<- replace(dat[j],dat[j]=="5. Completamente Verdadero",5)
dat[j]<- replace(dat[j],dat[j]=="5.Completamente verdadero",5)
dat[j]<- replace(dat[j],dat[j]=="5.Completamente Verdadero",5)
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

#interpretación de Buss 1969
interp<-c()
p1=round(m1/n,0)
p2=round(m2/n,0)
p3=round(m3/n,0)
p4=round(m4/n,0)



i_eima=27:200
i_eia=22:26
i_eim=18:21
i_eib=13:17
i_eimb=0:12

	try(if(which(i_eima  == p1)) interp=c(interp," muy alta "),TRUE)
	try(if(which(i_eia   == p1)) interp=c(interp," alta "),TRUE)
	try(if(which(i_eim   == p1)) interp=c(interp," media "),TRUE)
	try(if(which(i_eib   == p1)) interp=c(interp," baja"),TRUE)
	try(if(which(i_eimb  == p1)) interp=c(interp," muy baja "),TRUE)

af_eima=30:200
af_eia=24:29
af_eim=16:23
af_eib=12:15
af_eimb=0:11

	try(if(which(af_eima  == p2)) interp=c(interp," muy alta "),TRUE)
	try(if(which(af_eia   == p2)) interp=c(interp," alta "),TRUE)
	try(if(which(af_eim   == p2)) interp=c(interp," media "),TRUE)
	try(if(which(af_eib   == p2)) interp=c(interp," baja "),TRUE)
	try(if(which(af_eimb  == p2)) interp=c(interp," muy baja "),TRUE)

av_eima=18:200
av_eia=14:17
av_eim=11:13
av_eib=7:10
av_eimb=0:6

	try(if(which(av_eima  == p3)) interp=c(interp," muy alta "),TRUE)
	try(if(which(av_eia   == p3)) interp=c(interp," alta "),TRUE)
	try(if(which(av_eim   == p3)) interp=c(interp," media "),TRUE)
	try(if(which(av_eib   == p3)) interp=c(interp," baja "),TRUE)
	try(if(which(av_eimb  == p3)) interp=c(interp," muy baja "),TRUE)

ho_eima=32:200
ho_eia=26:31
ho_eim=21:25
ho_eib=15:20
ho_eimb=0:14

	try(if(which(ho_eima  == p4)) interp=c(interp," muy alta "),TRUE)
	try(if(which(ho_eia   == p4)) interp=c(interp," alta "),TRUE)
	try(if(which(ho_eim   == p4)) interp=c(interp," media "),TRUE)
	try(if(which(ho_eib   == p4)) interp=c(interp," baja "),TRUE)
	try(if(which(ho_eimb  == p4)) interp=c(interp," muy baja "),TRUE)
print(interp)
print(" ")

#interpretación general de Buss 1992
p=(p1+p2+p3+p4)/4
muybajo=0:51
bajo=52:67
medio=68:82
alto=83:98
muyalto=99:200

	try(if(which(muyalto  == round(p,0))) cat("General muy alta \n"),TRUE)
	try(if(which(alto     == round(p,0))) cat("General alta \n"),TRUE)
	try(if(which(medio    == round(p,0))) cat("General media \n"),TRUE)
	try(if(which(bajo     == round(p,0))) cat("General baja \n"),TRUE)
	try(if(which(muybajo  == round(p,0))) cat("General muy baja \n"),TRUE)


