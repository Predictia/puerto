#########################################################################
#### Par�metros de valor �nico para el modelo PUERTO

ts<-1   #Time step del modelo (d.)

tddmin<-4 #Temperatura m�nima para computar grados-d�a

tcor<-3 #Correcci�n aditiva de los valores de temperaturas de cl1 

Krs<-0.19 #Coeficiente de ajuste. 0.19=muy oce�nico;0.16=muy continental
# para la F�rmula de Hargreaves, para c�lculo de FR

Kcmax<-1.3 #Valor m�ximo del coeficiente del culivo Kc que  multiplica a ET0

pimx<-0.05 # pimx: Proporci�n diaria de biov0 ingerido a partir del cual se produce un
# desfronde m�ximo de lo muerto en pie
pimn<-0    #Proporci�n de biov0 ingerido por debajo del cual la tasa de desfronde
# es minima

######################################################################
### Par�metros de la siega
biov00<-5 #g MS verde/m2 m�ximos que quedan en pie tras la siega
biom00<-10 #g gMS seca/m2 m�ximos que quedan en pie tras la siega
ksv<-0.9 #Proporci�n de siega de MS viva cosechada
ksm<-0.8 #Proporci�n de siega de MS muerta cosechada


######################################################################
### Parametros de la parte animal

lintg<-26.8   # Biomasa de pasto (g/m2) por debajo de la cual no hay ingesti�n

EB<-18.4                        #Energia Bruta de cualquier alimento

qm<-0.8     #Coeficiente de metabolizaci�n de la energia

Kgest<-0.154                       #Efic de uso de EM para gestaci�n

#Valor energ�tico de la movilizaci�n de reservas corporales en todas las categorias
# (MJ de EN por kg de peso perdido)
ENmovil<-22.4

#Eficiencia de la utilizaci�n de la EM para movilizaci�n de reservas
Kreservas<-0.84

#Par�metros para calcular el �ndice de matorralizaci�n (IM)
IM0<-0 # Nivel de accesibilidad por debajo de la cual la accesibilidad es m�nima Fmos=Fmos0
IM1<-0.6 # Nivel de accesibilidad por encima de la cual que Fmos=1 (accesibilidad total)
Fmos0<-0 #Fmos minima

#Par�metros para estimar la producci�n de leche segun la Condici�n corporal (CC)
ccLmn<-1.75 #CC por debajo de la cual el animal no produce leche
ccLmx<-2.5 #CC por encima de la cual la producci�n de leche es m�xima

################################################################################
prvolatN<-0.5  #P�rdidas de N de la orina por volatilizaci�n
