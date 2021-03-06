
#############################################################################
### Funciones utilizada en la parte animal

ED<-function(Dig) EB*Dig        #Energia digestible
EM<-function(Dig) EB*Dig*0.8    #Energia metabolizable
Qm<-function(Dig) Dig*0.8       #Metabolicidad


##########################################################################

## Las funciones ss (Km,Kl,KG) no se utilizan pq se han pasado a la
## tabla ZA6_Necesidades
#Efic de uso de EM para mantenimiento y andar: Para pasar de EM a EN
Km<-function(Especie,Dig){
  ifelse(Especie==1,0.35*Dig*0.8+0.603,0.35*Dig*0.8+0.503)
}                              

#Efic de uso de EM para lactaci�n: Para pasar de EM a EN
Kl<-function(Especie,Dig){
  ifelse(Especie==1,0.35*Dig*0.8+0.52,0.35*Dig*0.8+0.42)
}                             

#Eficiencia de uso de la EM para ganancia de peso en 1)animales en crecimiento, 
# 2)hembras lactantes, � 3) animales adultos no lactantes 
KG<-function(Categoria,Especie,Dig){
  ifelse(Categoria==1,ifelse(Especie==1,0.95*0.35*Dig*0.8+0.52,0.95*0.35*Dig*0.8+0.42),
         ifelse(Categoria==3,ifelse(Especie==1,0.78*Dig*0.8+0.106,0.78*Dig*0.8+0.006),
                ifelse(Especie==1,0.88*0.78*Dig*0.8+0.106,0.88*0.78*Dig*0.8+0.006)))
}

###################################################################################

#Funci�n correctora de la producci�n de leche en animales lactantes seg�n
#su condici�n corporal. Usada para corregir ENl
#Considera m�xima producci�n de leche a partir de cc=3 y prod=0 cuando cc=1.75
#Entre estos dos valores, relaci�n lineal
Kccl<-function(cc,Lmx,ccLmx,ccLmn){
  ifelse(cc>ccLmx,1,ifelse(cc<ccLmn,0,Lmx+((Lmx/(ccLmx-ccLmn))*(cc-ccLmx))))
}

#Correcci�n de necesidades segun condici�n corporal
CorCC<-function(cc) 1+((cc-3)*0.2)

#########################################################################################

#Correcci�n de ingesti�n segun condici�n corporal
#A los animales gordos se les reduce su ingesti�n
CorccI<-function(cc){
  ccImx<-2.5  #CC por debajo de la cual la ingesti�n potencial es m�xima
  ccImn<-3.5    #CC a la que la ingesti�n potencial es m�nima
  prImn<-.5   #Proporci�n de la ingesti�n potencial a ccImn
  ifelse(cc<ccImx,1,ifelse(cc>ccImn,.5,1+((1-prImn)/(ccImx-ccImn))*(cc-ccImx)))
}
