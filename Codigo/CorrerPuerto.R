#############################    MODELO PUERTO    ##############################
################################################################################
#### ARCHIVO PARA CORRER FUNCIONES DE DISTINTOS ASPECTOS DEL MODELO PUERTO
### Crecimiento/Senescencia/Ingestión del pasto
# Sys.setlocale("LC_NUMERIC", "Spanish_Spain.1252")  #para correr "fread"

disk<-"D:/C/PROYECTOS/Puerto2018"
# site<-"PruebaPredictia2"
site<-"Tudanca"
año1<-2010
año2<-2016
opcion<-2 #Procesado de tablas de inicio según como se exprese el tiempo
# 1: periodos explícitos: dia/mes inicial y dia/mes final
# 2: Se expresa con t explícita, pero a una escala de t=10 días: transformar a t=1 día #p.ej:Tudanca

#################################################################################
#### PREVIO PUERTO: CARGAR INFO DE RADIACIÓN-CLIMA-SUELO PARA UN NUEVO SITIO ####
#################################################################################
#setwd(disk)

##Cargar la radiación. Sólo para correrlo una vez:
# source(file.path(disk,"Codigo/PuertoRadiacion.R"))
# puertoradiacion(site)

##Cargar el clima. Solo para correrlo una vez despues de correr puertoradiacion:
# source(file.path(disk,"Codigo/PuertoClima.R"))
# puertoclima(site,año1,año2)

##Cargar el suelo. Solo para correrlo una vez:
# source(file.path(disk,"Codigo/PuertoSuelo.R"))
# puertosuelo(site)
##################################################################################
##################################################################################

setwd(file.path(disk,"Inputs/sites",site))
tmax<-365*2
tmax<-365*7      #Número de ts a correr el modelo
# i<-366      #Para hacer pruebas (debugging) Corresponde a las t

source(file.path(disk,"Codigo/Plant.R"))
system.time(pf<-plantf(site,tmax))


################################################################################
################################################################################
