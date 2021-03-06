##################################################################################
##################################################################################
### DATOS DE CLIMA DEL PERIODO a�o1-a�o2 PARA LAS TESELAS DE VEGETACI�N DE PUERTO
##Se deben tener los datos de radiaci�n (ejecutar previamente "PuertoRadiaci�n.R")

site<-"Tudanca" #Nombre del "site" para Puerto
a�o1<-2010
a�o2<-2016

puertoclima<-function(site,a�o1,a�o2){

library(data.table)
library(raster)
library(rgdal)
# setwd("D:/C/PROYECTOS/Puerto2018")

#1. Cargar los raster de radiaci�n

load(paste0("./Inputs/sites/",site,"/Rasters/rr25.Rdata"))
load(paste0("./Inputs/sites/",site,"/Rasters/rr1.Rdata"))
rr25<-round(rr25/1000,2) # pasarlos a MJ/m2/d
rr1<-round(rr1/1000,2) # pasarlos a MJ/m2/d
names(rr25)<-1:365
names(rr1)<-1:365
rr25r<-raster(rr25)

#1a. Cargar el shp de teselas del site
tes<-readOGR(dsn=file.path("./Inputs/sites",site),layer="teselas")
proj4string(tes)<-CRS("+init=epsg:25830")

#2. Reproyectar y clipear rr1 a rr25
rr1a25<-projectRaster(rr1,rr25r,method="ngb")

#3. Cargar los datos de temperaturas y precipitaci�n de Cantabria y las coordenadas
## de sus celdas de 1km aprox.

# load("./CantabriaClima/coorid.Rdata")
# porcion<-list.files(path="./CantabriaClima",pattern="^clima.")[1:10]
# cl<-data.table()
# for (i in 1:10){
# load(paste0("./CantabriaClima/",porcion[i]))
# cl<-rbindlist(list(cl,cl1[a�o>=a�o1 & a�o<=a�o2]))
# save(cl,coor,file=paste0("./CantabriaClima/clCantabria",a�o1,"_",a�o2,".Rdata"))
# }
load(paste0("./CantabriaClima/clCantabria",a�o1,"_",a�o2,".Rdata"))


#4. Escoger s�lo el clima de los de los cuadrados 1km del site

# create spatial points data frame
coordinates(coor) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(coor) <- TRUE
# coerce to raster
rcoor<-raster(coor)
projection(rcoor)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
#Reproyectar y clipear a rr25r
rcoor25s<-projectRaster(rcoor,rr25r,method="ngb")
idsunique<-unique(values(rcoor25s))
ids25<-data.table(ids=values(rcoor25s))
ids<-unique(ids25$ids)
#Se quitan los 29/2 de los a�os bisiestos para que siempre sean 365d/a�o
cls<-cl[id1 %in% ids][!(diam==29 & mes==2)]
rm(cl)
#################################

#5. A�adir a rcoor25s la informaci�n diaria de tmed, tmin y tmax (a�o a a�o)
# y calcular la ecuaci�n 1 (puerto, p28) en formato raster

a�os<-cls[,unique(a�o)]

#tmed

i<-2010
tmedtes2<-data.table()
for(i in a�os){
  clstmed0<-dcast.data.table(cls[a�o==i][,.(id1,dj,tmed)],id1~dj,value.var="tmed")
  clstmed0$id1<-as.numeric(clstmed0$id1)
  setkey(clstmed0,id1);setkey(ids25,ids)
  clstmed<-ids25[clstmed0][,ids:=NULL][]
  rtmed<-brick(rcoor25s)
  values(rtmed)<-data.matrix(clstmed)
  rtmed1<-rtmed-(0.463*(rr25-rr1a25)) #calcular la tmed corregida por la radiaci�n
  tmedtes<-data.table(t(extract(rtmed1,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(tmedtes)<-c(tes@data$ID_Mancha,"diaj")
  tmedtes1<-melt.data.table(tmedtes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="tmed",variable.factor=F)[,a�o:=i][]
  tmedtes2<-rbindlist(list(tmedtes2,tmedtes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}
#######
#tmin
tmintes2<-data.table()
for(i in a�os){
  clstmin0<-dcast.data.table(cls[a�o==i][,.(id1,dj,tmin)],id1~dj,value.var="tmin")
  clstmin0$id1<-as.numeric(clstmin0$id1)
  setkey(clstmin0,id1);setkey(ids25,ids)
  clstmin<-ids25[clstmin0][,ids:=NULL][]
  rtmin<-brick(rcoor25s)
  values(rtmin)<-data.matrix(clstmin)
  rtmin1<-rtmin-(0.463*(rr25-rr1a25)) #calcular la tmin corregida por la radiaci�n
  tmintes<-data.table(t(extract(rtmin1,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(tmintes)<-c(tes@data$ID_Mancha,"diaj")
  tmintes1<-melt.data.table(tmintes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="tmin",variable.factor=F)[,a�o:=i][]
  tmintes2<-rbindlist(list(tmintes2,tmintes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}
######
#tmax
tmaxtes2<-data.table()
for(i in a�os){
  clstmax0<-dcast.data.table(cls[a�o==i][,.(id1,dj,tmax)],id1~dj,value.var="tmax")
  clstmax0$id1<-as.numeric(clstmax0$id1)
  setkey(clstmax0,id1);setkey(ids25,ids)
  clstmax<-ids25[clstmax0][,ids:=NULL][]
  rtmax<-brick(rcoor25s)
  values(rtmax)<-data.matrix(clstmax)
  rtmax1<-rtmax-(0.463*(rr25-rr1a25)) #calcular la tmax corregida por la radiaci�n
  tmaxtes<-data.table(t(extract(rtmax1,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(tmaxtes)<-c(tes@data$ID_Mancha,"diaj")
  tmaxtes1<-melt.data.table(tmaxtes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="tmax",variable.factor=F)[,a�o:=i][]
  tmaxtes2<-rbindlist(list(tmaxtes2,tmaxtes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}

#####
#precipitaci�n
prectes2<-data.table() 
for(i in a�os){
  clsprec0<-dcast.data.table(cls[a�o==i][,.(id1,dj,prec)],id1~dj,value.var="prec")
  clsprec0$id1<-as.numeric(clsprec0$id1)
  setkey(clsprec0,id1);setkey(ids25,ids)
  clsprec<-ids25[clsprec0][,ids:=NULL][]
  rprec<-brick(rcoor25s)
  values(rprec)<-data.matrix(clsprec)
  prectes<-data.table(t(extract(rprec,tes,mean)))[,dia:=.I][] #calcular los valores medios por tesela
  names(prectes)<-c(tes@data$ID_Mancha,"diaj")
  prectes1<-melt.data.table(prectes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="prec",variable.factor=F)[,a�o:=i][]
  prectes2<-rbindlist(list(prectes2,prectes1))[,ID_Mancha:=as.integer(ID_Mancha)][]
}
#####
#Radiaci�n extraterrestre + orograf�a (salida de SAGA)
  radtes<-data.table(t(extract(rr25,tes,mean)))[,diaj:=.I][]
  names(radtes)<-c(tes@data$ID_Mancha,"diaj")
  radtes1<-melt.data.table(radtes,id.vars ="diaj",measure.vars=as.character(tes@data$ID_Mancha),
                            variable.name="ID_Mancha",value.name="radpot",variable.factor=F)[,
                            ID_Mancha:=as.integer(ID_Mancha)][]
  

  
#####  
setkey(tmedtes2,a�o,diaj,ID_Mancha)
setkey(tmintes2,a�o,diaj,ID_Mancha)
setkey(tmaxtes2,a�o,diaj,ID_Mancha)
setkey(prectes2,a�o,diaj,ID_Mancha)

ttes2<-tmedtes2[tmintes2][tmaxtes2][prectes2][,
                .(a�o,diaj,ID_Mancha,tmed,tmin,tmax,prec)]
setkey(ttes2,diaj,ID_Mancha)
setkey(radtes1,diaj,ID_Mancha)
ttes3<-ttes2[radtes1][order(a�o,diaj,ID_Mancha)][,
              Rs:=0.18*radpot*sqrt(tmax-tmin)][]


#Crear una tabla de fechas (quitando los d�as bisiestos)
# load("./CantabriaClima/clima01.Rdata")
# fechas<-unique(cl1[,.(dia,diam,mes,est,a�o)])[!(diam==29 & mes==2)][,
#    t:=.I][,dj:=1:365][]
# save(fechas,file="./Codigo/fechas.Rdata")
load("./Codigo/fechas.Rdata")
fechas<-fechas[a�o>=a�o1 & a�o<=a�o2][,t:=.I][] #ajustar t al periodo a�o1-a�o2
setkey(ttes3,diaj,a�o);setkey(fechas,dj,a�o)
cl1<-fechas[ttes3][,.(dia,diam,mes,est,a�o,t,dj,ID_Mancha,
                      tmed=round(tmed,1),tmin=round(tmin,1),tmax=round(tmax,1),
                      prec=round(prec,1),rg=round(Rs,1))][order(dia,ID_Mancha)]

save(cl1,file=paste0("./Inputs/sites/",site,"/clima.Rdata"))

# ggplot(cl1,aes(x=diaj,y=Rs,colour=ID_Mancha))+geom_line()+
#   geom_line(aes(y=radpot),colour="black")+
#     facet_wrap( ~ a�o, ncol=3)
#################################################
}








