########################################################################################
### C�lculo de los mapas de radiaci�n solar diaria del site
########################################################################################
# La radiaci�n se calcula a partir del mde
# Se calcula tanto a nivel de mde 1km como mde25m
# Una relaci�n entre rad1km y rad25m sirve para corregir las temperaturas (Pastos, p.28;
# funci�n siguiente: "puertoclima")
# De cara a ganar eficiencia, se parte de mde del �rea local donde se usar� puerto

site<-"PruebaPredictia" #Nombre del "site" para Puerto

puertoradiacion<-function(site){

# setwd("D:/C/PROYECTOS/Puerto2018")
library(data.table)
library(raster)
library(rgdal)
library(rgeos)
library(RSAGA) # Para el c�lculo de la radiaci�n solar con SAGA


# env<-rsaga.env(path="C:/programas/saga_2.2.1_x64")
# env<-rsaga.env(path="C:/SAGA-GIS/saga-4.0.1_x64")
env<-rsaga.env(path="C:/SAGA-GIS/saga_2.2.3_x64")
# env$version

# mde a diferentes resoluciones (25x25m y 1x1km)
# mde<-raster("D:/S1Can/mde/w001000.adf")
# mde_25m<-aggregate(mde,fact=5,method="bilinear") # Disminuir la resoluci�n del mde a 25x25m
# mde_1km<-aggregate(mde,fact=200,method="bilinear") # Disminuir la resoluci�n del mde a 1x1km
# writeRaster(mde_25m,file="mde25m.tif",overwrite=T)
# writeRaster(mde_1km,file="mde1km.tif",format="GTiff",overwrite=T)

# Cargar el shape con las teselas del site
tes<-readOGR(dsn=file.path(disk,"Inputs/sites",site),layer="teselas")
proj4string(tes)<-CRS("+init=epsg:25830")

#Crear un marco con una holgura de 1000m respecto a tes
e<-extent(tes)+c(-1000,1000,-1000,1000)

#Crear un shp con el extent
data = data.frame(f=0)
e1<-SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(
  cbind(x_coord=c(rep(e[1],2),rep(e[2],2),e[1]),y_coord=c(e[4],e[3],e[3],e[4],e[4])))),1))),data)
proj4string(e1)<-CRS("+init=epsg:25830")

writeOGR(e1,dsn=paste0(disk,"/Inputs/sites/",site,"/Rasters"),
         layer="e1",driver="ESRI Shapefile",overwrite=T)


#Reducir el mde al entorno del extent del site de puerto. Uso de SAGA
# rsaga.get.lib.modules("shapes_grid", env = rsaga.env(), interactive = FALSE)
# rsaga.get.usage("shapes_grid", 11)

rsaga.geoprocessor("shapes_grid",11,list(INPUT="./Rasters/mde1km.sgrd",
                  SHAPES=paste0(disk,"/Inputs/sites/",site,"/Rasters/e1.shp"),
                  OUTPUT=paste0(disk,"/Inputs/sites/",site,"/Rasters/mdep1km")))
rsaga.geoprocessor("shapes_grid",11,list(INPUT="./Rasters/mde25m.sgrd",
                  SHAPES=paste0(disk,"/Inputs/sites/",site,"/Rasters/e1.shp"),
                  OUTPUT=paste0(disk,"/Inputs/sites/",site,"/Rasters/mdep25m")))

# C�lculo de la Radiaci�n diaria con SAGA #################################

#A�o 2015. Se asume que todos los a�os se tendr� la misma radiaci�n potencial
# (la determinada por las condiciones topogr�ficas)
# as.numeric(as.IDate("1970-01-01"))
days<-as.IDate(16436:16800,origin=as.IDate("1970-01-01"))
fechas<-data.table(days,dia=mday(days),mes=month(days),a�o=year(days))

######################################

#Calcular la radiaci�n total diaria del mde del site de 1km y guardar en un raster brick
setwd(paste0(disk,"/Inputs/sites/",site,"/Rasters"))
rr1<-brick()
for(i in 1:365){
rsaga.pisr2(in.dem ="mdep1km.sdat",
            out.total.grid="r",
            latitude=43,unit="kJ/m2",time.step=12,
            start.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$a�o[i]),
            end.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$a�o[i]),
            day.step=1,env=env)
rr1<-addLayer(rr1,raster(readGDAL("r.sdat")))
file.remove(list.files(pattern="^r.\\b"))
projection(rr1)<-CRS("+init=epsg:25830")
save(rr1,file="rr1.Rdata")
}
##########################################################################

#Calcular la radiaci�n total diaria del mde de 25m y guardar en un brick
rr25<-brick()
for(i in 1:365){
rsaga.pisr2(in.dem ="mdep25m.sdat",
           out.total.grid="r",
           latitude=43,unit="kJ/m2",time.step=12,
           start.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$a�o[i]),
           end.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$a�o[i]),
           day.step=1,env=env)
rr25<-addLayer(rr25,raster(readGDAL("r.sdat")))
file.remove(list.files(pattern="^r.\\b"))
projection(rr25)<-CRS("+init=epsg:25830")
save(rr25,file="rr25.Rdata")
}

### FIN DE CALCULOS DE RADIACI�N
############################################################################
}


