########################################################################################
### Cálculo de radiación solar diaria en pixels 25m de toda Cantabria
########################################################################################
# La radiación se calcula a partir del mde


setwd("D:/C/PROYECTOS/Puerto2018")
library(data.table)
library(raster)
library(RSAGA) # Para el cálculo de la radiación solar con SAGA

env<-rsaga.env(path="C:/SAGA-GIS/saga_2.2.3_x64")
# env$version

# Cálculo de la Radiación diaria con SAGA #################################

#Año 2015. Se asume que todos los años se tendrá la misma radiación potencial
# (la determinada por las condiciones topográficas)
# as.numeric(as.IDate("1970-01-01"))
days<-as.IDate(16436:16800,origin=as.IDate("1970-01-01"))
fechas<-data.table(days,dia=mday(days),mes=month(days),año=year(days))

######################################

#Calcular la radiación total diaria del mde del site de 1km y guardar en un raster brick
setwd(paste0("./Inputs/sites/",site,"/Rasters"))
rr1<-brick()
for(i in 1:365){
rsaga.pisr2(in.dem ="./Rasters/mde1km.sdat",
            out.total.grid="r",
            latitude=43,unit="kJ/m2",time.step=12,
            start.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$año[i]),
            end.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$año[i]),
            day.step=1,env=env)
rr1<-addLayer(rr1,raster(readGDAL("r.sdat")))
file.remove(list.files(pattern="^r.\\b"))
projection(rr1)<-CRS("+init=epsg:25830")
save(rr1,file="./Rasters/rr1.Rdata")
}
##########################################################################

#Calcular la radiación total diaria del mde de 25m y guardar en data.tables
##1ªtabla con las coordenadas de cada grid (centroides): xydt
rsaga.pisr2(in.dem ="./Rasters/mde25m.sdat",
           out.total.grid="r",
           latitude=43,unit="kJ/m2",time.step=12,
           start.date=list(day=fechas$dia[i],month=fechas$mes[1],year=fechas$año[1]),
           end.date=list(day=fechas$dia[1],month=fechas$mes[1],year=fechas$año[1]),
           day.step=1,env=env)
rr25<-raster(readGDAL("r.sdat"))
projection(rr25)<-CRS("+init=epsg:25830")
xydt<-data.table(xyFromCell(rr25,1:ncell(rr25)))[,id:=.I][]
######

##2ªtabla: datos diarios de radiación para cada grid y día del año
dt<-data.table()
for(i in 11:365){
rsaga.pisr2(in.dem ="./Rasters/mde25m.sdat",
           out.total.grid="r",
           latitude=43,unit="kJ/m2",time.step=12,
           start.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$año[i]),
           end.date=list(day=fechas$dia[i],month=fechas$mes[i],year=fechas$año[i]),
           day.step=1,env=env)
rr25<-raster(readGDAL("r.sdat"))

# file.remove(list.files(pattern="^r.\\b"))
projection(rr25)<-CRS("+init=epsg:25830")

#xydt<-data.table(xyFromCell(rr25,1:ncell(rr25)))[,id:=.I][]
dt0<-data.table(id=xydt$id,dj=i,rad=as.integer(getValues(rr25)))[!is.na(rad)]
dt<-rbindlist(list(dt,dt0))
save(dt,file="./Rasters/dt.Rdata")
# write.table(dt,file="./Rasters/dt.txt",row.names=F)
}

############################################################################



