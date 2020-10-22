# INTRODUCTION #######################################
# Nodali Ndraha, National Taiwan Ocean University
# Extracting data of chlA, sst, and sal from NASA data
# Check and set the directory of files
setwd("F:/NASA Data") # Set the global directory
# load libraries
# ncdf4 needs libnetcdf-dev netcdf-bin in Linux
# install.packages(c("ncdf4","reshape2"))
library(maptools)
library(ncdf4)
library(raster)
library(rgdal)
library(reshape2)
library(sf)
library(lubridate)

# set working directory
setwd("F:/2019_2020/chlA")
file.exists("chlA_QiguQigu.txt")     # caution new data will be appended to this file if it already exists
# file.rename("MODISA_sstQigu.txt","MODISA_sst.old")
file.remove("chlA_QiguQigu.txt")

# list and remove objects from workspace
ls()
rm(list = ls())

# set the study area in decimal degrees
lonmax <- 120.5
lonmin <- 119.5
latmax <- 23.50
latmin <- 22.50

setwd("F:/2019_2020/chlA")
# create a list of files and indicate its length
(f <- list.files(".", pattern="*.L3m_DAY_CHL_chlor_a_4km.nc",full.names=F))
(lf<-length(f))

# variable
var<-"chlor_a"

for (i in 1:lf) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f),f[i],sep=" "))
  # open netCDF file
  data<-nc_open(f[i])
  # extract data
  lon<-ncvar_get(data,"lon")
  lat<-ncvar_get(data,"lat")
  value<-ncvar_get(data,var)
  unit<-ncatt_get(data,var,"units")$value
  # matrix to data.frame
  dimnames(value)<-list(lon=lon,lat=lat)
  dat.var<-melt(value,id="lon")
  # select data from the study area taking out missing data
  # dat.varSAtmp<-na.omit(subset(dat.var,lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin))
  dat.varSAtmp<-subset(dat.var,lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin)
  # dat.varSAtmp<-na.omit(subset(dat.var,lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin))
  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  # prepare final data set
  dat.varSA<-data.frame(rep(as.integer(year,nrow(dat.varSAtmp))),rep(as.integer(month,nrow(dat.varSAtmp))),
                        dat.varSAtmp,rep(unit,nrow(dat.varSAtmp)),rep(var,nrow(dat.varSAtmp)),datemean)
  names(dat.varSA)<-c("year","month","lon","lat","value","unit","var", "date")
  # save csv file
  fe<-file.exists("chlA_QiguQigu.txt")
  write.table(dat.varSA,"chlA_QiguQigu.txt",row.names=FALSE,col.names=!fe,sep="\t",dec=".",append=fe)
  # close netCDF
  nc_close(data)
  # clean workspace
  rm(data,lon,lat,value,unit,dat.var,dat.varSAtmp,dateini,dateend,datemean,year,month,dat.varSA,fe)
}
rm(var,f,i,latmax,latmin,lf,lonmax,lonmin)

setwd("F:/2019_2020/sst")
(sst_files <- list.files(".", pattern="*.L3m.DAY.SST.sst.4km.nc",full.names=F))
(list_sst_files <- length(sst_files))

# variable
var<-"sst"
file.exists("MODISA_sstQigu.txt")     # caution new data will be appended to this file if it already exists
# file.rename("MODISA_sstQigu.txt","MODISA_sst.old")
file.remove("MODISA_sstQigu.txt")

lonmax <- 120.5
lonmin <- 119.5
latmax <- 23.50
latmin <- 22.50

for (i in 1:list_sst_files) {
  # progress indicator
  print(paste("Processing file",i,"from",length(sst_files),sst_files[i],sep=" "))
  # open netCDF file
  data<-nc_open(sst_files[i])
  # extract data
  lon<-ncvar_get(data,"lon")
  lat<-ncvar_get(data,"lat")
  value<-ncvar_get(data,var)
  unit<-ncatt_get(data,var,"units")$value
  # matrix to data.frame
  dimnames(value)<-list(lon=lon,lat=lat)
  dat.var<-melt(value,id="lon")
  # select data from the study area taking out missing data
  dat.varSAtmp<-subset(dat.var,lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin)
  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  # prepare final data set
  dat.varSA<-data.frame(dat.varSAtmp,rep(unit,nrow(dat.varSAtmp)),rep(var,nrow(dat.varSAtmp)), datemean)
  names(dat.varSA)<-c("lon","lat","value","unit","var", "date")
  # save csv file
  fe<-file.exists("MODISA_sstQigu.txt")
  write.table(dat.varSA,"MODISA_sstQigu.txt",row.names=FALSE,col.names=!fe,sep="\t",dec=".",append=fe)
  # close connection
  nc_close(data)
  # clean workspace
  rm(data,lon,lat,value,unit,dat.var,dat.varSAtmp,dateini,dateend,datemean,year,month,dat.varSA,fe)
}
rm(var,list_sst_files,i,latmax,latmin,sst_files,lonmax,lonmin)


# Salinity ##############
setwd("F:/2019_2020/sal")
(sal_files <- list.files(".", pattern="*_8DAYS_V4.3.nc",full.names=F))
(list_sal_files <- length(sal_files))

# variable
var<-"smap_sss"
file.exists("MODISA_salQigu.txt")     # caution new data will be appended to this file if it already exists
# file.rename("MODISA_sstQigu.txt","MODISA_sst.old")
file.remove("MODISA_salQigu.txt")

lonmax <- 120.5
lonmin <- 119.5
latmax <- 23.50
latmin <- 22.50

for (i in 1:list_sal_files) {
  # progress indicator
  print(paste("Processing file",i,"from",length(sal_files),sal_files[i],sep=" "))
  # open netCDF file
  data<-nc_open(sal_files[i])
  # extract data
  lon<-ncvar_get(data,"longitude")
  lat<-ncvar_get(data,"latitude")
  value<-ncvar_get(data,var)
  unit<-ncatt_get(data,var,"units")$value
  # matrix to data.frame
  dimnames(value)<-list(lon=lon,lat=lat)
  dat.var<-melt(value,id="lon")
  # select data from the study area taking out missing data
  dat.varSAtmp<-subset(dat.var,lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin)
  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend, "%Y-%jT%H:%M:%OS"), as.Date(dateini, "%Y-%jT%H:%M:%OS")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  # prepare final data set
  dat.varSA<-data.frame(dat.varSAtmp,rep(unit,nrow(dat.varSAtmp)),rep(var,nrow(dat.varSAtmp)), datemean)
  names(dat.varSA)<-c("lon","lat","value","unit","var", "date")
  # save csv file
  fe<-file.exists("MODISA_salQigu.txt")
  write.table(dat.varSA,"MODISA_salQigu.txt",row.names=FALSE,col.names=!fe,sep="\t",dec=".",append=fe)
  # close connection
  nc_close(data)
  # clean workspace
  rm(data,lon,lat,value,unit,dat.var,dat.varSAtmp,dateini,dateend,datemean,year,month,dat.varSA,fe)
}
rm(var,list_sal_files,i,latmax,latmin,sal_files,lonmax,lonmin)

MODISA_salQigu <- MODISA_salQigu %>% filter(!is.na(value))
salData <- ddply(MODISA_salQigu, .(date), summarize,  mean = mean(value), max = max(value), min = min(value))
glimpse(salData)
salData$date <- as.Date(salData$date)
write.csv(salData,"F:/2019_2020/salData.csv", row.names = FALSE)

chlA_QiguQigu <- chlA_QiguQigu %>% filter(!is.na(value))
chlAData <- ddply(chlA_QiguQigu, .(date), summarize,  mean = mean(value, na.rm=T), 
                  max = max(value, na.rm=T), min = min(value, na.rm=T))
glimpse(chlAData)
chlAData$date <- as.Date(chlAData$date)
write.csv(chlAData,"F:/2019_2020/chlAData.csv", row.names = FALSE)
