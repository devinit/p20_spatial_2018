list.of.packages <- c("jsonlite","data.table","sp","rgdal","rgeos","spdep")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/p20_spatial_2018")
setwd(wd)

load("project_data/recent_dhs_1.RData")

point_dirs=list.dirs("E:/DHS GPS",full.names=T)

crs.geo <- CRS("+proj=longlat +datum=WGS84")
pointlist=list()
wanted.names=c("DHSID","DHSCC","DHSYEAR","DHSCLUST","CCFIPS","ADM1FIPS"
,"ADM1FIPSNA","ADM1SALBNA","ADM1SALBCO","ADM1DHS","ADM1NAME","DHSREGCO"
,"DHSREGNA","SOURCE","URBAN_RURA","LATNUM","LONGNUM","ALT_GPS"
,"ALT_DEM","DATUM","recode","OBJECTID","DHSREGEN")
for(i in 2:length(point_dirs)){
  dir=point_dirs[i]
  dir_basename=basename(dir)
  dhscc=substring(dir_basename,1,2)
  recode=substring(dir_basename,5,6)
  shp=list.files(dir,pattern="*.shp$",ignore.case = T,full.names=T)
  try({
    country_points=readOGR(shp)
    country_points$recode=recode
    proj4string(country_points) <- crs.geo
    #0.002 decimal degrees is approx 200 meters near the equator
    country_buffer_urban=gBuffer(subset(country_points,URBAN_RURA=="U"),width=0.02,byid=T)
    country_buffer_rural=gBuffer(subset(country_points,URBAN_RURA=="R"),width=0.05,byid=T)
    country_buffers=rbind(country_buffer_rural,country_buffer_urban)
    country_poly=subset(recent_dhs_1,DHSCC==dhscc)
    merged_buffers=over(country_buffers,country_poly)
    country_buffers$OBJECTID=merged_buffers$OBJECTID
    country_buffers$DHSREGEN=merged_buffers$DHSREGEN
    message(dhscc,recode,": ",sum(is.na(country_buffers$OBJECTID)))
    # country_buffers=subset(country_buffers,!is.na(OBJECTID))
    country_buffers=country_buffers[,wanted.names]
    pointlist[[i-1]]=country_buffers
  })
}
point.list.complete = pointlist[!sapply(pointlist, is.null)] 
options(fill=T)
dhs_points=do.call(rbind,point.list.complete)
save(dhs_points,file="E:/DHS mapping/dhs_points_buffers.RData")

write.csv(dhs_points[,c("DHSCC","DHSYEAR","DHSCLUST","OBJECTID","DHSREGEN","DHSREGNA")],"dhspoints.csv",row.names=F,na="")
