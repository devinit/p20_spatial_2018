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
    country_poly=subset(recent_dhs_1,DHSCC==dhscc)
    merged_points=over(country_points,country_poly)
    country_points$OBJECTID=merged_points$OBJECTID
    country_points$DHSREGEN=merged_points$DHSREGEN
    message(dhscc,recode,": ",sum(is.na(country_points$OBJECTID)))
    missingclusters=subset(country_points,is.na(OBJECTID))
    if(nrow(missingclusters)>0){
      for(j in 1:nrow(missingclusters)){
        missingcluster=missingclusters[j,]
        clust=missingcluster$DHSCLUST
        matchingpoly=country_poly[which.min(gDistance(missingcluster,country_poly,byid=T)),]
        if(matchingpoly$DHSREGEN=="Region 1" & matchingpoly$DHSCC=="CF"){
          country_points$OBJECTID[which(country_points$DHSCLUST==clust)]=363
          country_points$DHSREGEN[which(country_points$DHSCLUST==clust)]="Bangui"
        }else{
          country_points$OBJECTID[which(country_points$DHSCLUST==clust)]=matchingpoly$OBJECTID
          country_points$DHSREGEN[which(country_points$DHSCLUST==clust)]=matchingpoly$DHSREGEN
        }
      }
    }  
    # country_points=subset(country_points,!is.na(OBJECTID))
    country_points=country_points[,wanted.names]
    pointlist[[i-1]]=country_points
  })
}
point.list.complete = pointlist[!sapply(pointlist, is.null)] 
options(fill=T)
dhs_points=do.call(rbind,point.list.complete)
dhs_points$filename=paste0(dhs_points$DHSCC,"HR",dhs_points$recode,"FL")
save(dhs_points,file="E:/DHS mapping/dhs_points.RData")


write.csv(dhs_points@data[,c("DHSCC","DHSYEAR","DHSCLUST","OBJECTID","DHSREGEN","DHSREGNA","filename")],"project_data/dhspoints.csv",row.names=F,na="")
