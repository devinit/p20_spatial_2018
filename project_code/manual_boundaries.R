list.of.packages <- c("data.table","sp","rgdal","rgeos","spdep")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "C:"
}

wd = paste0(prefix,"/git/p20_spatial_2018")
setwd(wd)

load("project_data/svy_boundaries.RData")

manual_folder = "project_data/manual_shapefiles/"

dirs = list.dirs(manual_folder)
dirs = dirs[2:length(dirs)]

needed.columns = names(svy_boundaries)

manual.list = list()
manual.index = 1
for(dir in dirs){
  filename = basename(dir)
  shps = list.files(dir,pattern="*.shp$",full.names=T)
  for(shp in shps){
    shape = readOGR(shp)
    SVYID = shape$SVYID[1]
    if(!SVYID %in% svy_boundaries$SVYID){
      shape$filename = filename
      crs.geo <- CRS("+proj=longlat +datum=WGS84")
      proj4string(shape) <- crs.geo
      if("REGNAME" %in% names(shape)){
        shape$DHSREGEN = shape$REGNAME
      }
      have.columns = names(shape)
      missing.cols = setdiff(needed.columns,have.columns)
      extra.cols = setdiff(have.columns,needed.columns)
      for(missing.col in missing.cols){
        shape@data[,missing.col] = NA
      }
      for(extra.col in extra.cols){
        shape@data[,extra.col] = NULL
      }
      manual.list[[manual.index]] = shape
      manual.index = manual.index +1  
    }
  }
}


if(length(manual.list)>0){
  manual_boundaries = do.call(rbind,manual.list)
  svy_boundaries = rbind(svy_boundaries,manual_boundaries)
  save(svy_boundaries,file="project_data/svy_boundaries.RData")
  
  boundaries_count = nrow(svy_boundaries)
  cut = round(boundaries_count/3)
  svy_boundaries_1 = svy_boundaries[c(1:cut),]
  svy_boundaries_2 = svy_boundaries[c(cut+1:cut*2),]
  svy_boundaries_3 = svy_boundaries[c(((cut*2)+1):boundaries_count),]
  save(svy_boundaries_1,file="project_data/svy_boundaries_1.RData")
  save(svy_boundaries_2,file="project_data/svy_boundaries_2.RData")
  save(svy_boundaries_3,file="project_data/svy_boundaries_3.RData")  
}
