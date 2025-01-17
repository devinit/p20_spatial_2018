list.of.packages <- c("jsonlite","data.table","sp","rgdal","rgeos","spdep")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

source("https://raw.githubusercontent.com/akmiller01/r_arc_json/master/arc_json.R")

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/p20_spatial_2018")
setwd(wd)

load("project_data/svy_boundaries.RData")

svy_boundaries = subset(svy_boundaries,SVYTYPE=="DHS")

svy_boundaries = svy_boundaries[order(svy_boundaries$DHSCC,-svy_boundaries$SVYYEAR),]
svy_boundaries$dup = duplicated(svy_boundaries$DHSCC)
svy_boundaries$cc_year = paste0(svy_boundaries$DHSCC,svy_boundaries$SVYYEAR)

recent = subset(svy_boundaries,!dup)$cc_year

recent_dhs_boundaries = subset(svy_boundaries,cc_year %in% recent)
save(recent_dhs_boundaries,file="project_data/recent_dhs_boundaries.RData")

recent_dhs_1 = subset(recent_dhs_boundaries,LEVELRNK==1)
indian_obj_ids = c(4426:4461)
if(sum(indian_obj_ids %in% recent_dhs_1$OBJECTID,na.rm=T)>0){
  stop("We've got a non-unique ID.")
}
recent_dhs_1$OBJECTID[which(is.na(recent_dhs_1$OBJECTID))] = indian_obj_ids
save(recent_dhs_1,file="project_data/recent_dhs_1.RData")
