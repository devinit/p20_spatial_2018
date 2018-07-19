source("https://raw.githubusercontent.com/akmiller01/r_arc_json/master/arc_json.R")

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/p20_spatial_2018")
setwd(wd)

load("project_data/svy_ids.RData")

if(!file.exists("project_data/svy_boundaries.RData") && file.exists("project_data/svy_boundaries_1.RData")){
  load("project_data/svy_boundaries_1.RData")
  load("project_data/svy_boundaries_2.RData")
  load("project_data/svy_boundaries_3.RData")
  svy_boundaries = rbind(svy_boundaries_1,svy_boundaries_2,svy_boundaries_3)
  save(svy_boundaries,file="project_data/svy_boundaries.RData") 
}

if(file.exists("project_data/svy_boundaries.RData")){
  load("project_data/svy_boundaries.RData")
  collected_ids = unique(svy_boundaries$SVYID)
  svy_ids = subset(svy_ids, !SVYID %in% collected_ids)
}

# Returning errors consistently
svy_ids = subset(svy_ids, !SVYID %in% c(355, 490, 515) )

shape.list = list()

if(nrow(svy_ids)>0){
  for(i in 1:nrow(svy_ids)){
    SVYID = svy_ids$SVYID[i]
    filename = svy_ids$filename[i]
    query = paste0("SVYID = ",SVYID,"")
    Sys.sleep(30)
    try(
      {
        country_spatialdf = dhsQuery(query)
        country_spatialdf$filename = filename
        shape.list[[i]] = country_spatialdf
      }
    )
  }
}


if(length(shape.list)>0){
  shape.list.complete = shape.list[!sapply(shape.list, is.null)] 
  
  if(file.exists("project_data/svy_boundaries.RData")){
    svy_boundaries_tmp = do.call(rbind,shape.list.complete)
    svy_boundaries = rbind(svy_boundaries,svy_boundaries_tmp)
    save(svy_boundaries,file="project_data/svy_boundaries.RData")
  }else{
    svy_boundaries = do.call(rbind,shape.list.complete)
    save(svy_boundaries,file="project_data/svy_boundaries.RData") 
  }
}
