list.of.packages <- c("data.table","readr","plyr","data.table")
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

regiontrends=read.csv("project_data/regionswide20180802.csv")
regiontrends$trends=0
regiontrends$trends[which(regiontrends$ext.growthrate>0)]=1
regiontrends=regiontrends[,c("OBJECTID","CNTRYNAMEE","trends","DHSREGEN")]

latest=read.csv("project_data/recent_region.csv")
latest=join(latest, regiontrends, by=c("OBJECTID"))
latest$leftbehind=0
latest$leftbehind[which(latest$ExtremeHC>.20 &latest$trends==1)]=1
latest$leftbehind[which(latest$ExtremeHC>.45)]=1

populations=read.csv("project_data/recent_regional_poor_pop.csv")
# setnames(populations,"region","DHSREGEN")
latest=join(latest,populations,by=c("OBJECTID"))

pop_reg_left_behind=data.table(latest)[,.
                                       (pops=sum(regionpop)
                                        ,national_population=mean(totalpop)
                                         ),by=c("CNTRYNAMEE","leftbehind")]
pop_reg_left_behind$pop_left_behind_reg=pop_reg_left_behind$pops/pop_reg_left_behind$national_population
pop_in_reg_lb=subset(pop_reg_left_behind, leftbehind==1)

pop_in_reg_lb=pop_in_reg_lb[,c("CNTRYNAMEE","pop_left_behind_reg")]
write.csv(pop_in_reg_lb,"project_data/population_in_regions_left_behind.csv",row.names=F,na="")
