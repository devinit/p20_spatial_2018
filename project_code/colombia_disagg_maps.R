list.of.packages <- c("data.table","readr","plyr","reshape","varhandle","mapproj")
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

regions=read.csv("project_data/dhspoints.csv")

regions=data.table(regions)
regions=regions[which(regions$DHSYEAR>=2005)]
regions$RequestYear=2015



regions=data.frame(regions[,.SD[which.max(DHSYEAR)],by=.(DHSCC)])
regions=unique(regions[c("DHSCC","RequestYear","filename")])
regions=regions[which(regions$DHSCC=="CO"),]


regions$CountryName=NA
regions$CountryName[which(regions$DHSCC=="CO")]="Colombia"


povcal=read.csv("E:/git/poverty_trends/data/P20incometrends.csv")
povcal=povcal[,c("CountryName","RequestYear","ExtPovHC","P20Headcount")]
povcal$P20Headcount=100*povcal$P20Headcount

povcalcuts=join(regions,povcal,by=c("CountryName","RequestYear"))


povcalcuts=povcalcuts[,c("filename","ExtPovHC","P20Headcount","RequestYear")]
povcalcuts$DHSCC=substring(povcalcuts$filename,1,2)



setwd("E:/DHSauto")
dir <-"E:/DHSauto/"

rdatas <- list.files(path=dir,pattern="*.RData",ignore.case=T,recursive=T,full.names=TRUE)
rdatas.split=strsplit(rdatas,"/")
rdatafolders=sapply(rdatas.split,`[`,index=4)
subrdatas=rdatas[which(rdatafolders %in% povcalcuts$filename)]

weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort 
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}


dhspoints=read.csv("E:/git/p20_spatial_2018/project_data/dhspoints.csv")
dhspoints$filename=unfactor(dhspoints$filename)


setnames(dhspoints,"DHSCLUST","cluster")
dhspoints=dhspoints[which(dhspoints$DHSCC=="CO"),]
data.list = list()
data.index = 1

for(subrdata in subrdatas){
  povcal_filename=strsplit(subrdata, "/")[[1]][4]
  RequestYear = subset(povcalcuts,filename==povcal_filename)$RequestYear
  Timeperiod = subset(povcalcuts,filename==povcal_filename)$time
  message(povcal_filename)
  load(subrdata)
  hr=data
  #Rename sample.weights var
  names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
  hr$weights <- hr$sample.weights/1000000
  
  if(is.null(hr$hv271)){
    DHSCC=substring(povcal_filename,1,2)
    recode=substring(povcal_filename,5,6)
    wealth_filename=paste0(DHSCC,"wi",recode)
    if(DHSCC %in% c("JO","KH")){
      wealth_filename=paste0(DHSCC,"wi41")
    }
    if(DHSCC %in% c("ML","MW")){
      wealth_filename=paste0(DHSCC,"wi42")
    }
    wealth_filepath=paste0("E:/DHSauto//",toupper(wealth_filename),"DT/",tolower(wealth_filename),"fl.RData")
    load(wealth_filepath)
    setnames(data, "whhid","hhid")
    if("wlthindf" %in% names(data)){
      setnames(data,"wlthindf","hv271")
    }
    if(DHSCC=="EG" & recode=="42"){
      data$hhid=trimws(data$hhid)
    }
    hr=join(hr,data,by=c("hhid"))
  }
  
  names(hr)[which(names(hr)=="hv271")] <- "wealth"
  hr$wealth <- hr$wealth/100000
  
  #Rename urban var
  names(hr)[which(names(hr)=="hv025")] <- "urban.rural"
  hr$urban <- NA
  hr$urban[which(hr$urban.rural==1)] <- 1
  hr$urban[which(hr$urban.rural==2)] <- 0
  #Rename cluster/hh var
  names(hr)[which(names(hr)=="hv001")] <- "cluster"
  names(hr)[which(names(hr)=="hv002")] <- "household"
  if(povcal_filename!="NPHR7HDT"){
    names(hr)[which(names(hr)=="hv024")] <- "region"
  }else{
    names(hr)[which(names(hr)=="shdevreg")] <- "region" 
  }
  names(hr)[which(names(hr)=="hvidx")] <- "line"
  #povcalcuts
  povcalcut <- subset(povcalcuts,filename==povcal_filename)$P20Headcount/100
  extcut <- subset(povcalcuts,filename==povcal_filename)$ExtPovHC/100
  cuts <- c(povcalcut,extcut,.2)
  povperc <- weighted.percentile(hr$wealth,hr$weights,prob=cuts)
  hr$p20 <- (hr$wealth < povperc[1])
  hr$ext <- (hr$wealth < povperc[2])
  hr$np20<- (hr$wealth < povperc[3])
  datapoints=dhspoints[which(dhspoints$filename==povcal_filename),]
  hr=join(hr,datapoints,by=c("cluster"))
  
  
  regional=data.table(hr)[,.(
    P20HC=weighted.mean(p20, weights, na.rm=TRUE)
    ,ExtremeHC=weighted.mean(ext, weights, na.rm=TRUE)
    ,NP20HC=weighted.mean(np20, weights, na.rm=TRUE)
    ,weights=sum(weights,na.rm=T)
  ),by=.(OBJECTID)]
  
  cluster=data.table(hr)[,.(
    P20HC=weighted.mean(p20, weights, na.rm=TRUE)
    ,ExtremeHC=weighted.mean(ext, weights, na.rm=TRUE)
    ,NP20HC=weighted.mean(np20, weights, na.rm=TRUE)
    ,weights=sum(weights,na.rm=T)
  ),by=.(cluster)]
  
  DHSCC=substring(povcal_filename,1,2)
  regional$DHSCC=DHSCC
  regional$RequestYear = RequestYear
  regional$time= Timeperiod
  data.list[[data.index]] = regional
  data.index = data.index + 1
  
}
regionalhc<-rbindlist(data.list)



wd = paste0(prefix,"/git/p20_spatial_2018")
setwd(wd)



load("project_data/recent_dhs_1.RData")
regionnames=recent_dhs_1@data[,c("OBJECTID","DHSREGEN","CNTRYNAMEE"),with=F]
regionalhc=join(regionalhc,regionnames,by="OBJECTID")

write.csv(regionalhc,"project_data/recent_region_colombia.csv",row.names=F,na="")
write.csv(cluster,"project_data/recent_cluster_colombia.csv",row.names=F,na="")





load("project_data/recent_dhs_1.RData")
regionalhc1=regionalhc
regionalhc=regionalhc[,c("OBJECTID"  ,  "P20HC"    ,   "ExtremeHC"  , "NP20HC"    ,  "weights" ,    "DHSCC"      
                         ,"RequestYear" )]

dhs_growth=merge(recent_dhs_1,regionalhc,by=c("OBJECTID","DHSCC"))
dhs_growth=subset(dhs_growth,DHSCC=="CO")
# dhs_growth=subset(dhs_growth,!is.na(RequestYear.0) | !is.na(RequestYear.1))

layers=list(
  "Latest Extreme Poverty Headcount"="ExtremeHC"
  ,"Latest P20 Headcount"="P20HC"
  ,"Latest National P20 Headcount"="NP20HC"
)
layernames=names(layers)

url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"

tmp <- tempdir()

file <- basename(url)

if(!file.exists(file)){
  download.file(url, file)
}
unzip(file, exdir = tmp)

countries <- readOGR(dsn = tmp, 
                     layer = "ne_50m_admin_0_countries", 
                     encoding = "UTF-8",
                     verbose = FALSE)

countries.f=fortify(countries,region="NAME")
countries.f$source="national"

dhs_growth.f=fortify(dhs_growth,region="OBJECTID")


dhs_growth_data=dhs_growth@data
setnames(dhs_growth_data,"OBJECTID","id")
dhs_growth.f=merge(dhs_growth.f, dhs_growth_data, by=c("id"))
dhs_growth.f$source="subnational"

allshapes=rbindlist(list(countries.f,dhs_growth.f),fill=T)


reds = c("#FBD7CB","#F6B2A7","#F28E83","#ED695E","#E8443A")
quants=quantile(allshapes$P20HC,na.rm=T)
allshapes$color="#d9d4da"
allshapes$color[which(allshapes$source=="subnational" & allshapes$P20HC>=quants[1])]=reds[1]
allshapes$color[which(allshapes$source=="subnational" & allshapes$P20HC>=quants[2])]=reds[2]
allshapes$color[which(allshapes$source=="subnational" & allshapes$P20HC>=quants[3])]=reds[3]
allshapes$color[which(allshapes$source=="subnational" & allshapes$P20HC>=quants[4])]=reds[4]

allshapes$trim="transparent"
allshapes$size=0.01

clusterpoints=readOGR("E:/DHS GPS/COGE61FL/COGE61FL.shp")
clusterpoints=clusterpoints@data
setnames(clusterpoints,"DHSCLUST","cluster")
clusterpoints=merge(clusterpoints,cluster,by=c("cluster"))
quants2=quantile(clusterpoints$P20HC,na.rm=T)
clusterpoints$color="#d9d4da"
clusterpoints$color[which(clusterpoints$P20HC>=quants2[1])]=reds[1]
clusterpoints$color[which(clusterpoints$P20HC>=quants2[2])]=reds[2]
clusterpoints$color[which(clusterpoints$P20HC>=quants2[3])]=reds[3]
clusterpoints$color[which(clusterpoints$P20HC>=quants2[4])]=reds[4]


P20mapregion=ggplot(allshapes)+
  geom_polygon(aes(long,lat,group=group,color=trim,fill=color,size=size))+
  geom_polygon(data=subset(allshapes,source=="national"), aes(long,lat,group=group,color=trim,fill="transparent",size=size))+
  scale_color_identity()+
  scale_fill_identity()+
  scale_size_identity()+
  coord_cartesian(ylim=c(-8,16),
              xlim=c(-84,-60))+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),plot.margin=unit(c(0,0,0,0),"cm"),axis.ticks = element_blank())+
  labs(x="",y="")

P20mapregioncluster=ggplot(allshapes)+
  geom_polygon(data=subset(allshapes,source=="national"), aes(long,lat,group=group,color=trim,fill=color,size=.001))+
  geom_polygon(data=subset(allshapes,source=="national"), aes(long,lat,group=group,color=trim,fill="transparent",size=.001))+
  geom_point(data=clusterpoints,aes(x=LONGNUM,y=LATNUM,color=color))+
  scale_color_identity()+
  scale_fill_identity()+
  scale_size_identity()+
  coord_cartesian(ylim=c(-8,16),
                  xlim=c(-84,-60))+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),plot.margin=unit(c(0,0,0,0),"cm"),axis.ticks = element_blank())+
  labs(x="",y="")
