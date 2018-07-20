list.of.packages <- c("data.table","readr","plyr")
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
regions=regions[which(regions$DHSYEAR>=1999)]
regions$RequestYear=NA
regions$RequestYear[which(regions$DHSYEAR<2018)]=2013
regions$RequestYear[which(regions$DHSYEAR<2013)]=2012
regions$RequestYear[which(regions$DHSYEAR<2012)]=2011
regions$RequestYear[which(regions$DHSYEAR<2011)]=2010
regions$RequestYear[which(regions$DHSYEAR<2010)]=2008
regions$RequestYear[which(regions$DHSYEAR<2008)]=2005
regions$RequestYear[which(regions$DHSYEAR<2005)]=2002
regions$RequestYear[which(regions$DHSYEAR<2002)]=1999

regions.min=data.frame(regions[,.SD[which.min(DHSYEAR)],by=.(DHSCC)])
regions.max=data.frame(regions[,.SD[which.max(DHSYEAR)],by=.(DHSCC)])
regions.min=unique(regions.min[c("DHSCC","RequestYear","filename")])
regions.max=unique(regions.max[c("DHSCC","RequestYear","filename")])
setnames(regions.min,"RequestYear","RequestYear.min")
setnames(regions.max,"RequestYear","RequestYear.max")
setnames(regions.min,"filename","filename.min")
setnames(regions.max,"filename","filename.max")
uniqueregions=merge(regions.min,regions.max)
uniqueregions=subset(uniqueregions, !(RequestYear.min==RequestYear.max))
uniqueregions=uniqueregions[which(uniqueregions$RequestYear.max>=2010),]

uniqueregions$CountryName=NA
uniqueregions$CountryName[which(uniqueregions$DHSCC=="AL")]="Albania"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="AM")]="Armenia"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="AO")]="Angola"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="BD")]="Bangladesh"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="BF")]="Burkina Faso"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="BJ")]="Benin"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="BU")]="Burundi"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="CD")]="Congo, Democratic Republic of"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="CM")]="Cameroon"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="DR")]="Dominican Republic"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="EG")]="Egypt, Arab Republic of"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="ET")]="Ethiopia"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="GH")]="Ghana"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="GN")]="Guinea"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="HT")]="Haiti"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="JO")]="Jordan"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="KE")]="Kenya"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="KH")]="Cambodia"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="LB")]="Liberia"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="LS")]="Lesotho"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="MD")]="Madagascar"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="ML")]="Mali"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="MW")]="Malawi"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="MZ")]="Mozambique"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="NG")]="Nigeria"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="NM")]="Namibia"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="NP")]="Nepal"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="RW")]="Rwanda"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="SL")]="Sierra Leone"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="SN")]="Senegal"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="TL")]="Timor-Leste"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="TZ")]="Tanzania"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="UG")]="Uganda"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="ZM")]="Zambia"
uniqueregions$CountryName[which(uniqueregions$DHSCC=="ZW")]="Zimbabwe"


povcal=read.csv("C:/Users/Zach/Documents/Poverty data/P20incometrends20180709.csv")
povcal=povcal[,c("CountryName","RequestYear","ExtPovHC","P20Headcount")]
setnames(povcal,"RequestYear","RequestYear.min")
baseline=join(uniqueregions,povcal,by=c("CountryName","RequestYear.min"))
setnames(povcal,"RequestYear.min","RequestYear.max")
latestyear=join(uniqueregions,povcal,by=c("CountryName","RequestYear.max"))
setnames(baseline,"RequestYear.min","RequestYear")
setnames(baseline,"filename.min","filename")
keep=c("filename","ExtPovHC","P20Headcount")
baseline=baseline[,keep]
setnames(latestyear,"RequestYear.max","RequestYear")
setnames(latestyear,"filename.max","filename")
latestyear=latestyear[,keep]
povcalcuts=rbind(baseline,latestyear)

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
setnames(dhspoints,"DHSCLUST","cluster")
data.list = list()

for(subrdata in subrdatas){
  povcal_filename=strsplit(subrdata, "/")[[1]][4]
  message(povcal_filename)
  load(subrdata)
  hr=data
  #Rename sample.weights var
  names(hr)[which(names(hr)=="hv005")] <- "sample.weights"
  hr$weights <- hr$sample.weights/1000000
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
  ),by=.(OBJECTID)]

}
regionalhc<-rbindlist(data.list)



