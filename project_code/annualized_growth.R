list.of.packages <- c("data.table","readr","plyr","reshape","varhandle")
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

filename.remapping=list(
  "ZWHR72DT"="ZWHR71DT"
 ,"AOHR52DT"="AOHR51DT"
 ,"BFHR32DT"="BFHR31DT"
 ,"BFHR71DT"="BFHR70DT"
 ,"BJHR42DT"="BJHR41DT"
 ,"BDHR42DT"="BDHR41DT"
 ,"BDHR71DT"="BDHR72DT"
 ,"BUHR71DT"="BUHR70DT"
 ,"CDHR52DT"="CDHR50DT"
 ,"CMHR42DT"="CMHR44DT"
 # ,"EGHR42DT"="EGHR41DT"
 ,"ETHR42DT"="ETHR41DT"
 ,"ETHR71DT"="ETHR70DT"
 ,"GNHR42DT"="GNHR41DT"
 ,"GNHR61DT"="GNHR62DT"
 ,"JOHR43DT"="JOHR42DT"
 ,"JOHR6ADT"="JOHR6CDT"
 ,"KEHR43DT"="KEHR42DT"
 ,"KEHR7ADT"="KEHR7HDT"
 ,"KHHR43DT"="KHHR42DT"
 ,"KHHR71DT"="KHHR73DT"
 ,"LBHR52DT"="LBHR51DT"
 ,"LBHR71DT"="LBHR70DT"
 ,"LSHR42DT"="LSHR41DT"
 ,"MDHR53DT"="MDHR51DT"
 ,"MLHR42DT"="MLHR41DT"
 ,"MLHR71DT"="MLHR70DT"
 ,"MWHR43DT"="MWHR41DT"
 ,"MWHR7IDT"="MWHR7QDT"
 ,"MZHR52DT"="MZHR51DT"
 ,"NMHR42DT"="NMHR41DT"
 ,"NPHR42DT"="NPHR41DT"
 ,"NPHR7ADT"="NPHR7HDT"
 ,"RWHR54DT"="RWHR53DT"
 ,"RWHR72DT"="RWHR70DT"
 ,"SNHR4BDT"="SNHR4HDT"
 ,"SNHR7IDT"="SNHR7HDT"
 ,"SLHR53DT"="SLHR51DT"
 ,"TLHR62DT"="TLHR61DT"
 ,"TZHR43DT"="TZHR41DT"
 ,"TZHR7ADT"="TZHR7HDT"
 ,"UGHR43DT"="UGHR41DT"
 ,"UGHR7ADT"="UGHR7HDT"
 ,"ZMHR52DT"="ZMHR51DT"
)
filename.remapping.names=names(filename.remapping)
uniqueregions$filename.max=unfactor(uniqueregions$filename.max)
uniqueregions$filename.min=unfactor(uniqueregions$filename.min)
for(from.remap in filename.remapping.names){
  to.remap=filename.remapping[[from.remap]]
  uniqueregions$filename.max[which(uniqueregions$filename.max==from.remap)]=to.remap
  uniqueregions$filename.min[which(uniqueregions$filename.min==from.remap)]=to.remap
}




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
keep=c("filename","ExtPovHC","P20Headcount","RequestYear")
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
dhspoints$filename=unfactor(dhspoints$filename)
for(from.remap in filename.remapping.names){
  to.remap=filename.remapping[[from.remap]]
  dhspoints$filename[which(dhspoints$filename==from.remap)]=to.remap
}
setnames(dhspoints,"DHSCLUST","cluster")
data.list = list()
data.index = 1

for(subrdata in subrdatas){
  povcal_filename=strsplit(subrdata, "/")[[1]][4]
  RequestYear = subset(povcalcuts,filename==povcal_filename)$RequestYear
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
  ),by=.(OBJECTID)]
  
  DHSCC=substring(povcal_filename,1,2)
  regional$DHSCC=DHSCC
  regional$RequestYear = RequestYear
  data.list[[data.index]] = regional
  data.index = data.index + 1

}
regionalhc<-rbindlist(data.list)

regionalhc = regionalhc[order(regionalhc$OBJECTID,regionalhc$RequestYear),]
regionalhc$time = duplicated(regionalhc$OBJECTID)*1

regionalhcwide=reshape(regionalhc, idvar=c("OBJECTID","DHSCC"), timevar="time",direction="wide")
#In Egypt DHS 2014 the Sinai Pennisula (called Frontier Governates) was excluded. We have dropped this portion from the analysis.
regionalhcwide=regionalhcwide[complete.cases(regionalhcwide),]

wd = paste0(prefix,"/git/p20_spatial_2018")
setwd(wd)

regionalhcwide$P20growthrate=((regionalhcwide$P20HC.1-regionalhcwide$P20HC.0)/regionalhcwide$P20HC.0)/(regionalhcwide$RequestYear.1-regionalhcwide$RequestYear.0)
regionalhcwide$ext.growthrate=((regionalhcwide$ExtremeHC.1-regionalhcwide$ExtremeHC.0)/regionalhcwide$ExtremeHC.0)/(regionalhcwide$RequestYear.1-regionalhcwide$RequestYear.0)
regionalhcwide$NP20growthrate=((regionalhcwide$NP20HC.1-regionalhcwide$NP20HC.0)/regionalhcwide$NP20HC.0)/(regionalhcwide$RequestYear.1-regionalhcwide$RequestYear.0)

regionalhcwide$P20growthrate[which(!is.finite(regionalhcwide$P20growthrate))]=NA
regionalhcwide$ext.growthrate[which(!is.finite(regionalhcwide$ext.growthrate))]=NA
regionalhcwide$NP20growthrate[which(!is.finite(regionalhcwide$NP20growthrate))]=NA

write.csv(regionalhcwide,"project_data/regionswide20180720.csv",row.names=F,na="")