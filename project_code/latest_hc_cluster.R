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
regions=regions[which(regions$DHSYEAR>=2005)]
regions$RequestYear=2015



regions=data.frame(regions[,.SD[which.max(DHSYEAR)],by=.(DHSCC)])
regions=unique(regions[c("DHSCC","RequestYear","filename")])


filename.remapping=list(
  "ZWHR72DT"="ZWHR71DT"
 ,"AOHR52DT"="AOHR51DT"
 ,"ALHR51DT"="ALHR50DT"
 ,"BFHR32DT"="BFHR31DT"
 ,"BFHR71DT"="BFHR70DT"
 ,"BJHR42DT"="BJHR41DT"
 ,"BOHR53DT"="BOHR51DT"
 ,"BDHR71DT"="BDHR72DT"
 ,"BDHR71DT"="BDHR72DT"
 ,"BUHR71DT"="BUHR70DT"
 ,"CDHR52DT"="CDHR50DT"
 ,"CMHR42DT"="CMHR44DT"
 ,"CIHR61DT"="CIHR62DT"
 # ,"EGHR42DT"="EGHR41DT"
 ,"ETHR42DT"="ETHR41DT"
 ,"ETHR71DT"="ETHR70DT"
 ,"GAHR61DT"="GAHR60DT"
 ,"GNHR42DT"="GNHR41DT"
 ,"GYHR5JDT"="GYHR5IDT"
 ,"GNHR61DT"="GNHR62DT"
 ,"HNHR61DT"="HNHR62DT"
 ,"JOHR43DT"="JOHR42DT"
 ,"JOHR6ADT"="JOHR6CDT"
 ,"KEHR43DT"="KEHR42DT"
 ,"KEHR7ADT"="KEHR7HDT"
 ,"KHHR43DT"="KHHR42DT"
 ,"KHHR71DT"="KHHR73DT"
 ,"LBHR52DT"="LBHR51DT"
 ,"LBHR71DT"="LBHR70DT"
 ,"LSHR42DT"="LSHR41DT"
 ,"MBHR52DT"="MBHR53DT"
 ,"MDHR53DT"="MDHR51DT"
 ,"MLHR42DT"="MLHR41DT"
 ,"MLHR71DT"="MLHR70DT"
 ,"MWHR43DT"="MWHR41DT"
 ,"MWHR7IDT"="MWHR7QDT"
 ,"MZHR52DT"="MZHR51DT"
 ,"NMHR42DT"="NMHR41DT"
 ,"NPHR42DT"="NPHR41DT"
 ,"NPHR7ADT"="NPHR7HDT"
 ,"PKHR51DT"="PKHR52DT"
 ,"RWHR54DT"="RWHR53DT"
 ,"RWHR72DT"="RWHR70DT"
 ,"SNHR4BDT"="SNHR4HDT"
 ,"SNHR7IDT"="SNHR7HDT"
 ,"SLHR53DT"="SLHR51DT"
 ,"SZHR53DT"="SZHR51DT"
 ,"TGHR62DT"="TGHR61DT"
 ,"TLHR62DT"="TLHR61DT"
 ,"TZHR43DT"="TZHR41DT"
 ,"TZHR7ADT"="TZHR7HDT"
 ,"UGHR43DT"="UGHR41DT"
 ,"UGHR7ADT"="UGHR7HDT"
 ,"ZMHR52DT"="ZMHR51DT"
)
filename.remapping.names=names(filename.remapping)
regions$filename=unfactor(regions$filename)
for(from.remap in filename.remapping.names){
  to.remap=filename.remapping[[from.remap]]
  regions$filename[which(regions$filename==from.remap)]=to.remap
}

regions$CountryName=NA
regions$CountryName[which(regions$DHSCC=="AL")]="Albania"
regions$CountryName[which(regions$DHSCC=="AM")]="Armenia"
regions$CountryName[which(regions$DHSCC=="AO")]="Angola"
regions$CountryName[which(regions$DHSCC=="BD")]="Bangladesh"
regions$CountryName[which(regions$DHSCC=="BF")]="Burkina Faso"
regions$CountryName[which(regions$DHSCC=="BJ")]="Benin"
regions$CountryName[which(regions$DHSCC=="BO")]="Bolivia"
regions$CountryName[which(regions$DHSCC=="BU")]="Burundi"
regions$CountryName[which(regions$DHSCC=="CD")]="Congo, Democratic Republic of"
regions$CountryName[which(regions$DHSCC=="CI")]="Cote d'Ivoire"
regions$CountryName[which(regions$DHSCC=="CM")]="Cameroon"
regions$CountryName[which(regions$DHSCC=="CO")]="Colombia"
regions$CountryName[which(regions$DHSCC=="DR")]="Dominican Republic"
regions$CountryName[which(regions$DHSCC=="EG")]="Egypt, Arab Republic of"
regions$CountryName[which(regions$DHSCC=="ET")]="Ethiopia"
regions$CountryName[which(regions$DHSCC=="GA")]="Gambia, The"
regions$CountryName[which(regions$DHSCC=="GH")]="Ghana"
regions$CountryName[which(regions$DHSCC=="GN")]="Guinea"
regions$CountryName[which(regions$DHSCC=="GY")]="Guyana"
regions$CountryName[which(regions$DHSCC=="HN")]="Honduras"
regions$CountryName[which(regions$DHSCC=="HT")]="Haiti"
regions$CountryName[which(regions$DHSCC=="JO")]="Jordan"
regions$CountryName[which(regions$DHSCC=="KE")]="Kenya"
regions$CountryName[which(regions$DHSCC=="KM")]="Comoros"
regions$CountryName[which(regions$DHSCC=="KH")]="Cambodia"
regions$CountryName[which(regions$DHSCC=="KY")]="Kyrgyz Republic"
regions$CountryName[which(regions$DHSCC=="LB")]="Liberia"
regions$CountryName[which(regions$DHSCC=="LS")]="Lesotho"
regions$CountryName[which(regions$DHSCC=="MD")]="Madagascar"
regions$CountryName[which(regions$DHSCC=="ML")]="Mali"
regions$CountryName[which(regions$DHSCC=="MB")]="Moldova"
regions$CountryName[which(regions$DHSCC=="MW")]="Malawi"
regions$CountryName[which(regions$DHSCC=="MZ")]="Mozambique"
regions$CountryName[which(regions$DHSCC=="MM")]="Myanmar"
regions$CountryName[which(regions$DHSCC=="NG")]="Nigeria"
regions$CountryName[which(regions$DHSCC=="NM")]="Namibia"
regions$CountryName[which(regions$DHSCC=="NP")]="Nepal"
regions$CountryName[which(regions$DHSCC=="PE")]="Peru"
regions$CountryName[which(regions$DHSCC=="PH")]="Philippines"
regions$CountryName[which(regions$DHSCC=="PK")]="Pakistan"
regions$CountryName[which(regions$DHSCC=="RW")]="Rwanda"
regions$CountryName[which(regions$DHSCC=="SL")]="Sierra Leone"
regions$CountryName[which(regions$DHSCC=="SN")]="Senegal"
regions$CountryName[which(regions$DHSCC=="SZ")]="Swaziland"
regions$CountryName[which(regions$DHSCC=="TG")]="Togo"
regions$CountryName[which(regions$DHSCC=="TJ")]="Tajikistan"
regions$CountryName[which(regions$DHSCC=="TL")]="Timor-Leste"
regions$CountryName[which(regions$DHSCC=="TZ")]="Tanzania"
regions$CountryName[which(regions$DHSCC=="UG")]="Uganda"
regions$CountryName[which(regions$DHSCC=="ZM")]="Zambia"
regions$CountryName[which(regions$DHSCC=="ZW")]="Zimbabwe"


povcal=read.csv("E:/git/poverty_trends/data/P20incometrends.csv")
povcal=povcal[,c("CountryName","RequestYear","ExtPovHC","P20Headcount")]

povcalcuts=join(regions,povcal,by=c("CountryName","RequestYear"))

keep=c("filename","ExtPovHC","P20Headcount","RequestYear")
povcalcuts=povcalcuts[,keep]
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


dhspoints=read.csv("E:/git/p20_spatial_2018/project_data/dhspoints_latlong.csv")
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
  povcalcut <- subset(povcalcuts,filename==povcal_filename)$P20Headcount
  extcut <- subset(povcalcuts,filename==povcal_filename)$ExtPovHC
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
    ,lat=mean(LATNUM)
    ,long=mean(LONGNUM)
  ),by=c("cluster","OBJECTID")]
  
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
regionalhc=join(regionalhc,regionnames,by=c("OBJECTID"))

write.csv(regionalhc,"project_data/recent_cluster.csv",row.names=F,na="")
