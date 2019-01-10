list.of.packages <- c("data.table","readr","WDI","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

prefix="E:"
wd = paste0(prefix,"/git/p20_spatial_2018")
setwd(wd)

merge.WDI = function(df,indicator,varname,start=1960,end=2018){
  wdi_tmp = WDI(indicator,country="all",extra=T,start=start,end=end)
  keep = c("country","year",indicator)
  wdi_tmp = wdi_tmp[keep]
  names(wdi_tmp) = c("country","Year",varname)
  df = join(df,wdi_tmp,by=c("country","Year"))
  return(df)
}

regionalhc=read.csv("project_data/regionswide.csv")

countryweights=data.table(regionalhc)[,.(
      national.weights.1=sum(weights.1, na.rm=T)
      ,national.weights.0=sum(weights.0,na.rm=T)
                                        ),by=.(CNTRYNAMEE)]

regionalhc=join(regionalhc,countryweights,by="CNTRYNAMEE")
regionalhc$weights.0=regionalhc$weights.0/regionalhc$national.weights.0
regionalhc$weights.1=regionalhc$weights.1/regionalhc$national.weights.1
regionalhc$CNTRYNAMEE=unfactor(regionalhc$CNTRYNAMEE)
regionalhc$CNTRYNAMEE[which(regionalhc$CNTRYNAMEE=="Congo Democratic Republic")]="Congo, Dem. Rep."
regionalhc$CNTRYNAMEE[which(regionalhc$CNTRYNAMEE=="Egypt")]="Egypt, Arab Rep."
setnames(regionalhc,"CNTRYNAMEE","country")





setnames(regionalhc,"RequestYear.0","Year")
regionalhc = merge.WDI(regionalhc,"SP.POP.TOTL","totalpop")

setnames(regionalhc,"Year","baseline.year")
setnames(regionalhc,"totalpop","pop.0")
setnames(regionalhc,"RequestYear.1","Year")

regionalhc = merge.WDI(regionalhc,"SP.POP.TOTL","totalpop")
setnames(regionalhc,"Year","final.year")
setnames(regionalhc,"totalpop","pop.1")
setnames(regionalhc,"OBJECTID","Region.Name")


regionalhc$poorpop.baseline=round(regionalhc$ExtremeHC.0*(regionalhc$weights.0*regionalhc$pop.0),0)
regionalhc$poorpop.recent=round(regionalhc$ExtremeHC.1*(regionalhc$weights.1*regionalhc$pop.1),0)


regionalhc=regionalhc[,c("DHSREGEN","country","poorpop.recent")]
regionalhc$poorpop.recent=order(as.numeric(regionalhc$poorpop.recent))

write.csv(regionalhc,"/project_data/regional_poor_pop.csv",row.names=F,na="")


regionalhc2=read.csv("project_data/recent_region.csv")

countryweights=data.table(regionalhc2)[,.(
  national.weights=sum(weights, na.rm=T)
),by=.(CNTRYNAMEE)]

regionalhc2=join(regionalhc2,countryweights,by="CNTRYNAMEE")
regionalhc2$weights=regionalhc2$weights/regionalhc2$national.weights
regionalhc2$CNTRYNAMEE=unfactor(regionalhc2$CNTRYNAMEE)
regionalhc2$CNTRYNAMEE[which(regionalhc2$CNTRYNAMEE=="Congo Democratic Republic")]="Congo, Dem. Rep."
regionalhc2$CNTRYNAMEE[which(regionalhc2$CNTRYNAMEE=="Egypt")]="Egypt, Arab Rep."
setnames(regionalhc2,"CNTRYNAMEE","country")


setnames(regionalhc2,"RequestYear","Year")
regionalhc2 = merge.WDI(regionalhc2,"SP.POP.TOTL","totalpop")

setnames(regionalhc2,"Year","year")


regionalhc2$regionpop=round((regionalhc2$weights*regionalhc2$totalpop),0)
regionalhc2$poorpop=round(regionalhc2$ExtremeHC*(regionalhc2$weights*regionalhc2$totalpop),0)


regionalhc2$region=paste0(regionalhc2$DHSREGEN,", ",regionalhc2$country)
regionalhc2=regionalhc2[,c("region","poorpop","totalpop","regionpop","OBJECTID")]

write.csv(regionalhc2,"E:/git/p20_spatial_2018/project_data/recent_regional_poor_pop.csv",row.names=F,na="")


