list.of.packages <- c("sp","rgdal","leaflet","data.table","mapview")
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

growthrates=read.csv("project_data/regionswide20180720.csv",as.is=T)


load("project_data/recent_dhs_1.RData")

dhs_growth=merge(recent_dhs_1,growthrates,by=c("OBJECTID","DHSCC"))
dhs_growth=subset(dhs_growth,!is.na(RequestYear.0) | !is.na(RequestYear.1))

layers=list(
  "Extreme Poverty Growth Rate"="ext.growthrate"
  ,"P20 Growth Rate"="P20growthrate"
  ,"National P20 Growth Rate"="NP20growthrate"
  ,"Baseline P20 Headcount"="P20HC.0"
  ,"Baseline National P20 Headcount"="NP20HC.0"
  ,"Baseline Extreme Poverty Headcount"="ExtremeHC.0"
  ,"Latest Extreme Poverty Headcount"="ExtremeHC.1"
  ,"Latest P20 Headcount"="P20HC.1"
  ,"Latest National P20 Headcount"="NP20HC.1"
)
layernames=names(layers)


for(layername in layernames){
  varname=layers[[layername]]
  dhs_growth@data[,(varname)]=dhs_growth@data[,(varname),with=F][[1]]*100
  
  pal <- colorBin(
    palette = "YlOrRd",
    domain = dhs_growth@data[,(varname),with=F]
    ,bins=quantile(dhs_growth@data[,(varname),with=F],probs=seq(0,1,0.2),na.rm=T)
  )


m=leaflet(data=dhs_growth) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(0, 0, zoom=2) 
  if(grepl(".0",varname,fixed=T)){
    m=m %>% addPolygons(color = pal(dhs_growth@data[,(varname),with=F][[1]])
                        ,fillOpacity = 1
                        ,stroke=F
                        ,smoothFactor=0.2
                        ,popup=paste0(
                          "<b>",dhs_growth@data$CNTRYNAMEE,"</b> <br/>",
                          "<b>Region Name: </b>",dhs_growth@data$DHSREGEN,"<br/>",
                          "<b>Value: </b>",round(dhs_growth@data[,(varname),with=F][[1]],2),"<br/>",
                          "<b>Year: </b>",dhs_growth@data$RequestYear.0
                        )
    ) 
  }  else if(grepl(".1",varname,fixed=T)){
    m=m %>% addPolygons(color = pal(dhs_growth@data[,(varname),with=F][[1]])
                        ,fillOpacity = 1
                        ,stroke=F
                        ,smoothFactor=0.2
                        ,popup=paste0(
                          "<b>",dhs_growth@data$CNTRYNAMEE,"</b> <br/>",
                          "<b>Region Name: </b>",dhs_growth@data$DHSREGEN,"<br/>",
                          "<b>Value: </b>",round(dhs_growth@data[,(varname),with=F][[1]],2),"<br/>",
                          "<b>Year: </b>",dhs_growth@data$RequestYear.1
                        )
    ) 
  } else {
    m=m %>% addPolygons(color = pal(dhs_growth@data[,(varname),with=F][[1]])
                        ,fillOpacity = 1
                        ,stroke=F
                        ,smoothFactor=0.2
                        ,popup=paste0(
                          "<b>",dhs_growth@data$CNTRYNAMEE,"</b> <br/>",
                          "<b>Region Name: </b>",dhs_growth@data$DHSREGEN,"<br/>",
                          "<b>Value: </b>",round(dhs_growth@data[,(varname),with=F][[1]],2),"<br/>",
                          "<b>Year Range: </b>",dhs_growth@data$RequestYear.0," - ",dhs_growth@data$RequestYear.1
                        )
    ) 
  }

  
m=m %>%  addLegend("bottomright", pal=pal, values = dhs_growth@data[,(varname),with=F][[1]], opacity = 1, title=layername)
      
   filename=paste0(prefix,"/git/p20_spatial_2018/graphics/",layername,".html") 
try({mapshot(m,file=filename)})
}