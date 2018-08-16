list.of.packages <- c("sp","rgdal","leaflet","data.table","mapview","ggplot2","RColorBrewer")
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

countries.f=fortify(countries)
basemap= ggplot(countries.f)+
  geom_polygon(aes(long,lat,group=group,color="dimgrey",fill="grey",size=0.02))+
  scale_color_identity()+
  scale_fill_identity()+
  scale_size_identity()+
  expand_limits(x=countries.f$long,y=countries.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  labs(x="",y="")
ggsave("eps/basemap.png",basemap,device="png",width=10,height=6)
ggsave("eps/basemap.eps",basemap,device="eps",width=10,height=6)

for(layername in layernames){
  varname=layers[[layername]]
  dhs_growth@data[,(varname)]=dhs_growth@data[,(varname),with=F][[1]]*100
  palbins=quantile(dhs_growth@data[,(varname),with=F],probs=seq(0,1,0.2),na.rm=T)
  palbins[1]=floor(palbins[1])
  palbins[length(palbins)]=ceiling(palbins[length(palbins)])
  palbins=round(palbins)
  pal <- colorBin(
    palette = "YlOrRd",
    domain = dhs_growth@data[,(varname),with=F]
    ,bins=palbins
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

  
m=m %>%  addLegend("bottomright", pal=pal, values = dhs_growth@data[,(varname),with=F][[1]], opacity = 1, title=layername,labFormat = labelFormat(suffix="%"))
      
   filename=paste0(prefix,"/git/p20_spatial_2018/graphics/",layername,".html") 
try({mapshot(m,file=filename)})
dhs_growth.f=fortify(dhs_growth,region="OBJECTID")
setnames(dhs_growth.f,"id","OBJECTID")
dhs_growth.f=merge(dhs_growth.f, dhs_growth, by=c("OBJECTID"))
dhs_growth.f$color=pal(dhs_growth.f[,(varname)])
overlay=ggplot(dhs_growth.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=color,color="grey",size=0.02))+
  scale_color_identity()+
  scale_fill_identity()+
  scale_size_identity()+
  expand_limits(x=countries.f$long,y=countries.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  labs(x="",y="")
  ggsave(paste0("eps/",varname,".png"),overlay,device="png",width=10,height=6)
  ggsave(paste0("eps/",varname,".eps"),overlay,device="eps",width=10,height=6)
   }




