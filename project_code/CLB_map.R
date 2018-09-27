list.of.packages <- c("sp","rgdal","leaflet","data.table","mapview","ggplot2","RColorBrewer","varhandle")
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


#CLB map
clb=c(
  "Afghanistan"
  ,"Benin"                    
  ,"Burundi"
  ,"Central African Republic" 
  ,"Chad"                     
  ,"Republic of Congo"                     
  ,"Democratic Republic of the Congo" 
  ,"Eritrea"
  ,"Gambia"  
  ,"Guinea"
  ,"Guinea-Bissau"            
  ,"Haiti"
  ,"Lesotho"                  
  ,"Liberia" 
  ,"Madagascar" 
  ,"Malawi" 
  ,"Mali"
  ,"Micronesia"
  , "Mozambique"               
  ,"Niger"
  ,"Nigeria"
  ,"Papua New Guinea"   
  ,"Somalia"
  ,"South Sudan"
  ,"Sudan" 
  ,"Syria"  
  ,"Togo" 
  ,"Uganda"
  ,"Yemen"
  ,"Zambia" 
)

world=readOGR(dsn="E:/git/di-geospatial/Datahub/world",
              layer="world")
world.f=fortify(world,region="COUNTRY")
world.f$CLB=NA
world.f$CLB[which(world.f$id %in% clb)]=1


world.f$color="#d9d4da"
world.f$color[which(world.f$CLB==1)]="#e84439"
world.f$color[which(world.f$CLB==0)]="white"
world.f$trim="transparent"
world.f$size=1
world.f$size=.01

les=world.f[which(world.f$id=="Lesotho"),]

clbmap=ggplot(world.f)+
  geom_polygon(aes(long,lat,group=group,color=trim,fill=color,size=size))+
  geom_polygon(data=les,aes(long,lat,group=group,color=trim,fill=color,size=size))+
  geom_polygon(data=world.f, aes(long,lat,group=group,color=trim,fill="transparent",size=size))+
  scale_color_identity()+
  scale_fill_identity()+
  scale_size_identity()+
  expand_limits(x=world.f$long,y=world.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),plot.margin=unit(c(0,0,0,0),"cm"),axis.ticks = element_blank())+
  labs(x="",y="")

ggsave("eps/clbmap.png",clbmap,device="png",width=5,height=3)
ggsave("eps/clbmap.eps",clbmap,device="eps",width=5,height=3)
ggsave("eps/clbmap.ps",clbmap,device="ps",width=5,height=3)
ggsave("eps/clbmap.svg",clbmap,device="svg",width=5,height=3)

#Micronesia Zoom
micronesiamap=ggplot(world.f)+
  geom_polygon(aes(long,lat,group=group,color=trim,fill=color,size=size))+
  geom_polygon(data=world.f, aes(long,lat,group=group,color=trim,fill="transparent",size=size))+
  scale_color_identity()+
  scale_fill_identity()+
  scale_size_identity()+
  ylim(6,8)+
  xlim(157,159)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),plot.margin=unit(c(0,0,0,0),"cm"),axis.ticks = element_blank())+
  labs(x="",y="")
 
ggsave("E:/git/p20_spatial_2018/eps/subnationallb.svg",lbmap,device="svg",width=20,height=12)
ggsave("E:/git/p20_spatial_2018/eps/subnationallb.ps",lbmap,device="ps",width=20,height=12) 
ggsave("E:/git/p20_spatial_2018/eps/subnationallb.png",lbmap,device="png",width=20,height=12)
ggsave("E:/git/p20_spatial_2018/eps/subnationallb.eps",lbmap,device="eps",width=20,height=12)

ggsave("E:/git/p20_spatial_2018/eps/micronesiamap.png",micronesiamap,device="png",width=20,height=12)
ggsave("E:/git/p20_spatial_2018/eps/micronesiamap.eps",micronesiamap,device="eps",width=20,height=12)
ggsave("E:/git/p20_spatial_2018/eps/micronesiamap.svg",micronesiamap,device="svg",width=20,height=12)
ggsave("E:/git/p20_spatial_2018/eps/micronesiamap.ps",micronesiamap,device="ps",width=20,height=12)
