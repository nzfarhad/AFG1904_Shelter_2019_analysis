
require(OpenStreetMap)
require(rgdal)
require(dplyr)

create_spatial_dataframe<-function(data,lat,long){
  data<-data[!is.na(data[[long]])& !is.na(data[[lat]]),]
  xy <- data[,c(long,lat)] %>% class_assess
  datasp <- SpatialPointsDataFrame(coords = xy, data = data,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(datasp)
}


get_osm<-function(datasp,perczoom=0.1){
  bb<-bbox(datasp)
  # add the rounding
  if((bb[1,2]-bb[1,1])>0.01){lat_ext<-(bb[1,2]-bb[1,1])*perczoom}else{lat_ext<-0.01}
  if((bb[2,2]-bb[2,1])>0.01){lng_ext<-(bb[2,2]-bb[2,1])*perczoom}else{lng_ext<-0.01}
  
  bb[2,2]<-bb[2,2]+lng_ext
  bb[1,1]<-bb[1,1]-lng_ext
  bb[2,1]<-bb[2,1]-lat_ext
  bb[1,2]<-bb[1,2]+lat_ext

  mp <- openmap(c(bb[2,2],bb[1,1]),c(bb[2,1],bb[1,2]),type='bing')

  mp<-openproj(mp,projection= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  return(mp)
}



create_map<-function(db,lat="_geopoint_latitude",long="_geopoint_longitude", disa=NULL,level=NULL){
  
  db$lat<-db[[lat]]
  db$long<-db[[long]]
  
  if(!is.null(disa) & !is.null(level)){
    db<-db[db[[disa]]==level,]
    label<-paste0(disa," - ",level)
  } else {
    label<-"a map"
  }
  
  osmmap<-db %>% create_spatial_dataframe(.,"lat","long") %>% get_osm
  map <- autoplot(osmmap)  +
    geom_point(data=db, aes(y=lat, x=long),shape=1,color="red") + 
    theme_minimal() +
    xlab('Longitude') + ylab('Latitude') + 
    ggtitle(label) +  
    theme(plot.title = element_text(hjust=0.5, size=rel(1)),
          legend.title = element_text(size=rel(1))) 
  
  ggsave(paste0("map/map_",label,".png"),map,width = 20, height = 20, units = "cm",dpi=500)
  
  return(map)
}

