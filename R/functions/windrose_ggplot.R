require(ggplot2)
require(RColorBrewer)

#############################################################################################
#############################################################################################
recoding<-function(x,y,z){
  if(is.na(y)){
    x[is.na(x)]<-z
  }else {
    x[which(x==y)]<-z
  }
  return(x)
}

# function
extract_weather<-function(wdb,lev){
  lapply( 
    seq_along(wdb), function(x,wdb){
      wdb[[x]][[lev]] %>% as.data.frame
    },wdb=wdb) %>% bind_rows
}


getweather<-function(filename,write=F){
  ################################################################################################
  # get the data for BOa Vista
  try(detach(package:plyr),silent=T)
  
  wdb<-readRDS(sprintf("../weather/%s.rds",filename))
  
  #####################
  # extract hourly data
  hourlywdata <-extract_weather(wdb,"hourly") 
  hourlywdata$date<-as.Date(hourlywdata$time,"%Y-%m-%d")
  var_to0<- c("windBearing","windGust","windSpeed","precipProbability","precipIntensity")
  for(x in var_to0){
    hourlywdata[[x]]<-recoding(hourlywdata[[x]],NA,0)
  }
  
  #####################
  # extract daily data
  dailywdata<-extract_weather(wdb,"daily") 
  # recode the var missing for export to 0
  var_to0<- c("windBearing","windGust","windSpeed","precipProbability","precipIntensityMax")
  for(x in var_to0){
    dailywdata[[x]]<-recoding(dailywdata[[x]],NA,0)
  }

  ################################################################################################
  ################################################################################################
  ################################################################################################
  # source: https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
  # create a wind rose for the location
  
  
  winddata<-hourlywdata[which(hourlywdata$windSpeed!=0),]
  p0 <- plot.windrose(spd = winddata$windSpeed, 
                      dir = winddata$windBearing)
  p0
  
  precip<-hourlywdata %>% group_by(date) %>% summarize(precip=sum(precipIntensity))
  prec<-ggplot(precip)  + 
    geom_bar(aes(x=date, y=precip),stat="identity", fill="tan1", colour="sienna3") + theme_minimal() + ylab("Precipitation (mm)") + xlab("")
  prec
 
 
  temp<-ggplot(dailywdata)+
    geom_smooth(aes(x=time, y=temperatureHigh),stat="identity",color="red")+
    geom_smooth(aes(x=time, y=temperatureLow),stat="identity",color="blue")+ ylab("Temperature (C)") +  xlab("")
  temp
  
  if(write){
    ggsave(sprintf("../datamerge/figures/windrose_%s.png",filename),p0,width = 4, height = 3)
    ggsave(sprintf("../datamerge/figures/precipitation_%s.png",filename),prec,width = 4, height = 2)  
    ggsave(sprintf("../datamerge/figures/temperture_%s.png",filename),temp,width = 4, height = 2)
    write.csv(hourlywdata,sprintf("../weather/weather_hourly_%s.csv",filename))
    write.csv(dailywdata,sprintf("../weather/weather_daily_%s.csv",filename))
  }
}



getweather2<-function(filename,path,write=F){
  ################################################################################################
  # get the data for BOa Vista
  try(detach(package:plyr),silent=T)
  
  wdb<-readRDS(paste0(path,"/",filename,".rds"))
  
  #####################
  # extract hourly data
  hourlywdata <-extract_weather(wdb,"hourly") 
  hourlywdata$date<-as.Date(hourlywdata$time,"%Y-%m-%d")
  var_to0<- c("windBearing","windGust","windSpeed","precipProbability","precipIntensity")
  for(x in var_to0){
    hourlywdata[[x]]<-recoding(hourlywdata[[x]],NA,0)
  }
  
  #####################
  # extract daily data
  dailywdata<-extract_weather(wdb,"daily") 
  # recode the var missing for export to 0
  var_to0<- c("windBearing","windGust","windSpeed","precipProbability","precipIntensityMax")
  for(x in var_to0){
    dailywdata[[x]]<-recoding(dailywdata[[x]],NA,0)
  }
  
  ################################################################################################
  ################################################################################################
  ################################################################################################
  # source: https://stackoverflow.com/questions/17266780/wind-rose-with-ggplot-r
  # create a wind rose for the location
  
  
  winddata<-hourlywdata[which(hourlywdata$windSpeed!=0),]
  p0 <- plot.windrose(spd = winddata$windSpeed, 
                      dir = winddata$windBearing)
  p0
  
  precip<-hourlywdata %>% group_by(date) %>% summarize(precip=sum(precipIntensity))
  prec<-ggplot(precip)  + 
    geom_bar(aes(x=date, y=precip),stat="identity", fill="tan1", colour="sienna3") + theme_minimal() + ylab("Precipitation (mm)") + xlab("")
  prec
  
  
  temp<-ggplot(dailywdata)+
    geom_smooth(aes(x=time, y=temperatureHigh),stat="identity",color="red")+
    geom_smooth(aes(x=time, y=temperatureLow),stat="identity",color="blue")+ ylab("Temperature (C)") +  xlab("")
  temp
  
  if(write){
    ggsave(sprintf("../weather/figures/windrose_%s.png",sanit_vector(filename)),p0,width = 4, height = 3)
    ggsave(sprintf("../weather/figures/precipitation_%s.png",sanit_vector(filename)),prec,width = 4, height = 2)  
    ggsave(sprintf("../weather/figures/temperture_%s.png",sanit_vector(filename)),temp,width = 4, height = 2)
    write.csv(hourlywdata,sprintf("../weather/weather_hourly_%s.csv",sanit_vector(filename)))
    write.csv(dailywdata,sprintf("../weather/weather_daily_%s.csv",sanit_vector(filename)))
  }
}




plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 5,
                          dirres = 45,
                          spdmin = 0,
                          spdmax = 30,
                          spdseq = NULL,
                          palette = "Spectral",
                          countmax = NA,
                          debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- rev(colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range))
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,max(data[[spd]],na.rm = TRUE))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
  }
  
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)
  
   dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                   paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                         "-",
                         seq(3*dirres/2, 360-dirres/2, by = dirres)),
                   paste(360-dirres/2,"-",dirres/2))
  
  dir.labels <- c("NORTH","NE","EST","SE","SOUTH","SW","WEST","NW","NORTH")
  
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme_minimal() + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
          )
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}

