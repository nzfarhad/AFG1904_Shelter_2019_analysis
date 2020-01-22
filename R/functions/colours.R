

# INDIVIDUAL COLORS


lighten <- function(color, factor = 1 ){
  factor <- 1+((factor-1)*.15)
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
  col
}

# for test
# testcol <- function(a) image(1:length(a), 1, as.matrix(1:length(a)), col=a, axes=FALSE , xlab="", ylab="")
# reach.style.color.rainbow.blues(12) %>% testcol


reach.style.color.red<-function(lightness=1){
  thecolr<-rgb(238/255,88/255,89/255)
  lighten(thecolr,lightness)
  
}

reach.style.color.darkgrey<-function(lightness=1){
  thecolr<-rgb(88/255,88/255,90/255)
  lighten(thecolr,lightness)
}

reach.style.color.lightgrey<-function(lightness=1){
  thecolr<-rgb(209/255,211/255,212/255)
  lighten(thecolr,lightness)
}


reach.style.color.beige<-function(lightness=1){
  thecolr<-rgb(210/255,203/255,184/255)
  lighten(thecolr,lightness)
}


reach.style.color.blue<-function(lightness=1){
  thecolr<-rgb(102/255,122/255,149/255)
  lighten(thecolr,lightness)
}


reach.style.color.agora<-function(lightness=1){
  thecolr<-rgb(94/255,28/255,35/255)
  lighten(thecolr,lightness)
}


# COLOUR TRIPLES

reach.style.color.reds<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach.style.color.red)
}
reach.style.color.darkgreys<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach.style.color.darkgrey)
}

reach.style.color.lightgreys<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach.style.color.lightgrey)
}

reach.style.color.beiges<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach.style.color.beige)
}

reach.style.color.blues<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach.style.color.blue)
}

reach.style.color.agoras<-function(){
  vapply(1:3,FUN.VALUE = vector(mode = 'character',length = 1),reach.style.color.agora)
}



reach.style.color.rainbow<-function(n){
cols<-  c(reach.style.color.reds(),reach.style.color.beiges(),rev(reach.style.color.lightgreys()),rev(reach.style.color.darkgreys()))
colorRampPalette(cols)(n)
}


reach.style.color.rainbow.blues<-function(n){
cols<-  c(reach.style.color.blues(),reach.style.color.beiges(),rev(reach.style.color.lightgreys()),rev(reach.style.color.darkgreys()))
colorRampPalette(cols)(n)
}


agora.style.color.rainbow<-function(n){
cols<-  c(reach.style.color.agoras(),reach.style.color.beiges(),rev(reach.style.color.lightgreys()),rev(reach.style.color.darkgreys()))
colorRampPalette(cols)(n)
}



# GGPLOT GRADIENTS
scale_fill_reach <- function(color=NULL){
  if(is.null(color)){
    structure(list(
      scale_fill_manual(values= reach.style.color.rainbow(12))
    ))    
  }else{
  structure(list(
    scale_fill_manual(values= get(paste0('reach.style.color.',color,'s'))())
    
  ))}
}

# GGPLOT GRADIENTS
scale_fill_reach.blues <- function(color=NULL){
  if(is.null(color)){
    structure(list(
      scale_fill_manual(values= reach.style.color.rainbow.blues(12))
    ))    
  }else{
  structure(list(
    scale_fill_manual(values= get(paste0('reach.style.color.',color,'s'))())
    
  ))}
}

# GGPLOT AGORA
scale_fill_agora <- function(color=NULL){
  if(is.null(color)){
    structure(list(
      scale_fill_manual(values = agora.style.color.rainbow(12))
    ))    
  }else{
  structure(list(
    scale_fill_manual(values= get(paste0('reach.style.color.',color,'s'))())
    
  ))}
}

scale_color_discrete_reach <- function(color='red'){
  
  structure(list(
    scale_color_manual(values= get(paste0('reach.style.color.',color,'s'))())
  ))
}

scale_color_continuous_reachn <- function(color='red'){
  
  structure(list(
    scale_color_gradientn(colours = get(paste0('reach.style.color.',color,'s'))())
  ))
}



