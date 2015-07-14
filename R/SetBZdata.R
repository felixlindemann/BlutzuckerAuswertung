addBZData <- function(data, datasource, isReference=FALSE){
  
  if(isValidBZDataframe(data) == FALSE){
    stop("Data can't be set.", 
         "Data-object is not a valid BZ-data.frame") 
  }
  data$ref <- isReference
  n <- length(.BZAuswertungEnv$rawdata)+1
  data$source <- n 
  data$Zeit <- as.POSIXct(round(as.numeric(data$Zeit)/60,digits=0)*60,origin = "1970-01-01")
  data$Zeitpunkt <- as.POSIXct(round(as.numeric(data$Zeitpunkt)/60,digits=0)*60,origin = "1970-01-01")
  .BZAuswertungEnv$rawdata[[n]] <- data
  .BZAuswertungEnv$datasource[[n]]  <- datasource
  
}  

clearBZData <- function() {
  
  .BZAuswertungEnv$data <- data.frame()  
  .BZAuswertungEnv$rawdata <- list()
  .BZAuswertungEnv$datasource <- list()
  
  .BZAuswertungEnv$xlim <- c(NA,NA)
  .BZAuswertungEnv$ylim <- c(0,320)
  
}

PrepareBZData <- function(){
  .BZAuswertungEnv$data <- data.frame()
  values <- .BZAuswertungEnv$rawdata 
  if(class(values) == "list"){
    for(i in 1:length(values)){
      BZ<-values[[i]]
      
      if(!isValidBZDataframe(BZ)){
        stop("Can't PrepareBZData:", 
             "unexpected data type for Ref-values[",i,"]. Please provide a data.frame instead of '",class(BZ),"'") 
      } 
      
      .BZAuswertungEnv$data <- rbind(.BZAuswertungEnv$data, BZ)
    }
    .BZAuswertungEnv$data <- .BZAuswertungEnv$data[ order(.BZAuswertungEnv$data$Zeitpunkt), ]
    .BZAuswertungEnv$dataFiltered <-.BZAuswertungEnv$data
  }else{ 
    .BZAuswertungEnv$data <- NA
    .BZAuswertungEnv$dataFiltered<-NA
  } 
}

ApplyFilter <- function(){
  
  pars <- settings() 
  .BZAuswertungEnv$dataFiltered<-NA
  
  if(!is.na(pars$Filter.max.Date) && class(pars$Filter.max.Date) == c("POSIXct","POSIXt")){
    #do nothing
    .BZAuswertungEnv$xlim[2] <- pars$Filter.max.Date
  }else{ 
    .BZAuswertungEnv$xlim[2] <- max(c(.BZAuswertungEnv$data$Zeitpunkt), na.rm=TRUE)
  }
  
  if(!is.na(pars$Filter.min.Date)&& class(pars$Filter.min.Date) == c("POSIXct","POSIXt")){
    
    .BZAuswertungEnv$xlim[1] <- pars$Filter.min.Date
  }else{ 
    .BZAuswertungEnv$xlim[1] <- min(c(.BZAuswertungEnv$data$Zeitpunkt), na.rm=TRUE)
  }
  
  
  BZ<-subset(.BZAuswertungEnv$data, Zeitpunkt <= .BZAuswertungEnv$xlim[2] &
                                    Zeitpunkt >= .BZAuswertungEnv$xlim[1]-new_duration(day=1)) 
  
  .BZAuswertungEnv$dataFiltered<- BZ
  #.BZAuswertungEnv$ylim[2] <- max(BZ$BZ, na.rm=TRUE)
}

plotInsulinWirksamkeit<-function(){
  
  
  pars <- settings()
  
  par(mar=c(5,5,5,1))   #c(bottom, left, top, right)
  nf <- layout(matrix(c(1,2),2,1,byrow=TRUE))
 # layout.show(nf) 
  
  offset <- as.POSIXct("2015-01-01 00:00:00")
  xlim<- c(0,  pars$Bolus.Duration) + offset
  v<-getBolus() *100
  V<-getBolus(TRUE)*100
  w<-getBasal() *100
  W<-getBasal(TRUE)*100 
  
  x <- seq(offset,offset + pars$Bolus.Duration, length=length(v))
  plot(x,V, type="l", main="Wirksamkeit des Bolus-Insulins", ylab = "Wirksamkeit in [%]", xlab = "Dauer nach Injektion", xlim = xlim, ylim=c(0,100), col=2)
  #lines(x,V,col=3)
  polygon(x,V,col=pars$Bolus.BG.Color, border = pars$Bolus.Color, lwd=2)  
  abline(h=0.0 + seq(0,1,by=0.2)*100, lwd=1, lty=2, col=pars$Grid.Color) 
  abline(h=0.0, lwd=2)
  
  abline(v=offset + seq(0,24,by=0.25)*60*60, lwd=1, lty=2, col=pars$Grid.Color)
  abline(v=offset, lwd=2) 
  
  xlim<- c(0, pars$Basal.Duration) +offset
  x <- seq(offset,offset + pars$Basal.Duration, length=length(w))
  
  plot(x,W, type="l", main="Wirksamkeit des Basal-Insulins", ylab = "Wirksamkeit in [%]", xlab = "Dauer nach Injektion", xlim = xlim, ylim=c(0,100), col=2)
  #lines(x,W,col=3)
  polygon(x,W,col=pars$Basal.BG.Color, border = pars$Basal.Color,lwd=2)  
  abline(h=0.0 + seq(0,1,by=0.2)*100, lwd=1, lty=2, col=pars$Grid.Color) 
  abline(h=0.0, lwd=2)
  
  abline(v=offset + seq(0,24,by=1)*60*60, lwd=1, lty=2, col=pars$Grid.Color)
  abline(v=offset, lwd=2) 
}

getBolus <- function(Dichte=FALSE){
  pars <- settings()
  
  x <- seq(0,1, length= as.numeric(pars$Bolus.Duration)/60)
  if(Dichte==TRUE){
    v<- dbeta(x,            pars$Bolus.Param[1], pars$Bolus.Param[2]) 
    v<-v/ max(v)
  }else{
    v<- 1- pbeta(x,         pars$Bolus.Param[1], pars$Bolus.Param[2]) 
  }
  return (v)
}

drawBG <- function(r,bp,vertical=FALSE,  xlim=NA, dx=NA){
  pars <- settings()
  if(is.na(dx)){
    dx <- pars$Grid.dx
  }
  if(pars$drawBG.Danger == TRUE){
    rect(-r,pars$Warning.Range[2] , r, 1000 ,  border=pars$Grid.Color , col =paste(pars$Danger.Color, pars$Alpha.Color, sep="")) 
    rect(-r,0, r,    pars$Warning.Range[1] ,   border=pars$Grid.Color , col =paste(pars$Danger.Color,  pars$Alpha.Color, sep="")) 
  }
  if(pars$drawBG.Warning == TRUE){
    rect(-r,pars$OK.Range[2], r, pars$Warning.Range[2], border=pars$Warning.Color , col =paste(pars$Warning.Color, pars$Alpha.Color, sep="")) 
    rect(-r,pars$Warning.Range[1], r, pars$OK.Range[1], border=pars$Warning.Color , col =paste(pars$Warning.Color,  pars$Alpha.Color, sep=""))  
  }
  if(pars$drawBG.OK == TRUE){
    rect(-r,pars$OK.Range[1], r, pars$OK.Range[2], border=pars$Ok.Color, col =paste(pars$Ok.Color,  pars$Alpha.Color, sep=""))  
  }
  
  if(pars$Grid.draw == TRUE){
    abline(h=seq(1,100,by=1)*pars$Grid.dy,  col=pars$Grid.Color, lty=pars$Grid.lty)
    if(vertical == TRUE){
      
      abline(v=seq(1,1000,by=1)*dx+xlim[1], col=pars$Grid.Color, lty=pars$Grid.lty)
    }
  }
  if(pars$BGStats.draw == TRUE)
  {
    abline(h=bp$stats,lty=pars$BGStats.lty,lwd=pars$BGStats.lwd, col=pars$BGStats.Color)  
  }
  abline(h=0) 
}

DangerRamp <- function(){
  pars <- settings()
   
    c0  <- colorRampPalette(c(pars$DangerO.Color,  pars$Danger.Color))
    c1 <- colorRampPalette(c(pars$Danger.Color,  pars$Warning.Color))
    c2 <- colorRampPalette(c(pars$Warning.Color, pars$OK.Color))
    c3 <- colorRampPalette(c(pars$OK.Color, pars$OkM.Color))
    c4 <- colorRampPalette(c(pars$OkM.Color, pars$Warning.Color))
    c5 <- colorRampPalette(c( pars$Warning.Color, pars$DangerO.Color))
    
    col<- c(c0(pars$Danger.Range[1]), 
            c1(pars$Warning.Range[1] - pars$Danger.Range[1]),
            c1(pars$OK.Range[1] - pars$Warning.Range[1]),
            c1(pars$OK.Range[2] - pars$OK.Range[1]),
            c1(pars$Warning.Range[2] - pars$OK.Range[2]),
            c1(pars$Danger.Range[2] - pars$OK.Range[2])     
    )
    return (col) 
}
testRamp<-function(){
  
  pars <- settings()
  x <- 0:10
  y <- 0:330
  z<-NULL
  z<- outer(x,y,function(a,b) b)
  
  ramp <-   colorRampPalette(c(pars$DangerO.Color,  pars$Danger.Color,  pars$Warning.Color, pars$OK.Color, pars$OkM.Color, pars$Warning.Color,pars$Danger.Color,  pars$DangerO.Color),
                                                                  space = "Lab")
  
filled.contour(x, y, z,  color.palette =ramp, ylim=c(0,330) )
  #contour(x, y, z, levels = seq(60, 200, by = 10),add = TRUE, col = "black", lty=2)
}


getBasal <- function(Dichte=FALSE){
  pars <- settings() 
  x <- seq(0,1, length= as.numeric(pars$Basal.Duration)/60)  
  if(Dichte==TRUE){
    v<-dbeta(x,         pars$Basal.Param[1], pars$Basal.Param[2]) 
    v<-v  / max(v)
  }else{
    v<- 1-pbeta(x,         pars$Basal.Param[1], pars$Basal.Param[2])  
  }
  return (v)
}

preparePlot<-function(...){
  pars<-settings()
  param<-list(...) 
  
  if(is.null(param$outputFileName)){
    param$outputFileName <- paste(format(Sys.time(), "%Y-%m-%d"),"BZ-Auswertung", sep=".")
  }
  if(pars$file.onefile == FALSE){
    param$outputFileName <- paste(param$outputFileName,".%03d", sep="")
  }
  if(is.null(param$PlotMethod)){ 
  } else if(param$PlotMethod == "plot"){
  } else if(param$PlotMethod == "pdf"){
    
    pdf(file=paste(param$outputFileName, ".pdf", sep=""), 
        width = pars$file.width , paper="a4",
        height = pars$file.height, 
        bg = pars$file.bg  )
  } else if(param$PlotMethod == "png"){
    
    png(filename=paste(param$outputFileName, ".png", sep=""), 
        width = pars$file.width ,
        height = pars$file.height, 
        bg = pars$file.bg  )
  } else if(param$PlotMethod == "bmp"){
    
    bmp(filename=paste(param$outputFileName, ".bmp", sep=""), 
        width = pars$file.width ,
        height = pars$file.height, 
        bg = pars$file.bg  )
  } else if(param$PlotMethod == "jpg"){
    
    jpeg(filename=paste(param$outputFileName, ".jpeg", sep=""), 
         width = pars$file.width ,
         height = pars$file.height, 
         bg = pars$file.bg  )
  } else if(param$PlotMethod == "tiff"){
    
    tiff(filename=paste(param$outputFileName, ".tiff", sep=""), 
         width = pars$file.width ,
         height = pars$file.height, 
         bg = pars$file.bg  )
  } else if(param$PlotMethod == "tikz"){
    tikz(file = paste(param$outputFileName, ".%03d.tex", sep=""), width = pars$file.width /96, onefile = pars$file.onefile, 
         height =  pars$file.height /96, onefile = TRUE, bg = "transparent",   standAlone = FALSE, timestamp = TRUE)
  }else{
    stop("Can't evaluate\n", 
         "unknown PlotMethod. Please provide a valid PlotMethod!") 
  }
}


EndPlot<-function(...){
  param <- list(...)
  if(is.null(param$PlotMethod)){ 
    return(TRUE)
  } else if(param$PlotMethod == "plot"){
    return(TRUE)
  } else if(param$PlotMethod == "png"){ 
  } else if(param$PlotMethod == "bmp"){ 
  } else if(param$PlotMethod == "jpg"){ 
  } else if(param$PlotMethod == "tiff"){ 
  } else if(param$PlotMethod == "tikz"){
  } else if(param$PlotMethod == "pdf"){ 
  }else{ 
    return(TRUE)
  }
  
  dev.off()
}

