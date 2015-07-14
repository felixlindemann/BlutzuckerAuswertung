EvaluateBZ <- function(...){
  
  param <- list(...)
  if(length(param)==1 & class(param[[1]])=="list"){
    param <- param[[1]]
    
  }
  #load default values
  pars <- settings() 
  
  
  # get data 
  if(is.null(param$data)){ 
    values <- .BZAuswertungEnv$dataFiltered
  }else{
    values <- param$data
  }
  if(!isValidBZDataframe(values)){ 
    stop("Can't evaluate\n", 
         "unexpected data type for values. Please provide a data.frame as specified!") 
  }   
  
  if(!is.null(param$byDay)){
    if(param$byDay == TRUE){
      param <- param[-which(names(param) =="byDay")]
      param$Batch<-TRUE
      
      D <- unique(values$Datum)
      for(d in D){
        
        setFilterMinDate(as.POSIXct(d,origin = "1970-01-01"))
        setFilterMaxDate(as.POSIXct(d +  new_duration( day = 1.5),origin = "1970-01-01" ))
         
        ApplyFilter()
         
        EvaluateBZ (param)
      }
      return(TRUE)
    } 
  }
  if(is.null(param$Batch)){
    preparePlot(param,pars)
  }else if(param$Batch==FALSE){ 
    preparePlot(param,pars)  
  }
  
  
  # get filter
  xlim <- .BZAuswertungEnv$xlim 
  ylim <- .BZAuswertungEnv$ylim  
  ylim[1] <- ylim[1]-50
  if(class(xlim) =="numeric"){
    xlim <- as.POSIXct(xlim, origin = "1970-01-01")
  } 
  
  
  
  # produce scale on x-Axis
  
  
  t <- as.POSIXct(seq(xlim[1]-new_duration(day=1), xlim[2]+new_duration( day = 1), by=new_duration( minute = 1)), origin = "1970-01-01")
  
  #get BE-Consumption, BAsal, BOlus as well as SPort and BZ Blutzucker
  BE <- subset(values, BE == TRUE )
  BA <- subset(values, Basal == TRUE)
  BO <- subset(values, Bolus == TRUE)
  SP <- subset(values, Bewegung == TRUE) 
  BZ <- subset(values, is.na(BZ) == FALSE & ref==FALSE) 
  BZref <- subset(values, is.na(BZ) == FALSE & ref==TRUE) 
  
  leg.txt <- NULL
  leg.col <- NULL
  leg.bg.col <- NULL
  leg.lty <- NULL
  leg.pch <- NULL
  leg.lwd <- NULL
  leg.cex <- NULL
  
  #create time-series for BE-Consumption, BAsal, BOlus as well as SPort and BZ Blutzucker
  if(pars$Basal.Plot == TRUE){
    BAts <- data.frame(Zeitpunkt = t, value= 0) # BA[!is.NA(BA$Basal.amount),]
    I <- which(!is.na(BA$Basal.amount))
    for(i in I){
      v <- getBasal()*BA[i,]$Basal.amount
      j <- which(t==BA[i,]$Zeitpunkt)
      if(length(j)==1){
        J <- min(j+length(v), nrow(BAts))
        V <- min(J-j, length(v))
        J <- j + V -1
        BAts[j:J,"value"]<- BAts[j:J,"value"]+v[1:V]
      }
    }
  }
  if(pars$Bolus.Plot == TRUE){
    BOts <- data.frame(Zeitpunkt = t, value= 0) # BO[!is.NA(BO$Bolus.amount),]
    I <-  which(!is.na(BO$Bolus.amount))
    for(i in I){
      v <- getBolus()*BO[i,]$Bolus.amount
      j <- which(t==BO[i,]$Zeitpunkt)
      if(length(j)==1){
        j<-j+1 # it's a delta with a lag of 1 minute!
        J <- min(j+length(v), nrow(BOts))
        V <- min(J-j, length(v))
        J <- j + V -1
        BOts[j:J,"value"]<- BOts[j:J,"value"]+v[1:V]
      }
    }
  }
  
  
  adj<-par()$adj
  r <- max(as.numeric(BZ$Zeitpunkt))*2   
  if(max(BOts$value)>0){ 
    nf <- layout(matrix(c(1,3,2,0,4,0),2,3,byrow=TRUE), width= c(2,10,1.5), height=c(8,2))
  }else{
    nf <- layout(matrix(c(1,3,2),1,3,byrow=TRUE), width= c(2,10,1.5))
  }
  
  
  nf <- layout(matrix(c(1,3,2),1,3,byrow=TRUE), width= c(2,10,1.5))
  #layout.show(nf) 
  MAIN<-paste(pars$plot.main,format(xlim[1],"%d. %b %Y")," - ",format(xlim[2],"%d. %b %Y"))
  ylab <- "Blutzucke(mg/dl)"
  
  
  #Plot BoxPlot
  par(mar=c(3,5,5,0))    #c(bottom, left, top, right)
  bp<-boxplot(BZ$BZ,main="",
              ylim=ylim,ylab="Blutzucker [mg/dL]",box=F) 
  drawBG(r, bp) 
  rug(BZ$BZ,side=4)
  
  #Plot Histogramm 
  par(mar=c(3,0,5,1))  #c(bottom, left, top, right)  
  d<-density(BZ$BZ)
  plot(d$y,d$x, main="Kern-Dichte",
       type="l",
       #xlab="Kern-Dichte",
       ylim=ylim,xaxt='n')
  drawBG(r, bp)
  
  par(mar=c(3,0,5,0))  #c(bottom, left, top, right)  
  par(adj=0) 
  plot(BZ$Zeitpunkt,
       BZ$BZ,
       type="n", col=1, 
       xlim=xlim, xlab="Zeitpunkt",
       ylim=ylim,ylab="Blutzucker [mg/dL]",
       main=MAIN ,xaxt='n',yaxt='n')
  par(adj=adj)  
  
  
  
  if(is.null(param$Grid.dx)){ 
    if(as.numeric(xlim[2])-as.numeric(xlim[1])>as.numeric(new_duration( day = 2))){ 
      param$Grid.dx <- new_duration( day = 1)
    }else{
      param$Grid.dx <-  new_duration( hour = 2)
    }
  }else{
    param$Grid.dx <- pars$Grid.dx
  }
  
  customlabels <-as.POSIXct(as.numeric(seq(xlim[1],xlim[2],by=as.numeric(param$Grid.dx))),origin="1970-01-01")
  
  
  if( param$Grid.dx < new_duration( day = 1)){
    
    axis(1, at=customlabels,labels=format(customlabels, "%a %H:%M ") )
  }else{
    
    axis(1, at=customlabels,labels=format(customlabels, "%a %d.%m ") )
  }
  
  
  
  drawBG(r, bp, TRUE,xlim, param$Grid.dx)
  
  if(pars$TS.Plot == TRUE){
    lines(BZ$Zeitpunkt,BZ$BZ, col=pars$TS.Color, lty=pars$TS.lty,lwd=pars$TS.lwd)
    
    leg.txt <- c(leg.txt, "Zeitreihe")
    leg.col <- c(leg.col, pars$TS.Color) 
    leg.bg.col <- c(leg.bg.col, pars$TS.BG.Color)
    leg.lty <- c(leg.lty, pars$TS.lty)
    leg.pch <- c(leg.pch, pars$TS.pch)
    leg.lwd <- c(leg.lwd, pars$TS.lwd)
    leg.cex <- c(leg.cex, pars$TS.cex)
  }
  
  if(pars$Ref.Plot == TRUE){
    lines(BZref$Zeitpunkt,BZref$BZ, col=pars$Ref.Color, lty=pars$Ref.lty, lwd=pars$Ref.lwd)
    points(BZref$Zeitpunkt,BZref$BZ, bg=pars$Ref.BG.Color, pch=pars$Ref.pch, cex=pars$Ref.cex)
    leg.txt <- c(leg.txt, "Zeitreihe (Referenz Werte)")
    leg.col <- c(leg.col, pars$Ref.Color)
    leg.bg.col <- c(leg.bg.col, pars$Ref.BG.Color)
    leg.lty <- c(leg.lty, pars$Ref.lty)
    leg.pch <- c(leg.pch, pars$Ref.pch)
    leg.lwd <- c(leg.lwd, pars$Ref.lwd)
    leg.cex <- c(leg.cex, pars$Ref.cex)
    
  }
  
  if(pars$Bolus.Plot == TRUE){
    
    leg.txt <- c(leg.txt, "wirksames Bolus-Insulin")
    leg.col <- c(leg.col, pars$Bolus.Color)
    leg.bg.col <- c(leg.bg.col, pars$Bolus.BG.Color)
    leg.lty <- c(leg.lty, pars$Bolus.lty)
    leg.pch <- c(leg.pch, pars$Bolus.pch)
    leg.lwd <- c(leg.lwd, pars$Bolus.lwd)
    leg.cex <- c(leg.cex, pars$Bolus.cex)
    
    if(pars$Basal.Plot == TRUE){  
      leg.txt <- c(leg.txt, "wirksames Basal-Insulin")
      leg.col <- c(leg.col, pars$Basal.Color)
      leg.bg.col <- c(leg.bg.col, pars$Basal.BG.Color)
      leg.lty <- c(leg.lty, pars$Basal.lty)
      leg.pch <- c(leg.pch, pars$Basal.pch)
      leg.lwd <- c(leg.lwd, pars$Basal.lwd)
      leg.cex <- c(leg.cex, pars$Basal.cex)
      
      polygon(BAts$Zeitpunkt,  BAts$value, col=pars$Basal.BG.Color, border = pars$Basal.Color, lwd= pars$Basal.lwd,lty= pars$Basal.lty)   
      BA <- subset(BA, is.na(Basal.amount))
      if(nrow(BA) >0){
        points(BA$Zeitpunkt, rep(-5, nrow(BA)), pch=pars$Basal.pch, bg=pars$Basal.BG.Color, col=pars$Basal.Color , cex=pars$Basal.Cex)
      } 
    }
    polygon(BOts$Zeitpunkt, - BOts$value, col=pars$Bolus.BG.Color, border = pars$Bolus.Color, lwd=1)   
    BO <- subset(BO, is.na(Bolus.amount))
    if(nrow(BO) >0){
      points(BO$Zeitpunkt, rep(-2, nrow(BO)), pch=pars$Bolus.pch, bg=pars$Bolus.BG.Color, col=pars$Bolus.Color , cex=pars$Bolus.cex)
    }
  }
  
  if(pars$BE.Plot == TRUE & nrow(BE) >0){
    
    leg.txt <- c(leg.txt, "BE")
    leg.col <- c(leg.col, pars$BE.Color)
    leg.bg.col <- c(leg.bg.col, pars$BE.BG.Color)
    leg.lty <- c(leg.lty, pars$BE.lty)
    leg.pch <- c(leg.pch, pars$BE.pch)
    leg.lwd <- c(leg.lwd, pars$BE.lwd)
    leg.cex <- c(leg.cex, pars$BE.cex)
    points(BE$Zeitpunkt, rep(-50, nrow(BE)), pch=pars$BE.pch, bg=pars$BE.BG.Color, col=pars$BE.Color , cex=pars$BE.cex)
    BE <- subset(BE, is.na(BE.amount) == FALSE ) 
    if(nrow(BE)>0){
      par(new=TRUE)
      plot(BE$Zeitpunkt, -BE$BE.amount,
           type="h",
           xlim=xlim, xlab="Zeitpunkt",
           ylim=ylim,ylab="Blutzucker [mg/dL]",
           xaxt='n',yaxt='n',lwd=pars$BE.lwd, col=pars$BE.Color, lty=pars$BE.lty  )
    }
  }
  
  legend("topright", 
         legend = leg.txt,
         col = leg.col,
         lty = leg.lty,
         pch = leg.pch,
         lwd = leg.lwd,
         pt.cex = leg.cex,
         pt.bg = leg.bg.col,
         fill = "white", horiz = FALSE,title ="Legende",seg.len  = 4
  )
   
}