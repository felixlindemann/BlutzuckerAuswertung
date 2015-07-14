LoadIProSensor <- function(sfile, returnProtokoll=FALSE){
  
  pars <- settings() 
  
  dt <- read.csv(sfile,
                 header = pars$IProSensor.ReadHeader, 
                 dec    = pars$IProSensor.dec, 
                 sep    = pars$IProSensor.sep,
                 quote  = pars$IProSensor.quote, 
                 skip   = pars$IProSensor.skip)
  
  t <- unique(colnames(dt)==pars$IProSensor.ColNames)
  if(!(length(t)==1 && t[1]== TRUE)) {
    stop("File can't be Loaded:", 
         "CSV could not be recocgnized as a IProSensordt<-.CSV") 
  }
 
  bzcol<-0
  if(returnProtokoll==TRUE){
    bzcol = pars$IProSensor.BZKalibrierung
    dt<-dt[dt[,16]=="LogbookDatum",]   
  }else{
    bzcol = pars$IProSensor.BZSensor
    dt<-dt[dt[,16]=="SGReceivedIPro",]
  }
   
  x<-
    data.frame(
      Zeitpunkt=strptime(paste(dt[,2],dt[,3]), "%d.%m.%y %T"),
      Datum=strptime(dt[,2], "%d.%m.%y"),
      Zeit=strptime(dt[,3], "%T"),
      BZ=dt[,bzcol],  
      Bolus=  dt[,14] == "Medikation", 
      Bolus.amount=NA, 
      Basal=  dt[,14] == "Medikation",
      Basal.amount=NA,
      BE =   dt[,12]  == "Mahlzeit",
      BE.amount=NA,  
      BE.Desc=NA, 
      Bewegung =   dt[,13] == "Bewegung",
      Bewegung.dur = NA,
      Bewegung.tag = NA,
      Tag=NA,                
      Notiz=NA,
      Ort = NA,
      ref = FALSE,
      source = 0
    )
  return(x)
}