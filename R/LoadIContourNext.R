LoadContourNext <- function(sfile){
  
  pars <- settings() 
  
  d <- read.csv(sfile,header = pars$MySugr.ReadHeader, 
                dec=pars$MySugr.dec, 
                sep=pars$MySugr.sep,
                quote=pars$MySugr.quote)
  
  t <-unique( colnames(d)==pars$ContourNext.Colnames)
  if(length(t)!=1 || t[1]!= TRUE) { 
    
    stop("File can't be Loaded:", 
         "CSV could not be recocgnized as a ContourNext.CSV") 
  }
  
  return(
    data.frame(
      Zeitpunkt=strptime(paste(d[,1], d[,2]), "%m/%d/%Y %T"),
      Datum=strptime(d[,1], "%m/%d/%Y"),
      Zeit=strptime(d[,2], "%T"), 
      BZ=d[,3],   
      Bolus=!is.na(d[,14]) && !is.na(d[,15]) && d[,15] =="Schnellwirks", 
      Bolus.amount=(!is.na(d[,15]) && d[,15] =="Schnellwirks")*suppressWarnings(as.numeric(d[,14])), #suppressWarnings(as.numeric("A"))
      Basal=!is.na(d[,14]) &&  is.na(d[,15]),
      Basal.amount=(!is.na(d[,14]) &&  is.na(d[,15]))*suppressWarnings(as.numeric(d[,6])),#suppressWarnings(as.numeric("A"))
      BE = !is.na(d[,13]),
    BE.amount=suppressWarnings(as.numeric(d[,13])),  
    BE.Desc=NA,       
    Bewegung =   NA,
    Bewegung.dur = NA,
    Bewegung.tag = NA,
    Tag=NA,                
    Notiz=d[,19],                
    Ort=NA,
    ref = FALSE,
    source = 0 
  )
  
  )
}