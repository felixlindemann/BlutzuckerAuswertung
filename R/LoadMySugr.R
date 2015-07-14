LoadMySugr <- function(sfile, ...){
  
  pars <- settings() 
  
  dt <- read.csv(sfile,header = pars$MySugr.ReadHeader, 
                 dec=pars$MySugr.dec, 
                 sep=pars$MySugr.sep,
                 quote=pars$MySugr.quote)
  
  t <-unique( colnames(dt)==pars$MySugr.ColNames)
  if(length(t)!=1 || t[1]!= TRUE) {
    stop("File can't be Loaded:", 
         "CSV could not be recocgnized as a MySugr.CSV") 
  }
  
  I <- c(5,6,10)
  for(i in I){
    dt[,i] <- as.numeric(gsub("[,]", ".", dt[,i]))
  }
  
  return(
    data.frame(
      Zeitpunkt=strptime(paste(dt[,1], dt[,2]), "%d.%m.%Y %T"),
      Datum=strptime(dt[,1], "%d.%m.%Y"),
      Zeit=strptime(dt[,2], "%T"), 
      BZ=dt[,4],   
      Bolus=!is.na(dt[,5]) , 
      Bolus.amount=suppressWarnings(as.numeric(dt[,5])), #suppressWarnings(as.numeric("A"))
      Basal=!is.na(dt[,6]),
      Basal.amount=suppressWarnings(as.numeric(dt[,6])),
      BE = !is.na(dt[,10]) || !is.na(dt[,11]),
      BE.amount=suppressWarnings(as.numeric(dt[,10])),  
      BE.Desc=dt[,11],       
      Bewegung =   is.na(dt[,"Aktivitätsdauer..Minuten."]) == FALSE,
      Bewegung.dur = new_duration( min = dt[,"Aktivitätsdauer..Minuten."]),
      Bewegung.tag = dt[,"Aktivitätsbeschreibung"],
      Tag=dt[,3],                
      Notiz=dt[,15],                
      Ort=dt[,16] ,
      ref = FALSE,
      source = 0 
    )
  )  
}