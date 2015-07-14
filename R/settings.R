generateDefaultSettings <- function(){
  l <- list()
  t <- list() 
  
  l$OK.Range <- c(80,130)
  t$OK.Range <- class( l$OK.Range)
  
  l$Warning.Range <- c(60,200) 
  t$Warning.Range <- class( l$Warning.Range)  
  
  l$BGStats.draw <- TRUE
  t$BGStats.draw <- class( l$BGStats.draw)
  l$BGStats.Color <- "#CF7800"
  t$BGStats.Color <- class( l$BGStats.Color)  
  l$BGStats.lty <- c(2,2,1,2,2)
  t$BGStats.lty <- class( l$BGStats.lty)
  l$BGStats.lwd <- 2#c(1,1,1.5,1,1)
  t$BGStats.lwd <- class( l$BGStats.lwd)
  
  l$Grid.draw <- TRUE
  t$Grid.draw <- class( l$Grid.draw)
  l$Grid.Color <- "#797A4290"
  t$Grid.Color <- class( l$Grid.Color) 
  l$Grid.dx <- new_duration( hour = 2)
  t$Grid.dx <- class( l$Grid.dx) 
  l$Grid.dy <- 20
  t$Grid.dy <- class( l$Grid.dy) 
  l$Grid.lty <-2
  t$Grid.lty <- class( l$Grid.lty)
  l$Grid.lwd <- 0.75
  t$Grid.lwd <- class( l$Grid.lwd)
  
  l$Danger.Color <- "#FF280D"
  t$Danger.Color <- class( l$Danger.Color)
  
  l$DangerO.Color <- "#D60700"
  t$DangerO.Color <- class( l$DangerO.Color)
  
  l$Warning.Color <- "#FFC000"
  t$Warning.Color <- class( l$Warning.Color)
  
  l$Ok.Color <- "#22FF27"
  t$Ok.Color <- class( l$Ok.Color)
  
  l$OkM.Color <- "#00FF0A"
  t$OkM.Color <- class( l$OkM.Color)
  
  
  l$drawBG.OK <- TRUE
  t$drawBG.OK <- class( l$drawBG.OK)
  l$drawBG.Warning <- TRUE
  t$drawBG.Warning <- class( l$drawBG.Warning)
  l$drawBG.Danger <- FALSE
  t$drawBG.Danger <- class( l$drawBG.Danger)
  
  l$Alpha.Color <- "20"
  t$Alpha.Color <- class( l$Alpha.Color)
  
  l$plot.main <- "Blutzucker Felix Lindemann"      
  t$plot.main<-"character" 
  
  t$Filter.min.Date <- c("POSIXct","POSIXt")
  l$Filter.min.Date<-as.POSIXct(NA) 
  
  t$Filter.max.Date <- c("POSIXct","POSIXt")
  l$Filter.max.Date<-as.POSIXct(NA)  
   
  l$file.width <- 800
  t$file.width <- class(l$file.width)
  l$file.height <- 600 
  t$file.height <- class(l$file.height)
  l$file.onefile <- TRUE
  t$file.onefile <- class(l$file.onefile) 
  l$file.bg <- "white" 
  t$file.bg <- class(l$file.bg)
  l$file.type <-  "cairo-png" 
  t$file.type <- class(l$file.type)
  
  l$TS.Plot <- TRUE
  t$TS.Plot <- class(l$TS.Plot)
  l$TS.pch <- 0
  t$TS.pch <- class( l$TS.pch)
  l$TS.cex <- 1
  t$TS.cex <- class( l$TS.cex)
  l$TS.lty <- 1
  t$TS.lty <- class( l$TS.lty)
  l$TS.lwd <- 1
  t$TS.lwd <- class( l$TS.lwd)
  l$TS.BG.Color <- "#00016E80"
  t$TS.BG.Color <- class( l$TS.BG.Color)
  l$TS.Color <- "#00016E"
  t$TS.Color <- class( l$TS.Color)
  
  l$Ref.Plot <- TRUE
  t$Ref.Plot <- class(l$Ref.Plot) 
  l$Ref.pch <- 23
  t$Ref.pch <- class( l$Ref.pch)
  l$Ref.cex <- 1
  t$Ref.cex <- class( l$Ref.cex)
  l$Ref.lty <- 2
  t$Ref.lty <- class( l$Ref.lty)
  l$Ref.lwd <- 1
  t$Ref.lwd <- class( l$Ref.lwd)
  l$Ref.BG.Color <- "#E8210480"
  t$Ref.BG.Color <- class( l$Ref.BG.Color)
  l$Ref.Color <- "#E82104"
  t$Ref.Color <- class( l$Ref.Color)
  
  l$BE.Plot <- TRUE
  t$BE.Plot <- class(l$BE.Plot)
  l$BE.lty <- 1
  t$BE.lty <- class( l$BE.lty)
  l$BE.lwd <- 6
  t$BE.lwd <- class( l$BE.lwd)
  l$BE.pch <- 17
  t$BE.pch <- class( l$BE.pch)
  l$BE.cex <- 1.25
  t$BE.cex <- class( l$BE.cex)
  l$BE.BG.Color <- "#00221780"
  t$BE.BG.Color <- class( l$BE.BG.Color)
  l$BE.Color <- "#002217"
  t$BE.Color <- class( l$BE.Color)
  
  l$SP.Plot <- TRUE
  t$SP.Plot <- class(l$SP.Plot)
  l$SP.pch <- 17
  t$SP.pch <- class( l$SP.pch)
  l$SP.cex <- 1.25
  t$SP.cex <- class( l$SP.cex)
  l$SP.BG.Color <- "#4474FF"
  t$SP.BG.Color <- class( l$SP.BG.Color)
  l$SP.Color <- "#0007A6"
  t$SP.Color <- class( l$SP.Color)
  
  l$Bolus.Plot <- TRUE
  t$Bolus.Plot <- class(l$Bolus.Plot)
  l$Bolus.Duration <- new_duration( hour = 5) 
  t$Bolus.Duration<-"Duration"  
  l$Bolus.Param <- c(1.1,3)
  t$Bolus.Param<-"numeric"  
  l$Bolus.lty <- 1
  t$Bolus.lty <- class( l$Bolus.lty)
  l$Bolus.lwd <- 1
  t$Bolus.lwd <- class( l$Bolus.lwd)
  l$Bolus.pch <- 24
  t$Bolus.pch <- class( l$Bolus.pch) 
  l$Bolus.cex <- 1.25
  t$Bolus.cex <- class( l$Bolus.cex)
  l$Bolus.BG.Color <- "#93FF9A60"
  t$Bolus.BG.Color <- class( l$Bolus.BG.Color)
  l$Bolus.Color <- "#008A3A"
  t$Bolus.Color <- class( l$Bolus.Color)
  
  l$Basal.Plot <- TRUE
  t$Basal.Plot <- class(l$Basal.Plot)
  l$Basal.Duration <- new_duration( hour = 12)         
  t$Basal.Duration<-"Duration"   
  l$Basal.Param <- c(1.025,1.025)
  t$Basal.Param<-"numeric"  
  l$Basal.lty <- 1
  t$Basal.lty <- class( l$Basal.lty)
  l$Basal.lwd <- 1
  t$Basal.lwd <- class( l$Basal.lwd)
  l$Basal.pch <- 24
  t$Basal.pch <- class( l$Basal.pch)
  l$Basal.cex <- 1.25
  t$Basal.cex <- class( l$Basal.cex)
  l$Basal.BG.Color <- "#FFD04460"
  t$Basal.BG.Color <- class( l$Basal.BG.Color)
  l$Basal.Color <- "#FF160D"
  t$Basal.Color <- class( l$Basal.Color)
  
  #MySugr
  l$MySugr.ReadHeader <- TRUE   
  t$MySugr.ReadHeader<-"logical"  
  
  l$MySugr.dec <- "."      
  t$MySugr.dec<-"character" 
  
  l$MySugr.sep <- ","               
  t$MySugr.sep<-"character"    
  
  l$MySugr.quote <- "\""    
  t$MySugr.quote<-"character" 
  
  l$MySugr.skip <- 0       
  t$MySugr.skip<-"numeric"                           
  
  t$MySugr.ColNames<-"character"
  l$MySugr.ColNames <- c(
    "Datum",
    "Zeit",
    "Tags",
    "Blutzuckermessung..mg.dl.",
    "Bolusinjektionseinheiten..Pen.",
    "Basalinjektionseinheiten",
    "Bolusinjektionseinheiten..Pump.",
    "Bolus..Mahlzeit.",
    "Bolus..Korrektur.",
    "Temp..Basalprozent",
    "Temp..Basaldauer..Minuten.",
    "Mahlzeitkohlenhydrate..Gramm..Faktor.12.",
    "Mahlzeitbeschreibung",
    "Aktivitätsdauer..Minuten.",
    "Aktivitätsintensität..1..Bequem..2..Normal..3..Anstrengend.",
    "Aktivitätsbeschreibung",
    "Schritte",
    "Notiz",
    "Ort",
    "Blutdruck",
    "Körpergewicht..kg.",
    "HbA1c..mmol.mol.",
    "Ketone",
    "Nahrungsbestandteile",
    "Medikamente"
  )
  
  #IProSensor 
  l$IProSensor.ReadHeader <- TRUE                        
  t$IProSensor.ReadHeader<-"logical" 
  
  l$IProSensor.dec <- ","                                
  t$IProSensor.dec<-"character" 
  
  l$IProSensor.sep <- "\t"                                
  t$IProSensor.sep<-"character"
  
  l$IProSensor.quote <- "\""                             
  t$IProSensor.quote<-"character" 
  
  l$IProSensor.skip <- 11                                 
  t$IProSensor.skip<-"numeric" 
  
  l$IProSensor.BZSensor<- 10                            
  t$IProSensor.BZSensor<-"numeric" 
  
  l$IProSensor.BZKalibrierung<- 7                       
  t$IProSensor.BZKalibrierung<-"numeric" 
  
  t$IProSensor.ColNames <- "character"
  l$IProSensor.ColNames <- c(
    "Index",
    "Datum",
    "Zeit",
    "Zeitstempel",
    "Quelle",
    "Ausgeschlossen",
    "BZ.Messwert..mg.dl.",
    "Für.die.Kalibrierung.verwendet",
    "ISIG.Wert",
    "Sensorglukose..mg.dl.",
    "Sensorereignis",
    "Mahlzeit",
    "Bewegung",
    "Medikation",
    "Sonstiges",
    "Roh.Typ",
    "Roh.Werte"
  )
  
  # ContourNext
  l$ContourNext.ReadHeader <- TRUE                        
  t$ContourNext.ReadHeader<-"logical" 
  
  l$ContourNext.dec <- "."                                
  t$ContourNext.dec<-"character" 
  
  l$ContourNext.sep <- ","                                
  t$ContourNext.sep<-"character"
  
  l$ContourNext.quote <- "\""                             
  t$ContourNext.quote<-"character" 
  
  l$ContourNext.skip <- 0                                 
  t$ContourNext.skip<-"numeric" 
  
  l$ContourNext.Colnames <- c("Datum",
                              "Uhrzeit",
                              "Blutzucker..mg.dL.",
                              "Referenzmethode",
                              "Nüchtern",
                              "Vor.Mahlzeit",
                              "Nach.Mahlzeit",
                              "Tagebuch",
                              "Gelöscht",
                              "Kontroll.Lösung",
                              "Manuell",
                              "A1c..",
                              "A1c.mmol.mol",
                              "Insulin",
                              "Insulin.1",
                              "Kohlenhydrate.in.Gramm",
                              "Berechnungseinheiten..BE.",
                              "Kohlenhydrateinheiten..KE.",
                              "Kommentar"                   # 19
  )
  t$ContourNext.Colnames <-  class(l$ContourNext.Colnames)
  
  
  class(l) <- "BZAuswertungSettings" 
  attr(l, "type") <- t
  return(l)
}


isValidBZDataframe<-function(x){
  if (class(x) != "data.frame")
  {
    return(FALSE)
  }
  cnames <- c("Zeitpunkt",
              "Datum",
              "Zeit",
              "BZ",
              "Bolus",
              "Bolus.amount",
              "Basal",
              "Basal.amount",
              "BE",
              "BE.amount",
              "BE.Desc",
              "Bewegung",
              "Bewegung.dur",
              "Bewegung.tag",
              "Tag",
              "Notiz",
              "Ort", 
              "ref", 
              "source")
  t <- unique( colnames(x)  == cnames)
  if(length(t) != 1) {
    return(FALSE)
  } 
  return(t[1])
}

typecheck <- function(x, type){
  f <- switch(type,
              logical=is.logical,
              numeric=is.numeric)
  f(x)
}


# x  object of class BZAuswertungSettings 
checkSettingsIntegrity <- function(x, do.print=TRUE){
  if (class(x) != "BZAuswertungSettings")
    stop("settings integrity check cannot be performed", 
         "as objects class is not 'BZAuswertungSettings'") 
  types <- attr(x, "type")
  np <- !mapply(typecheck, x, types)           # not passed
  if (any(np)) { 
    for (par.name in names(x[np]))
      if (do.print) 
        cat("Parameter '", par.name, "' must be ", 
            types[[par.name]], "\n", sep="")
    stop("error in definition of parameters")
  } else {
    return(TRUE)
  }
}


setDefaultSettings <- function(){
  .BZAuswertungEnv$settings <- generateDefaultSettings()
}

setFilterMinDate <- function(value){
  tmp <- unique( class(value)==c("POSIXct","POSIXt") )
  if(length(tmp) > 1 || tmp[1] == FALSE){ 
    stop("Filter Min.Date can't be set.\n", 
         "Value is not of Type 'POSIXct' or 'POSIXt'") 
  } 
  
  .BZAuswertungEnv$settings$Filter.min.Date <- value
  
}


setFilterMaxDate <- function(value){
  tmp <- unique( class(value)==c("POSIXct","POSIXt") )
  if(length(tmp) != 1 || tmp[1] != TRUE){ 
    stop("Filter Max.Date can't be set.\n", 
         "Value is not of Type 'POSIXct' or 'POSIXt'") 
  } 
  
  .BZAuswertungEnv$settings$Filter.max.Date <- value
  
}



#' global settings for BZAuswertung 
#' 
#' @param ...       Use parameter value pairs (\code{par1=val1, par2=val2}) to 
#'                  change a parameter. Use parameter names to request  
#'                  parameter's value (\code{"par1", "par2"}).
#' @note   Currently the following parameters can be changed, ordered by topic.
#' The default value is shown in the brackets at the end of a line. 
#' 
#'  \itemize{
#'    \item{\code{Bolus.Duration}} {Wie lange wirkt das Bolus Insulin? (\code{4h}) }
#'    \item{\code{Basal.Duration}} {Wie lange wirkt das Basal Insulin? (\code{12h}) }  
#'    \item{\code{MySugr.ReadHeader}} {Hat die MySugr-Datei eine Kopfzeile? (\code{TRUE}) }  
#'    \item{\code{xxx}} {xyx? (\code{yxy}) }  
#'  } 
#' @export
#' @examples \dontrun{
#' # get current settings
#' settings() 
#'
#' # get some parameters
#' settings("Bolus.Duration", "Basal.Duration")
#' 
#' }
#'
settings <- function (...)
{
  parnames <- names(generateDefaultSettings())
  cur.settings <- .BZAuswertungEnv$settings
  args <- list(...) 
  if (length(args) == 0)                              # get all argumnets
    return(cur.settings)
  # get args
  if (is.null(names(args)) & 
        all(unlist(lapply(args, is.character)))) {
    pm <- pmatch(unlist(args), parnames)
    #if (length(pm) == 1L)
    #  return(cur.settings[pm][[1L]])
    return(cur.settings[na.omit(pm)])     
  } else {                                                  # set arguments
    names(args) <- parnames[pmatch(names(args), parnames)]  # partial matching of names
    new.settings <- modifyList(cur.settings, args)          # modify settings list   
    passed <- checkSettingsIntegrity(new.settings)
    if (passed) {
      .BZAuswertungEnv$settings <- new.settings              # replace settings       
      invisible(new.settings)
    }       
  }   
} 


#' subset method for BZAuswertungSettings class 
#' @export
#' @rdname BZAuswertungSettings
#' @method [ BZAuswertungSettings
#' @keywords internal
#'
`[.BZAuswertungSettings` <- function(x, i, ...){
  types <- attr(x, "type")
  x <- unclass(x)
  x <- x[i]         
  attr(x, "type") <- types
  class(x) <- "BZAuswertungSettings"
  x
}


#' Print method for BZAuswertungSettings class 
#' @export
#' @rdname BZAuswertungSettings
#' @method print BZAuswertungSettings
#' @keywords internal
#'
print.BZAuswertungSettings <- function(x, ...){
  cat("------------------------\n")
  cat("Settings for BZAuswertung\n")
  cat("------------------------\n")
   
  for(n in names(x)){
    if(!is.null(x[n])){
      if(class(x[n])=="list"){
        cat(paste("\t",n,":\n") )
        print(x[n])
      }else{ 
        cat(paste("\t",n,":",  x[n],"\n"))
      }
    }
  } 
}    


#' Save BZAuswertung settings
#' 
#' The current settings of BZAuswertung can be saved into a file with
#' the extension \code{.orgset}.
#'
#' @param file    Path of the file to be saved to. If a path is not supplied
#'                an interactive file saver dialog is opened.
#' @export
settingsSave <- function(file){
  if (missing(file)) {    
    args <- list("tk_getSaveFile", title="Select files", 
                 filetypes= "{{setting file} {.orgset}}")                          
    file <- tclvalue(do.call(tcl, args))
  }
  # TODO: check for orgset extension?
  saveRDS(.BZAuswertungEnv$settings ,file=file)
}


#' Load BZAuswertung settings
#' 
#' BZAuswertung settings saved in an a settings file with
#' the extension \code{.orgset} can be loaded to restore the
#' settings.
#'
#' @param file    Path of the file to be loaded. If a path is not supplied
#'                an interactive file chooser dialog is opened.  
#' @export
settingsLoad <- function(file){
  if (missing(file)){  
    Filters <- matrix(c("setting file", ".orgset"),
                      ncol=2, byrow = TRUE)
    file <- tk_choose.files(filters = Filters, multi=FALSE)    # returns complete path                     
  }
  orgset <- readRDS(file)
  if (class(orgset) != "BZAuswertungSettings")
    stop("file", file, "is no valid BZAuswertung settings file")
  .BZAuswertungEnv$settings <- orgset                          # save in environment in namespace
}

