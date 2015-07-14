pars<-settings()

filename<-paste(format(Sys.time(), "%Y-%m-%d"),"BZ-Auswertung", sep=".")

clearBZData() # clear all stored data
setFilterMinDate(as.POSIXct("2015-04-21 14:00:00")) 
setFilterMaxDate(as.POSIXct("2015-05-05 23:59:59"))
#add datasets

path2Ipro   <- "/Volumes/Daten/FelixLindemann/Documents/Privat/Blutzucker Analyse/Werte/iPro/data_export_2015_04_28.csv"
addBZData(LoadIProSensor(path2Ipro),       "IPro Sensor", FALSE)
path2Ipro   <- "/Volumes/Daten/FelixLindemann/Documents/Privat/Blutzucker Analyse/Werte/iPro/data_export_2015_05_06.csv"
addBZData(LoadIProSensor(path2Ipro),       "IPro Sensor", FALSE) 
path2Mysugr <- "/Volumes/Daten/FelixLindemann/Documents/Privat/Blutzucker Analyse/Werte/mySugr/mysugr_data_2015-05-07_0840.csv"



addBZData(LoadMySugr(path2Mysugr)   , "Selbstaufschrieb" , TRUE)

setwd("/Volumes/Daten/FelixLindemann/Documents/Privat/Blutzucker Analyse/Reports/meine/")


# Copy datasets in one
PrepareBZData()

# ApplyFilter to min / max date/time
ApplyFilter()

preparePlot( PlotMethod="pdf",outputFileName=filename, onefile=pars$file.onefile)

# start evaluation
EvaluateBZ(byDay=TRUE)

plotInsulinWirksamkeit() 

EndPlot( PlotMethod="pdf")
