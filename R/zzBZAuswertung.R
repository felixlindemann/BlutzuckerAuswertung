options(stringsAsFactors = FALSE)
.BZAuswertungEnv <- new.env()
assign("settings",  list(), envir = .BZAuswertungEnv)
assign("data",  NA, envir = .BZAuswertungEnv)
assign("rawdata",  NA, envir = .BZAuswertungEnv)
assign("datasource",  NA, envir = .BZAuswertungEnv)
 
.onAttach <- function(lib, pkg){
  packageStartupMessage(
    "------------------------------------------------",
    "\n BZ Auwswertung Version ",  utils::packageDescription("BlutzuckerAuswertung", field="Version"), 
    "\n Tools for the analysis of blood sugar data", 
    "\n CAUTION: The package is in alpha phase.",
    "\n          Design changes may still occur.", 
    "\n------------------------------------------------", 
    appendLF = TRUE)
  
  # invisible object saved in environment in namespace
  setDefaultSettings()
}

