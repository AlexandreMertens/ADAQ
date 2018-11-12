library(xts)
library(datetime)
library(lubridate)

Sys.setenv(TZ='Europe/Brussels')

setwd("C:/Users/m.mathot.admin/Desktop/ADAQ/R")

#################################
## Functions and values needed ##
#################################

# function that check if entries are characters and identical -> return the value, if not identical, return NA and if other that character -> the mean.
adaptedMean <- function(values){
  
  if (is.character(values)){
    
    if (isTRUE(all.equal.character(head(values, n = 1), tail(values, n = 1)))){
      return(values[[1]])
    }
    else return(NA)
  }
  else return(mean(values))
}

## Function that aggregate the data frame by 5 sec using the adaptedMean
## To be added: time selection (over the three last hours?)

aggregBy5Sec <- function(suffix, file, timeTitle, useTimeSel = FALSE, timeSel = 3){

  data.df <- read.csv(file=file, header=TRUE, sep=";",dec=",", stringsAsFactors=FALSE)    
  data.df[,c(timeTitle)] <- ymd_hms(data.df[,c(timeTitle)])
  if (useTimeSel){
    data.df <- data.df[data.df[, c(timeTitle)] > Sys.time()-as.difftime(timeSel, units = "hours"),]
    if (nrow(data.df) == 0) {
      print(paste("No entries in", file, " in the last", timeSel, "hours"))
    }
  }
  data.df <- data.df[complete.cases(data.df[,c(timeTitle)]),]
  data.df$newtimeTitle <- ymd_hms(data.df[,c(timeTitle)]) - as.difftime(as.numeric(ymd_hms(data.df[,c(timeTitle)])) %% 5, units = "secs")

  data5sec.df <- aggregate(data.df[,colnames(data.df) != "newtimeTitle"], 
                           by = data.df["newtimeTitle"],
                           FUN = adaptedMean
  )
  
  new5sectimeTitle <- paste0(suffix,"time5sec")
  colnames(data5sec.df)[which(names(data5sec.df) == "newtimeTitle")] <- new5sectimeTitle

  return(data5sec.df)
}

R <- 0.08206 # l*atm/mol/K
mmolCH4 <- 16.05 # g/mol 
mmolCO2 <- 44.01 # g/mol
mmolN2O <- 44.01 # g/mol
mmolNH3 <- 17.03 # g/mol

## compute emissions from :
##    cc: concentration of the analysed gaz in ppm
##    pc: percentage of ventilation (%)
##    pa: atmospheric pressure (atm)
##    t: temperature (celsius)
##    mmol: molar mass of the analysed gaz (g/mol)

computeEmission <- function(cc, pc, pa = 1, t, mmol){
  
  em <- 
    cc/1000000 * 
    pc/100 * (2850 * 1000) *
    pa *
    1 / R *
    1 / (t + 273.15) * 
    mmol
  
  return(em)
}

#######################
## Code start here ! ##
#######################

# CVP data frame
cvp_suffix <- "CVP"
cvp_file <- "./csv/CVP_Barn/CVP_Barn_201811061155.csv"
cvp_timeTitle <- "Datetime"
cvp.df <- aggregBy5Sec(cvp_suffix, cvp_file, cvp_timeTitle)

# ADC data frame

adc_suffix <- "ADC"
adc_file <- "./CSV/ADCGasData_18110600.CSV"
adc_timeTitle <- "AdjustedTime"
adc.df <- aggregBy5Sec(adc_suffix, adc_file, adc_timeTitle)

# Gas Sampling data frame

gs_suffix  <- "GS"
gs_file <- "./CSV/GasSampling_201811060956.csv"
gs_timeTitle <- "Time"
gs.df <- aggregBy5Sec(gs_suffix, gs_file, gs_timeTitle)

# merging all

# View(cvp.df)
# View(adc.df)
# View(gs.df)

merged.df <- merge(cvp.df, adc.df, by.x = "CVPtime5sec", by.y = "ADCtime5sec", all = TRUE)
merged.df <- merge(merged.df, gs.df, by.x = "CVPtime5sec", by.y = "GStime5sec", all = TRUE)

merged.df$PC <- 
  merged.df$PC1*(merged.df$Analyser_position == 1) + 
  merged.df$PC2*(merged.df$Analyser_position == 2) +
  merged.df$PC3*(merged.df$Analyser_position == 3) +
  merged.df$PC4*(merged.df$Analyser_position == 4)

merged.df$PCExt <- merged.df$PC # TO BE CORRECTED FOR H20
  
merged.df$Temp <-
  merged.df$Temp1*(merged.df$Analyser_position == 1) + 
  merged.df$Temp2*(merged.df$Analyser_position == 2) +
  merged.df$Temp3*(merged.df$Analyser_position == 3) +
  merged.df$Temp4*(merged.df$Analyser_position == 4) +
  merged.df$TempExt*(merged.df$Analyser_position == 5)

merged.df$CH4em <- computeEmission(cc = merged.df$CH4, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolCH4)
merged.df$CO2em <- computeEmission(cc = merged.df$CO2, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolCO2)
merged.df$N2Oem <- computeEmission(cc = merged.df$N2O, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolN2O)
merged.df$NH3em <- computeEmission(cc = merged.df$NH3, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolNH3)

merged.df <- merged.df[!is.na(merged.df$CH4em),]

View(merged.df)
