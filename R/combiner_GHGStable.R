library(datetime)
library(xts)
library(lubridate)
library(ggplot2)
library(dplyr)

Sys.setenv(TZ='Europe/Brussels')

setwd("/home/alexandre/GES/dev/ADAQ/R")

#################################
## Functions and values needed ##
#################################

# function that change the format of the csv file
csv2csv <- function(fileIn, fileOut){
  data.df <- read.csv(file=fileIn, header=TRUE, sep=",",dec=".", stringsAsFactors=FALSE)
  write.table(data.df, file=fileOut, sep=";",dec=",")
}

# function that check if entries are characters and identical -> return the value, if not identical, return NA and if other that character -> the mean.
adaptedMean <- function(values){

  if (is.character(values)){

    if (isTRUE(all.equal.character(head(values, n = 1), tail(values, n = 1)))){
      return(values[[1]])
    }
    else return(NA)
  }
  else return(mean(values, na.rm=TRUE))
}

## Function that aggregate the data frame by 5 sec using the adaptedMean
## To be added: time selection (over the three last hours?)

aggregBy5Sec <- function(suffix = "suf", file = "test.csv", timeTitle = "timeTitle", useTimeSel = FALSE, timeSel = 3){

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
mmolH2O <- 18.02 # g/mol

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

# celsius to kelvin
KelvinFct<-function(x){x+273.15}
# (g/hour per measuring point), MMg Molar mas of the gas (g/mole), ConcWetOut&ConcWetIn in ppm humid air.
# rely on constat mole of dry air at input and output, Flow wet is pc of max low of humid air per hour at output,
# Press and Temperature are the pressur (Pa) and temperature (°C) at air outlet
EmissionFCt<-function(DF,ConcWetOut,ConcWetIn,ConcWetOutH2O,ConcWetINH2O,Press,FlowWet,Temperature,GasEmitted){
  Emission_vector<-(DF[,ConcWetOut]-DF[,ConcWetIn]*((1-DF[,ConcWetOutH2O]/(10^6))/(1-DF[,ConcWetINH2O]/(10^6))))/(10^6)*(DF[,Press]*DF[,FlowWet]*MaxFlow/100)/(RConstantPa*KelvinFct(DF[,Temperature]))*MMg[GasEmitted][1,1]
  return(Emission_vector)
}


#######################
## Code start here ! ##
#######################

print(getwd())

# CVP data frame

#   badly written file
csv2csv("./../csv/CVPBarn/CVP_Barn_all_until20181113.csv", "./../csv/CVPBarn/CVP_Barn_all_until20181113_goodCsv.csv")
csv2csv("./../csv/SamplerBarn/GasSampling_all_until20181113.csv", "./../csv/SamplerBarn/GasSampling_all_until20181113_goodCsv.csv")


cvp_suffix <- "CVP"
cvp_files <- list(
  "./../csv/CVPBarn/CVP_Barn_all.csv",
  "./../csv/CVPBarn/CVP_Barn_all_until20181113_goodCsv.csv")
cvp_timeTitle <- "Datetime"
cvps.ls <- lapply(cvp_files,  function(x){ return(aggregBy5Sec(cvp_suffix, x, cvp_timeTitle))})
cvp.df <- unique(do.call("rbind", cvps.ls))

ggplot(cvp.df, aes(x=cvp.df$Datetime, y=cvp.df$Datetime)) + geom_point()

# ggplot(cvp.df, aes(x=Datetime, y=PC1))+geom_point()

# ADC data frame

adc_suffix <- "ADC"
adc_files <- list(
  "./../csv/ADCBarn/ADCGasData_18110800.CSV",
  "./../csv/ADCBarn/ADCGasData_18110900.CSV",
  "./../csv/ADCBarn/ADCGasData_18111000.CSV",
  "./../csv/ADCBarn/ADCGasData_18111100.CSV",
  "./../csv/ADCBarn/ADCGasData_18111200.CSV",
  "./../csv/ADCBarn/ADCGasData_18111300.CSV",
  "./../csv/ADCBarn/ADCGasData_18111400.CSV",
  "./../csv/ADCBarn/ADCGasData_18111500.CSV",
  "./../csv/ADCBarn/ADCGasData_18111600.CSV",
  "./../csv/ADCBarn/ADCGasData_18111700.CSV",
  "./../csv/ADCBarn/ADCGasData_18111800.CSV",
  "./../csv/ADCBarn/ADCGasData_18111900.CSV",
  "./../csv/ADCBarn/ADCGasData_18112000.CSV",
  "./../csv/ADCBarn/ADCGasData_18112100.CSV",
  "./../csv/ADCBarn/ADCGasData_18112200.CSV",
  "./../csv/ADCBarn/ADCGasData_18112300.CSV",
  "./../csv/ADCBarn/ADCGasData_18112400.CSV",
  "./../csv/ADCBarn/ADCGasData_18112500.CSV")
adc_timeTitle <- "AdjustedTime"

adcs.ls <- lapply(adc_files,  function(x){ return(aggregBy5Sec(adc_suffix, x, adc_timeTitle))})
adc.df <- unique(do.call("rbind", adcs.ls))

adcTimeCorr <- function(datetime){
  (datetime < ymd_hms("2018/11/17 13:00:00")) * -125 +
  (datetime > ymd_hms("2018/11/17 13:00:00") & datetime < ymd_hms("2018/11/20 12:00:00")) * -85
}


adc.df$oldtime5sec <- adc.df$ADCtime5sec
adc.df$ADCtime5sec <- adc.df$ADCtime5sec+as.difftime(adcTimeCorr(adc.df$ADCtime5sec),unit="secs")

ggplot(adc.df, aes(x=oldtime5sec, y=(difftime(adc.df$ADCtime5sec, adc.df$oldtime5sec)))) + geom_point()

# Gas Sampling data frame

gs_suffix  <- "GS"
gs_files <- list(
  "./../csv/SamplerBarn/GasSampling_all_until20181113_goodCsv.csv",
  "./../csv/SamplerBarn/GasSampling_all_until20181116.csv",
  "./../csv/SamplerBarn/GasSampling_all.csv")
gs_timeTitle <- "Time"
gss.ls <- lapply(gs_files, function(x){return(aggregBy5Sec(gs_suffix, x, gs_timeTitle))})
gs.df <- unique(do.call("rbind", gss.ls))

ggplot(gs.df, aes(x=Time, y=Time)) + geom_point()


# merging....

merged.df <- merge(cvp.df, adc.df, by.x = "CVPtime5sec", by.y = "ADCtime5sec", all = TRUE)
merged.df <- merge(merged.df, gs.df, by.x = "CVPtime5sec", by.y = "GStime5sec", all = TRUE)

colnames(merged.df)

merged.df <- merged.df[!(is.na(merged.df$CH4) | is.na(merged.df$Analyser_position) | is.na(merged.df$Status)),]

merged.df$transition <- !(lag(merged.df$Analyser_position) == merged.df$Analyser_position)
merged.df$transition_time <- ifelse(merged.df$transition, merged.df$CVPtime5sec, NA)
merged.df$transition_time <- na.locf(merged.df$transition_time, na.rm = FALSE)
merged.df$time_to_transition <- as.difftime(as.numeric(merged.df$CVPtime5sec-merged.df$transition_time), units="secs")

merged.df$statusChange <- !(lag(merged.df$Status) == merged.df$Status)
merged.df$statusChange_time <- ifelse(merged.df$statusChange, merged.df$CVPtime5sec, NA)
merged.df$statusChange_time <- na.locf(merged.df$statusChange_time, na.rm = FALSE)
merged.df$time_to_statusChange <- as.difftime(as.numeric(merged.df$CVPtime5sec-merged.df$statusChange_time), units="secs")

selected_data.df <- merged.df[merged.df$time_to_transition > 170 & merged.df$Status == "Connected" & merged.df$time_to_statusChange > 200,]

# aggregate by hour
selected_data.df$Datetime <- format(selected_data.df$AdjustedTime, '%Y-%m-%d %H')
selected_data.df$Date <- format(selected_data.df$AdjustedTime, '%Y-%m-%d')

hourly_data.df <- aggregate(selected_data.df, by = list(selected_data.df$Datetime, selected_data.df$Analyser_position), FUN = adaptedMean)
daily_data.df <- aggregate(selected_data.df, by = list(selected_data.df$Date, selected_data.df$Analyser_position), FUN = adaptedMean)

extValue <- function(df, gas, datetime){
  value <- df[which(df[,"Datetime"] == datetime & df[,"Analyser_position"] == "Ext"), gas]
  return(value)
}

gas_list <- list("CO2", "CH4", "N2O", "NH3", "H2O")

for (gas in gas_list){
 name <- paste0(gas,"_","Ext")
 hourly_data.df[, name] <- as.numeric(sapply(hourly_data.df$Datetime, function(x) {extValue(hourly_data.df, gas, x)}))
}

colnames(hourly_data.df)
hourly_data.df <- hourly_data.df[hourly_data.df$Analyser_position!="Ext",]

# Emmission Fct here!






help("")

ggplot(merged.df, aes(x=AdjustedTime, y=TimeDifference)) + ylim(-200,0) + geom_point()

# remove external rows

df <- merged.df
df <- selected_data.df
ggplot(df[day(df$AdjustedTime) == 15 & hour(df$AdjustedTime) == 3,], aes(x=CVPtime5sec, y=CH4, color=Analyser_position, shape=Status)) + geom_point()
df <- selected_data.df
ggplot(df[day(df$AdjustedTime) == 15,], aes(x=CVPtime5sec, y=CH4, color=Analyser_position, shape=Status)) + geom_point()
ggplot(df, aes(x=CVPtime5sec, y=CH4, color=Analyser_position, shape=Status)) + geom_point()
ggplot(hourly_data.df, aes(x=ymd_h(Datetime), y=CH4, color=Analyser_position)) + geom_point()


write.table(merged.df, file = "temp.csv", sep=";", row.names=FALSE)

ggplot(hourly_data.df, aes(x=Datetime, y=CO2em, col=as.character(Analyser_position))) + geom_point()
ggsave("./plots/CO2em.pdf")
ggplot(hourly_data.df, aes(x=Datetime, y=CH4em, col=as.character(Analyser_position))) + geom_point
ggsave("./plots/CH4em.pdf")

colnames(hourly_data.df)
ggplot(hourly_data.df, aes(x=ymd_h(Datetime), y=(CH4-CH4_Ext), col=as.character(Analyser_position))) + geom_smooth()
ggplot(hourly_data.df, aes(x=ymd_h(Datetime), y=(CO2-CO2_Ext), col=as.character(Analyser_position))) + geom_smooth()
ggplot(hourly_data.df, aes(x=ymd_h(Datetime), y=(CH4-CH4_Ext)/(CO2-CO2_Ext), col=as.character(Analyser_position))) + geom_smooth()
ggplot(hourly_data.df, aes(x=ymd_h(Datetime), y=(CH4-CH4_Ext)/(CO2-CO2_Ext), col=as.character(Analyser_position))) + geom_point()

hourly_data.df

ggplot(daily_data.df, aes(x=AdjustedTime, y=CH4, col=as.character(Analyser_position))) + geom_point()
