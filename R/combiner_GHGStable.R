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

print(getwd())

# CVP data frame
cvp_suffix <- "CVP"
cvp_file <- "./../csv/CVP_Barn/CVP_Barn_all.csv"
cvp_timeTitle <- "Datetime"
cvp.df <- aggregBy5Sec(cvp_suffix, cvp_file, cvp_timeTitle)

# ADC data frame

adc_suffix <- "ADC"
adc_file <- "./../csv/ADCGasData/ADCGasData_18111400.CSV"
adc_timeTitle <- "AdjustedTime"
adc.df <- aggregBy5Sec(adc_suffix, adc_file, adc_timeTitle)

# Gas Sampling data frame

gs_suffix  <- "GS"
gs_file <- "./../csv/GasSampling/GasSampling_all.csv"
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
  merged.df$PC4*(merged.df$Analyser_position == 4) +
  merged.df$PC1*(merged.df$Analyser_position == "Ext")


merged.df$PCExt <- merged.df$PC # TO BE CORRECTED FOR H20

merged.df$Temp <-
  merged.df$Temp1*(merged.df$Analyser_position == 1) +
  merged.df$Temp2*(merged.df$Analyser_position == 2) +
  merged.df$Temp3*(merged.df$Analyser_position == 3) +
  merged.df$Temp4*(merged.df$Analyser_position == 4) +
  merged.df$TempExt*(merged.df$Analyser_position == "Ext")

merged.df$CH4em <- 24 * computeEmission(cc = merged.df$CH4, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolCH4)
merged.df$CO2em <- 24 * computeEmission(cc = merged.df$CO2, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolCO2)
merged.df$N2Oem <- 24 * computeEmission(cc = merged.df$N2O, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolN2O)
merged.df$NH3em <- 24 * computeEmission(cc = merged.df$NH3, pc = merged.df$PC, t = merged.df$Temp, mmol = mmolNH3)

merged.df <- merged.df[!(is.na(merged.df$CH4em) | is.na(merged.df$Status)),]

merged.df

## Removing first entries for each position and entries after each calibration

merged.df$transition <- !(lag(merged.df$Analyser_position) == merged.df$Analyser_position)
merged.df$transition_time <- ifelse(merged.df$transition, merged.df$CVPtime5sec, NA)
merged.df$transition_time <- na.locf(merged.df$transition_time, na.rm = FALSE)
merged.df$time_to_transition <- as.difftime(as.numeric(merged.df$CVPtime5sec-merged.df$transition_time), units="secs")

merged.df$statusChange <- !(lag(merged.df$Status) == merged.df$Status)
merged.df$statusChange_time <- ifelse(merged.df$statusChange, merged.df$CVPtime5sec, NA)
merged.df$statusChange_time <- na.locf(merged.df$statusChange_time, na.rm = FALSE)
merged.df$time_to_statusChange <- as.difftime(as.numeric(merged.df$CVPtime5sec-merged.df$statusChange_time), units="secs")

write.table(merged.df, file = "temp.csv", sep=";")


selected_data.df <- merged.df[merged.df$time_to_transition > 240 & merged.df$Status == "Connected" & merged.df$time_to_statusChange > 180,]




# write.table(selected_data.df, file = "temp.csv", sep=";")


ggplot(selected_data.df, aes(x=AdjustedTime, y=CO2em, col=as.character(Analyser_position))) + geom_point()
ggsave("./plots/CO2em.pdf")
ggplot(selected_data.df, aes(x=AdjustedTime, y=CH4em, col=as.character(Analyser_position))) + geom_point()
ggsave("./plots/CH4em.pdf")


# ggplot(merged.df[hour(merged.df$CVPtime5sec)>7 & hour(merged.df$CVPtime5sec)<10,], aes(x=AdjustedTime, y=CH4em, col=as.character(Analyser_position))) + geom_point()

test <- selected_data.df[selected_data.df$CO2em < 300,]
print(test)
