# Acquisision donn?es systeme mesure NH3 Etable, pression et temperature
#
# Reset environment
#
rm(list = ls())         # Remove environemnent variables
graphics.off()          # Close any open graphics

#
setwd("C:/Users/m.mathot.admin/Desktop/ADAQ/R")
prefix <- "./../csv/SamplerBarn/GasSampling"


num<-as.numeric(format(Sys.time(),"%Y%m%d%H%M"))[1]# Nom du ficher: un chiffre sous le format anneemoisjourheureminute , deux caracteres a chaque fois. Example pour 9 : 09

RawFile<-paste(prefix,"_",num,".txt",sep=c("")) #Direct reconding file 
CSVFile<-paste(prefix,"_all.csv",sep=c("")) #Save all Data

# Libraries
#
library(serial)
library(stringr)
library(ggplot2)
library(chron)
library(lubridate)
#
# Script
#acquisition
if(exists(c("con"))){close(con)}
con <- serialConnection(name = "test_con",
                        port = "COM256",
                        mode = "9800,n,8,1",
                        buffering = "none",
                        newline = 1,
                        translation = "cr")

open(con)
Sys.timezone(location = TRUE)
starTime<-Sys.time()
stopTime <- starTime + 60*60*24*365  # en secondes
FirstLine<-TRUE
ToSave<-""
Read=TRUE
SaveFreq<-3*4 #Saving Data to CSV frenquency in seconds
SavePlotTime<-Sys.time()
##write.serialConnection(con,dat)

layout(matrix(c(1:2),2,1))


Header<-"Site,Washing_position,Analyser_position,Washing_line,Analyser_line,Mode,DP_washing,DP_analyser,Temper_washing,Temper_analyser,Analog_population,Sampling_duration,Warning,Time"
cat(Header,file=RawFile,sep=c("\n"))

if (!file.exists(CSVFile)){
  Header.df <- read.table(text=Header, sep=",")
  write.table(Header.df, file = CSVFile, sep = ";", dec = ",", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

while(Sys.time() < stopTime){
  
  if(Read){newText <- read.serialConnection(con)}
  if(0 < nchar(newText)){
    print(newText)
    if(substr(newText,1,1)==" "){newText<-substr(newText,2,nchar(newText))}
    Time<-Sys.time()
    
    if((substr(newText,nchar(newText),nchar(newText))==">")&(substr(newText,1,1)=="<"||(substr(newText,1,2)=="\n<"))){
      if(substr(newText,1,1)=="\n"){ToSave<-c(ToSave,substr(newText,2,nchar(newText)))}else{ToSave<-c(ToSave,newText)}
      
      ToSave<-gsub("<","",ToSave)
      
      if(dim(as.data.frame(tail(str_locate_all(newText,">"))))[1]==1){
        ToSave<-gsub(">",paste0(",",Time),ToSave)
      }else{
        ToSave<-gsub(">",paste0(",",NA),ToSave)
      }
      print(paste("last recording Sampler",Sys.time()))
      print(ToSave)
      cat(ToSave,file=RawFile,append=TRUE,sep=c("\n"))
      print(read.csv(text = ToSave, sep=",", stringsAsFactors = FALSE, header = FALSE))
      write.table(read.csv(text = ToSave, sep=",", stringsAsFactors = FALSE, header = FALSE), file = CSVFile, sep = ";", dec = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      ToSave<-""
    }else{
      ToSave<-newText
    }
    # if(Sys.time()>(SavePlotTime+SaveFreq)){
    #   
    #   Table<-read.table(RawFile,header=TRUE,dec=".",sep=",")
    #   write.table(x=Table,file=CSVFile,sep=c(";"),dec=(","),row.names =F)
    #   #print("plot Data")
    #   #plot(x=as_datetime(ymd_hms(Table$Time)),y=Table$DP_analyser,bg=as.factor(Table$Analyser_position),col=as.factor(Table$Analyser_position),pch=21)
    #   #plot(x=as_datetime(ymd_hms(Table$Time)),y=Table$DP_washing,bg=as.factor(Table$Washing_position),col=as.factor(Table$Washing_position),pch=21)
    #   
    #   print(paste("last recording Sampler",Sys.time()))
    #   
    #   DataToPlot<-Table
    #   DataToPlot$Time<-as_datetime(ymd_hms(DataToPlot$Time,tz=Sys.timezone()),tz=Sys.timezone())
    #   PlotTimeRange<-c(max(DataToPlot$Time,na.rm=TRUE)-3600*4,max(DataToPlot$Time,na.rm=TRUE))
    #   
    #   DataToPlot<-DataToPlot[DataToPlot$Time>PlotTimeRange[1]&DataToPlot$Time<=PlotTimeRange[2],]
    #   
    #   plot(x=DataToPlot$Time,y=DataToPlot$DP_analyser,bg=as.factor(DataToPlot$Analyser_position),col=as.factor(DataToPlot$Analyser_position),pch=21)
    #   plot(x=DataToPlot$Time,y=DataToPlot$DP_washing,bg=as.factor(DataToPlot$Washing_position),col=as.factor(DataToPlot$Washing_position),pch=21)
    #   
    #   
    #   
    #   #ggplot(data=Table,aes(x=as_datetime(ymd_hms(Time,tz=Sys.timezone(),tz=Sys.timezone()),y=DP_analyser,color=Analyser_position)))+
    #     #geom_point()
    #   SavePlotTime<-Sys.time()
    # }
  }
}
close(con)

