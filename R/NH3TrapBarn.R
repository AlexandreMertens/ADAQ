# Acquisision donn?es CVP
#
# Reset environment
#
rm(list = ls())         # Remove environemnent variables
graphics.off()          # Close any open graphics


setwd("C:/Users/m.mathot.admin/Desktop/ADAQ/R")
prefix <- "./../csv/NH3TrapBarn/NH3Trap"

num<-as.numeric(format(Sys.time(),"%Y%m%d%H%M"))[1]# Nom du ficher: un chiffre sous le format anneemoisjourheureminute , deux caracteres a chaque fois. Example pour 9 : 09

Site<-c("Barn")
RawFile<-paste(prefix,"_",Site,"_",num,".txt",sep=c(""))#Direct recoding file 
CSVFile<-paste(prefix,"_",Site,"_all.csv",sep=c(""))#Daily Save of Data

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
                        port = "COM254",
                        mode = "9800,n,8,1",
                        buffering = "none",
                        newline = 1,
                        translation = "cr")

open(con)
Sys.timezone(location = TRUE)
starTime<-Sys.time()
stopTime <- starTime + 60*60*24*365 # en secondes
FirstLine<-TRUE
ToSave<-""
Read=TRUE
SaveFreq<-3*4 #Saving Data to CSV frenquency in seconds
SavePlotTime<-Sys.time()
##write.serialConnection(con,dat)

# layout(matrix(c(1:10),5,2))

#Header<-c("V1;V2;V3;V4;V4;V6;V7;V7;V9;V10;V11;V12;V13;V14;V15;V16;V17;V18;V19;V20;V21;V22;V23;V24;V25;V26;V27;V28;V29;V30;V31;V32;V33;V34;V35;Time")
Header<-c("TimeCounter;DP_T;DP1;DP2;DP3;DP4;P_A;T_T;T1;T2;T3;T4;T_A;NumberMeas;Datetime")

cat(Header,file=RawFile,sep=c("\n"))

if (!file.exists(CSVFile)){
  Header.df <- read.table(text=Header, sep=";")
  write.table(Header.df, file = CSVFile, sep = ";", dec = ",", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

while(Sys.time() < stopTime){
  
  if(Read){newText <- read.serialConnection(con)}
  if(0 < nchar(newText)){
    #print(newText)
    if(substr(newText,1,1)==" "){newText<-substr(newText,2,nchar(newText))}
    Time<-Sys.time()
    
    if((substr(newText,nchar(newText),nchar(newText))==">")&(substr(newText,1,1)=="<"||(substr(newText,1,2)=="\n<"))){
      if(substr(newText,1,1)=="\n"){ToSave<-c(ToSave,substr(newText,2,nchar(newText)))}else{ToSave<-c(ToSave,newText)}
      
      ToSave<-gsub("<","",ToSave)
      
      if(dim(as.data.frame(tail(str_locate_all(newText,">"))))[1]==1){
        ToSave<-gsub(">",paste0(";",Time),ToSave)
      }else{
        ToSave<-gsub(">",paste0(";",NA),ToSave)
      }
      
      print(paste("last recording NH3 Trap at barn: ",Sys.time()))
      print(ToSave)
      cat(ToSave,file=RawFile,append=TRUE,sep=c("\n"))
      write.table(read.csv(text = ToSave, sep=";", stringsAsFactors = FALSE, header = FALSE), file = CSVFile, sep = ";", dec = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      
      ToSave<-""
    }else{
      ToSave<-newText
    }
    # if(Sys.time()>(SavePlotTime+SaveFreq)){
    #   
    #   Table<-read.table(RawFile,header=TRUE,dec=",",sep=";",stringsAsFactors = FALSE)
    #   write.table(x=Table,file=CSVFile,sep=c(";"),dec=(","),row.names =F)
    #   print(paste("last recording NH3 TRAP",Sys.time()))
    #   
    #   DataToPlot<-Table
    #   DataToPlot$Time<-as_datetime(ymd_hms(DataToPlot$Time,tz=Sys.timezone()),tz=Sys.timezone())
    #   PlotTimeRange<-c(max(DataToPlot$Time,na.rm=TRUE)-3600*4,max(DataToPlot$Time,na.rm=TRUE))
    #    
    #   DataToPlot<-DataToPlot[DataToPlot$Time>PlotTimeRange[1]&DataToPlot$Time<=PlotTimeRange[2],]
    #   
    #   plot(x=DataToPlot$Time,y=DataToPlot$V12,pch=21)
    #    
    #   SavePlotTime<-Sys.time()
    #   
    # }
  }
}
close(con)
