# Acquisision donn?es CVP
#
# Reset environment
#
rm(list = ls())         # Remove environemnent variables
graphics.off()          # Close any open graphics

setwd("C:/Users/m.mathot.admin/Desktop/ADAQ/R")
prefix <- "./../csv/CVPBarn/CVP"
Site<-c("Barn")
num<-as.numeric(format(Sys.time(),"%Y%m%d%H%M"))[1]# Nom du ficher: un chiffre sous le format anneemoisjourheureminute , deux caracteres a chaque fois. Example pour 9 : 09

RawFile<-paste(prefix,"_",Site,"_",num,".txt",sep=c(""))#Direct reconding file 
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

con <- serialConnection(name = "test_con",
                        port = "COM255",
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

# layout(matrix(c(1:10),5,2))
layout(matrix(c(1:2),2,1))


Header<-c("State,Info,Temp1,PC1,Temp2,PC2,Temp3,PC3,Temp4,PC4,TempExt,PC5,Datetime")

cat(Header,file=RawFile,sep=c("\n"))

if (!file.exists(CSVFile)){
  Header.df <- read.table(text=Header, sep=",")
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
        ToSave<-gsub(">",paste0(",",Time),ToSave)
      }else{
        ToSave<-gsub(">",paste0(",",NA),ToSave)
      }
      print(paste("last recording CVP and temprature barn",Sys.time()))
      print(ToSave)
      cat(ToSave,file=RawFile,append=TRUE,sep=c("\n"))
      write.table(read.csv(text = ToSave, sep=",", stringsAsFactors = FALSE, header = FALSE), file = CSVFile, sep = ";", dec = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      ToSave<-""
    }else{
      ToSave<-newText
    }
    
    # if(Sys.time()>(SavePlotTime+SaveFreq)){
    #   
    #   #Table<-read.table(RawFile,header=TRUE,dec=".",sep=",",stringsAsFactors = FALSE)
    #   #write.table(x=Table,file=CSVFile,sep=c(";"),dec=(","),row.names =F)
    #   #print("plot Data")
    #   
    # 
    #   
    #   print(paste("last recording CVP and temprature barn",Sys.time()))
    #   
    #   DataToPlot<-Table
    #   DataToPlot$Time<-as_datetime(ymd_hms(DataToPlot$Datetime,tz=Sys.timezone()),tz=Sys.timezone())
    #   
    #   
    #   DataToPlot$Time[is.na(DataToPlot$Time)]<-Sys.time()
    #   
    #   PlotTimeRange<-c(Sys.time()-3600*4,Sys.time())
    # 
    #   DataToPlot<-DataToPlot[DataToPlot$Time>PlotTimeRange[1]&DataToPlot$Time<=PlotTimeRange[2],]
    #   
    #   # plot(x=as_datetime(ymd_hms(Table$Datetime)),y=Table$Temp1,pch=21,ylim=c(min(Table[,grepl("Temp",colnames(Table))]),max(Table[,grepl("Temp",colnames(Table))])))
    #   # points(x=as_datetime(ymd_hms(Table$Datetime)),y=Table$Temp2,pch=21,bg="red")
    #   # points(x=as_datetime(ymd_hms(Table$Datetime)),y=Table$Temp3,pch=21,bg="green")
    #   # points(x=as_datetime(ymd_hms(Table$Datetime)),y=Table$Temp4,pch=21,bg="blue")
    #   # points(x=as_datetime(ymd_hms(Table$Datetime)),y=Table$TempExt,pch=21,bg="grey")
    #   # 
    #   plot(x=DataToPlot$Time,y=DataToPlot$Temp1,pch=21,xlab=c("Temps"),ylab=("Temperature"),ylim=c(-40,+50),type=c("p"))
    #   points(x=DataToPlot$Time,y=DataToPlot$Temp2,pch=21,bg="red")
    #   points(x=DataToPlot$Time,y=DataToPlot$Temp3,pch=21,bg="green")
    #   points(x=DataToPlot$Time,y=DataToPlot$Temp4,pch=21,bg="blue")
    #   points(x=DataToPlot$Time,y=DataToPlot$TempExt,pch=21,bg="grey")
    #   
    #   plot(x=DataToPlot$Time,y=DataToPlot$PC1,pch=21,xlab=c("Temps"),ylab=("% ventilation"),ylim=c(0,120),type=c("p"))
    #   points(x=DataToPlot$Time,y=DataToPlot$PC2,pch=21,bg="red")
    #   points(x=DataToPlot$Time,y=DataToPlot$PC3,pch=21,bg="green")
    #   points(x=DataToPlot$Time,y=DataToPlot$PC4,pch=21,bg="blue")
    #   #points(x=DataToPlot$Time,y=DataToPlot$PC5,pch=21,bg="grey")
    #   
    #   
    #   
    #   # 
    # 
    #   SavePlotTime<-Sys.time()
    # }
    # 
  }
}
close(con)

