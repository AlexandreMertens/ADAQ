rm(list=ls())

#+++++++++++++++++++++++++++++++++++++++++++
##-------------------------Libraries Opening
#+++++++++++++++++++++++++++++++++++++++++++

library(XML)
library(lubridate)
library(serial)
library(ggplot2)
library(chron)


#+++++++++++++++++++++++++++++++++++++++++++
##-------------------------Gobal parameters
#+++++++++++++++++++++++++++++++++++++++++++

TimeZone<-Sys.timezone() # PC Time zone
ADCTimeZone<-c("UTC") # ADC Time zone

#++++++++++++++++++++++++++++++++++++++++++++++++++
##-------------------------Communication parameters
#++++++++++++++++++++++++++++++++++++++++++++++++++

URLDataADC<-"http://10.15.109.220/logs/Log1/" # URL for etherne communiation with ADC for Data
URLInfoADC<-"http://10.15.109.220/logs/Log2/" # URL for etherne communiation with ADC for Information

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##-------------------------Data recording parameters and initialisation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #++++
  # ADC
  #++++

  RecordFrequence<-10#Seconds
  EndTime<-Sys.time()+60*60*24*365
  RecordingFenq<-5 # en secondes
  LastSAnalysisTime<-ymd(Sys.Date(),tz=TimeZone)
  NewFile=TRUE
  LastAnalysisDate<-ymd(Sys.Date(),tz=TimeZone)
  PreviousAnalysisDate<-ymd(Sys.Date(),tz=TimeZone)-3600*24
  NameSaveDataADC<-""
  Prefix<-"./../csv/ADCBarn/ADCGasData"

layout(matrix(c(1:5,5),2,3))

#++++++++++++++++++++++++++++++++++++++++
##-------------------------Data recording
#++++++++++++++++++++++++++++++++++++++++

while(Sys.time()<EndTime){
  
  #++++
  # ADC
  #++++
  
  RecordingTime<-Sys.time()
  print("Recording Data")
  
  doc<-htmlParse(URLDataADC)
  RecordedFiles<-xpathSApply(doc,"//a/@href")
  free(doc)
  RecordedFiles<-RecordedFiles[grepl(".CSV",RecordedFiles)]
  RecordedFiles<-RecordedFiles[order(as.numeric(gsub(".CSV","",RecordedFiles)),decreasing=TRUE)]
  
  
  if(NameSaveDataADC==paste0(Prefix,"_",RecordedFiles[1])){
    NewFile=FALSE
  }else{NewFile=TRUE}
  
  NameSaveDataADC<-paste0(Prefix,"_",RecordedFiles[1])
  
  TimeRecord<-Sys.time()
  DownloadedData1<-try(read.table(paste("http://10.15.109.220/logs/Log1/",RecordedFiles[1],sep=""),sep=c(","),dec=c("."),header=TRUE,stringsAsFactors = FALSE))
  #DownloadedData2<-try(read.table(paste("http://10.15.109.220/logs/Log2/",RecordedFiles[1],sep=""),sep=c(","),dec=c("."),header=TRUE,stringsAsFactors = FALSE, fill = TRUE))
  
  
  
  while(class(DownloadedData1)=="try-error"){
    print("unable to open source data")
    DownloadedData1<-try(read.table(paste("http://10.15.109.220/logs/Log1/",RecordedFiles[1],sep=""),sep=c(","),dec=c("."),header=TRUE,stringsAsFactors = FALSE))
    #DownloadedData2<-try(read.table(paste("http://10.15.109.220/logs/Log2/",RecordedFiles[1],sep=""),sep=c(","),dec=c("."),header=TRUE,stringsAsFactors = FALSE, fill = TRUE))
    
  }  
  
  DownloadedData1$AdjustedTime<-with_tz(as_datetime(ymd_hms(paste(DownloadedData1$Date,DownloadedData1$Time),tz=ADCTimeZone),tz=ADCTimeZone),tzone=TimeZone)
  #DownloadedData2$AdjustedTime<-with_tz(as_datetime(ymd_hms(paste(DownloadedData2$Date,DownloadedData2$Time),tz=ADCTimeZone),tz=ADCTimeZone),tzone=TimeZone)
  
  #DownloadedData <- merge(DownloadedData1, DownloadedData2[,!colnames(DownloadedData2) %in% c("Time","Date", "Type")], by = "AdjustedTime") 
  DownloadedData <- DownloadedData1
  
  DownloadedData$H2O<-as.numeric(gsub(",","",DownloadedData$H2O))
  colnames(DownloadedData)[colnames(DownloadedData)%in%c("Gas1","Gas2","Gas3","Gas4")]<-c("CH4","N2O","CO2","NH3")
  DownloadedData$AdjustedTime<-with_tz(as_datetime(ymd_hms(paste(DownloadedData$Date,DownloadedData$Time),tz=ADCTimeZone),tz=ADCTimeZone),tzone=TimeZone)
  DownloadedDataToRecord<-DownloadedData[DownloadedData$AdjustedTime>LastSAnalysisTime,]


  if(nrow(DownloadedDataToRecord)>0){
    TimeDifference<-as.numeric(difftime(TimeRecord,DownloadedDataToRecord$AdjustedTime[length(DownloadedDataToRecord$AdjustedTime)],units=c("secs")))# the minimum of these value (even if negative) gives the time delay between Computer time and ADC Time with en delay of about 3 seconds 
    DownloadedDataToRecord$TimeDifference<-TimeDifference
    DownloadedDataToRecord$PCTime<-as.character(TimeRecord)
    DownloadedDataToRecord$AdjustedTime<-as.character(DownloadedDataToRecord$AdjustedTime)
      
    if(!NewFile){
      SavedData<-read.table(file=NameSaveDataADC,sep=c(";"),dec=c(","),header=TRUE)
      SavedData<-rbind(SavedData,DownloadedDataToRecord)
      write.table(SavedData,file=NameSaveDataADC,sep=c(";"),dec=c(","),row.names = FALSE)
    }else{
      write.table(DownloadedDataToRecord,file=paste0(Prefix,"_",RecordedFiles[1]),sep=c(";"),dec=c(","),row.names = FALSE)
    }
    
    LineSize<-4
    TextSize<-7
    TextMulti<-1
    Decallage<-3*60*60
    Ambiant<-440
    Vache<-Ambiant+260
    
    DataToPlot<-DownloadedData[DownloadedData$AdjustedTime>(Sys.time()-Decallage*1),]

    plot(x=DataToPlot$AdjustedTime,y=DataToPlot$CO2,xlab=c("Time"),ylab=c("CO2"))
    plot(x=DataToPlot$AdjustedTime,y=DataToPlot$CH4,xlab=c("Time"),ylab=c("CH4"))
    plot(x=DataToPlot$AdjustedTime,y=DataToPlot$NH3,xlab=c("Time"),ylab=c("NH3"))
    plot(x=DataToPlot$AdjustedTime,y=DataToPlot$N2O,xlab=c("Time"),ylab=c("N2O"))
    plot(x=DataToPlot$AdjustedTime,y=DataToPlot$H2O,xlab=c("Time"),ylab=c("H2O"))
    
    print(Sys.time()-RecordingTime)
    LastSAnalysisTime<-DownloadedData$AdjustedTime[length(DownloadedData$AdjustedTime)]
    print(max(DownloadedData$AdjustedTime))
    print("delay  of ")
    print(TimeDifference)
    print("Number of new data")
    print(dim(DownloadedDataToRecord)[1])
  }else{print("No new Data")}

  RecordingTime<-Sys.time()+RecordingFenq
  print("Waiting for next recording in (seconds) :")
  print(RecordingTime-Sys.time())
  
  print("Waiting for plot :")
  
  WaitingDelayforPlot<-Sys.time()+30
  
  while(Sys.time()<WaitingDelayforPlot){ }
  print("PlotSouldBe finished")

}
