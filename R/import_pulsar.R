#################################
##### import_pulsar         #####
##### By: Marloes Eeftens   #####
##### Last edit: 18/12/2017 #####
#################################

#Function import_pulsar:
import_pulsar=function(filename,timeformat,prefix){

  #0) Set defaults & parameters:
  #Set the time format, or use defaults:
  timeformats<-c("%Y/%m/%d %H:%M:%S","%m/%d/%Y %H:%M:%S","%Y-%m-%d %H:%M:%S","%d.%m.%Y %H:%M:%S","%Y/%m/%d %H:%M:%S","%d/%m/%Y %H:%M:%S","%d.%m.%Y %H:%M:%S")
  ifelse(missing(timeformat),possible_time_formats<-timeformats,possible_time_formats<-timeformat)
  if(missing(prefix)){prefix<-""}
  new_names<-c("id","LAT","LCT","LZT")

  #1) Read the file:
  firstline<-head(read.csv(file=filename,header=FALSE,skip=2,sep="\t"),1)
  device_id<-paste(firstline[1,1],firstline[1,2],firstline[1,3],sep=" ")
  dat<-read.csv(file=filename,header=TRUE,skip=5,sep="\t")
  dat$id<-device_id

  #2) Format timestamp:
  known_time_format<-FALSE
  i<-1
  while (known_time_format==FALSE&i<=length(possible_time_formats)){
    dat$PosixTime<-as.POSIXct(dat$Date_Hour,format=possible_time_formats[[i]])
    if(all(is.na(dat$PosixTime))==TRUE){i<-i+1}
    if(all(is.na(dat$PosixTime))==FALSE){
      known_time_format<-TRUE
      print(paste0("Time format is: ",possible_time_formats[[i]]))
    }
  }
  if(known_time_format==FALSE){stop(cat("The correct timeformat has not been found. Specify by setting e.g. timeformat=",'"',"%d-%m-%Y %H-%M-%S",'"',".",sep=""))}

  #3) Eliminate uninformative variables
  dat<-subset(dat,select=c(PosixTime,id,LAT,LCT,LZT))

  #4) Change names to include prefix:
  if(!prefix==""){names(dat)[names(dat) %in% new_names]<-paste0(prefix,new_names)}

  #5) Return the resulting R dataframe:
  return(dat)
}
