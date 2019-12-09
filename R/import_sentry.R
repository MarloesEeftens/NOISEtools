#################################
##### import_sentry         #####
##### By: Marloes Eeftens   #####
##### Last edit: 18/12/2017 #####
#################################

#Function import_pulsar:
import_sentry=function(filename,timeformat,prefix=""){

  #0) Set defaults & parameters:
  #Set the time format, or use defaults:
  separators<-c("\t",",",";")
  timeformats<-c("%Y/%m/%d %H:%M:%S","%m/%d/%Y %H:%M:%S","%Y-%m-%d %H:%M:%S","%d.%m.%Y %H:%M:%S","%Y/%m/%d %H:%M:%S","%d/%m/%Y %H:%M:%S","%d.%m.%Y %H:%M:%S")
  ifelse(missing(timeformat),possible_time_formats<-timeformats,possible_time_formats<-timeformat)
  new_names<-c("device_id","LEQ","LMin","LMax")

  #1) Read the file:
  dat<-read.csv(file=filename,header=TRUE,skip=1,blank.lines.skip=TRUE,sep=separators[1])
  dat$device_id<-gsub("_.*","",read.csv(file=filename,nrows=1,header=FALSE)[1,1])
  if(dim(dat)[2]<3){
    dat<-read.csv(file=filename,header=TRUE,skip=1,blank.lines.skip=TRUE,sep=separators[2])
    dat$device_id<-gsub("_.*","",read.csv(file=filename,nrows=1,header=FALSE)[1,1])
  }
  if(dim(dat)[2]<3){
    dat<-read.csv(file=filename,header=TRUE,skip=1,blank.lines.skip=TRUE,sep=separators[3])
    dat$device_id<-gsub("_.*","",read.csv(file=filename,nrows=1,header=FALSE)[1,1])
  }

  #2) Format timestamp:
  known_time_format<-FALSE
  i<-1
  while (known_time_format==FALSE&i<=length(timeformats)){
    dat$PosixTime<-as.POSIXct(substr(dat$Time..Date.hh.mm.ss.ms.,1,19),format=timeformats[[i]])
    if(all(is.na(dat$PosixTime))==TRUE){i<-i+1}
    if(all(is.na(dat$PosixTime))==FALSE){
      known_time_format<-TRUE
      print(paste0("Time format is: ",timeformats[[i]]))
    }
  }
  if(known_time_format==FALSE){stop(cat("The correct timeformat has not been found. Specify by setting e.g. timeformat=",'"',"%d-%m-%Y %H-%M-%S",'"',".",sep=""))}

  #3) Eliminate uninformative variables
  if("L.Max.dB..A" %in% names(dat)){dat$LMax<-dat$L.Max.dB..A}
  if("LEQ.dB..A" %in% names(dat)){dat$LEQ<-dat$LEQ.dB..A}
  if("L.Min.dB..A" %in% names(dat)){dat$LMin<-dat$L.Min.dB..A}
  dat<-dat[,match(c("PosixTime",new_names),names(dat))[!is.na(match(c("PosixTime",new_names),names(dat)))]]

  #4) Change names to include prefix:
  if(!prefix==""){names(dat)[names(dat) %in% new_names]<-paste0(prefix,new_names)}

  #5) Return the resulting R dataframe:
  return(dat)
}
