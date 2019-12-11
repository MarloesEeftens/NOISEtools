#################################
##### import_sentry         #####
##### By: Marloes Eeftens   #####
##### Last edit: 10/12/2019 #####
#################################

#Function import_pulsar:
import_sentry=function(filename,timeformat,prefix=""){

  #0) Set defaults & parameters:
  #Set the time format, or use defaults:
  separators<-c("\t",",",";")
  timeformats<-c("%Y/%m/%d %H:%M:%S","%m/%d/%Y %H:%M:%S","%Y-%m-%d %H:%M:%S","%d.%m.%Y %H:%M:%S","%Y/%m/%d %H:%M:%S","%d/%m/%Y %H:%M:%S","%d.%m.%Y %H:%M:%S")
  ifelse(missing(timeformat),possible_time_formats<-timeformats,possible_time_formats<-timeformat)
  new_names<-c("device_id","LEQ","LMin","LMax")

  #1) Determine file extension
  if(substr(filename,nchar(filename)-3,nchar(filename)) %in% c("xlsx",".xls")){
    filetype="xlsx"}else if(substr(filename,nchar(filename)-3,nchar(filename)) %in% c(".csv",".txt")){
      filetype="csv"}else{stop("Unknown file extension")}

  #2) Read the file:
  if(filetype=="xlsx"){
    dat<-read_excel(path=filename,col_names=TRUE,sheet=1,skip=2)
    dat$device_id<-gsub("_.*","",read_excel(path=filename,n_max=1,col_names=FALSE)[1,1])
  }
  if(filetype=="csv"){
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
  }

  #3) Format timestamp:
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

  #4) Eliminate uninformative variables
  if("L.Max.dB..A" %in% names(dat)){dat$LMax<-dat$L.Max.dB..A}
  if("LEQ.dB..A" %in% names(dat)){dat$LEQ<-dat$LEQ.dB..A}
  if("L.Min.dB..A" %in% names(dat)){dat$LMin<-dat$L.Min.dB..A}
  dat<-dat[,match(c("PosixTime",new_names),names(dat))[!is.na(match(c("PosixTime",new_names),names(dat)))]]

  #5) Change names to include prefix:
  if(!prefix==""){names(dat)[names(dat) %in% new_names]<-paste0(prefix,new_names)}

  #6) Return the resulting R dataframe:
  return(dat)
}
