#################################
##### import_sentry         #####
##### By: Marloes Eeftens   #####
##### Last edit: 18/12/2017 #####
#################################

#Function import_pulsar:
import_sentry=function(filename,timeformat,prefix){

  #0) Set defaults & parameters:
  #Set the time format, or use defaults:
  separators<-c("\t",",",";")
  timeformats<-c("%Y/%m/%d %H:%M:%S","%m/%d/%Y %H:%M:%S","%Y-%m-%d %H:%M:%S","%d.%m.%Y %H:%M:%S","%Y/%m/%d %H:%M:%S","%d/%m/%Y %H:%M:%S","%d.%m.%Y %H:%M:%S")
  ifelse(missing(timeformat),possible_time_formats<-timeformats,possible_time_formats<-timeformat)
  if(missing(prefix)){prefix<-""}
  new_names<-c("id","LMax","LEQ","LMin")

  #1) Read the file:
  try(dat<-read.csv(file=filename,header=TRUE,skip=1,blank.lines.skip=TRUE,sep=separators[1]),silent=TRUE)
  try(dat$id<-as.numeric(gsub("nsentry","",gsub("_.*","",read.csv(file=filename,nrows=1,header=FALSE)[1,1]))))
  if(dim(dat)[2]<3){
    try(dat<-read.csv(file=filename,header=TRUE,skip=1,blank.lines.skip=TRUE,sep=separators[2]),silent=TRUE)
    try(dat$id<-as.numeric(gsub("nsentry","",gsub("_.*","",read.csv(file=filename,nrows=1,header=FALSE)[1,1]))))
  }
  if(dim(dat)[2]<3){
    try(dat<-read.csv(file=filename,header=TRUE,skip=1,blank.lines.skip=TRUE,sep=separators[3]),silent=TRUE)
    try(dat$id<-as.numeric(gsub("nsentry","",gsub("_.*","",read.csv(file=filename,nrows=1,header=FALSE)[1,1]))))
  }

  #2) Format timestamp:
  known_time_format<-FALSE
  i<-1
  while (known_time_format==FALSE&i<=length(possible_time_formats)){
    dat$PosixTime<-as.POSIXct(substr(dat$Time..Date.hh.mm.ss.ms.,1,19),format=possible_time_formats[[i]])
    if(all(is.na(dat$PosixTime))==TRUE){i<-i+1}
    if(all(is.na(dat$PosixTime))==FALSE){
      known_time_format<-TRUE
      print(paste0("Time format is: ",possible_time_formats[[i]]))
    }
  }
  if(known_time_format==FALSE){stop(cat("The correct timeformat has not been found. Specify by setting e.g. timeformat=",'"',"%d-%m-%Y %H-%M-%S",'"',".",sep=""))}

  #3) Eliminate uninformative variables
  dat$LMax<-dat$L.Max.dB..A
  dat$LEQ<-dat$LEQ.dB..A
  dat$LMin<-dat$L.Min.dB..A
  dat<-subset(dat,select=c(PosixTime,id,LMax,LEQ,LMin))

  #4) Change names to include prefix:
  if(!prefix==""){names(dat)[names(dat) %in% new_names]<-paste0(prefix,new_names)}

  #5) Return the resulting R dataframe:
  return(dat)
}
