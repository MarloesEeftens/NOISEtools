#################################
##### im_ratio_dB           #####
##### By: Marloes Eeftens   #####
##### Last edit: 08/01/2018 #####
#################################

#Function im_ratio_dB:
im_ratio_dB=function(x,L,C,timevar,plot_filename){

  #0) Set defaults & parameters:
  if(missing(x)){stop("Please specify a dataframe or numeric vector for x...")}
  if(missing(C)){C<-3} #C is 3dB by default following paper by Wunderli et al. (2016)
  is.POSIXct<-function(x)inherits(x,"POSIXct")
  if(is.data.frame(x)){
    if(missing(timevar)&sum(sapply(x,is.POSIXct))==1){
      timevar<-names(x)[sapply(x,is.POSIXct)]
      message(paste0("No timevar was specified. Variable ",timevar," taken as default."))
    }
  }
  if(is.data.frame(x)&missing(L)){stop("Please specify which variable contains the sound level L.")}
  # Check that the specified plot_filename actually exists
  if(missing(plot_filename)==FALSE&dir.exists(plot_filename)==FALSE){message("Specified plot_filename does not exist. Time series plot will not be generated.")}
  # You need the suggested package for this function
  if(missing(plot_filename)==FALSE&!requireNamespace("ggplot2",quietly=TRUE)){
    stop("package ggplot2 is needed for this function to work. Please install it.")}

  #1) Average variables indicated for averaging:
  #Remove any missing values if present
  if(is.data.frame(x)){
    dat<-subset(x,select=c(timevar,L))
    dat$x_new<-dat[,2]
    dat<-dat[!is.na(dat$x_new),]
  }
  if(is.vector(x)){dat<-data.frame(x_new=x[!is.na(x)])}
  #Calculate a logarithmic mean of the sounds levels (or "LEQ_T_tot" in the paper by Wunderli)
  LEQ_T_tot<-10*log10((1/length(dat$x_new))*sum(10^(0.1*dat$x_new)))

  #2) Calculate the threshold above which a measurement should be classed as an "event"
  K<-LEQ_T_tot+C

  #3) Compute the exceedances, eventfullness and LEQ_T_events with the Heaviside function
  dat$exc<-(sign(dat$x_new-K)+1)/2
  dat$exc_min1<-c(0,dat$exc[1:length(dat$exc)-1])
  dat$exc_1st<-ifelse(dat$exc>0&dat$exc_min1==0,dat$exc,0)
  dat$exc_last<-ifelse(dat$exc==0&dat$exc_min1>0,dat$exc_min1,0)
  N_events<-sum(dat$exc_1st)
  LEQ_T_events<-10*log10((1/length(dat$x_new))*sum(dat$exc*10^(0.1*dat$x_new)))

  #4) Calculate the intermittancy ratio
  IM_ratio<-10^(0.1*(LEQ_T_events-LEQ_T_tot))*100

  #5) If plots are desired, plot the time series:
  if(missing(plot_filename)==FALSE){
    if(is.vector(x)){dat$PosixTime<-seq(1:dim(dat)[1])}
    interval<-dat$PosixTime[2]-dat$PosixTime[1]
    xmin<-dat$PosixTime[dat$exc_1st!=0]
    xmax<-dat$PosixTime[dat$exc_last!=0]
    if(length(xmax)<length(xmin)){xmax<-c(xmax,tail(dat$PosixTime,1))}
    p1<-ggplot(data=dat,aes(x=PosixTime,y=x_new))+
      annotate("rect",xmin=xmin,xmax=xmax,ymin=min(dat$x_new)/1.1,ymax=max(dat$x_new)*1.1,fill="grey")+
      geom_line(color="black")+
      geom_hline(yintercept=LEQ_T_tot,color="green")+
      geom_hline(yintercept=K,color="red")+
      #theme(title=text_b,axis.title=text_s,axis.text=text_s,legend.text=text_s,legend.title=element_blank(),legend.position="bottom")+
      ylim(min(dat$x_new)/1.1,max(dat$x_new)*1.1)+
      labs(x="",y="Sound level in dB[A]")+
      ggtitle(bquote(list(IR==.(round(IM_ratio,1)),Events==.(N_events),L[eq_T_tot]==.(round(LEQ_T_tot,1)),L[eq_T_events]==.(round(LEQ_T_events,1)),C==.(C))))+
      theme_bw()
    ggsave(paste0(plot_filename,".png"),plot=p1,units="cm",dpi=600,width=20,height=10)
  }

  #6) Return intermittancy ratio:
  return(IM_ratio)
}
