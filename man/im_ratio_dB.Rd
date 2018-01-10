\name{im_ratio_dB}
\alias{im_ratio_dB}
\title{"Calculates the intermittancy ratio for a time series of sound levels expressed in dB"}
\description{Given a time series of sound levels as a numeric vector or variable within a dataframe, this function calculates the intermittancy ratio based on the measured sound levels.}
\usage{im_ratio_dB(x,L,C,timevar,plot_filename)}
\arguments{
  \item{x}{A numeric vector or R dataframe that includes the variable for which the intermittancy ratio should be calculated.}
  \item{L}{If x is a dataframe, L is the name of the variable within the dataframe which contains the sound level measurements for which IR should be calculated.}
  \item{C}{(Optional, defaults to C=3.) The "only free parameter in the definition of IR" (Wunderli, 2016). C is the difference between the average sound level Leq_T_tot and the threshold level at which we define an "event", or Leq_T_event. Setting C lower will result in a higher eventfullness.}
  \item{timevar}{(Optional, defaults to any variable of class "POSIXct", if present in dataframe specified in x.) This is the variable which contains the datetime values for the time series.}
  \item{plot_filename}{If a visual representation of the time series is required, you can specify here which folder and filename it can be saved to.}
}
\details{Please note that a noise "event" (see below) can last more than 1 second. So the number of events is not equal to the number of values above the threshold, but equal to the number of times the time series goes above the K threshold line.}
\value{The result is a named vector with 7 different statistics in it: 1) N_obs, the number of observations in the time series; 2) LEQ_T_tot, the average sound level during the measurement; 3) C, The value of the offset used to determin the exceedance threshold for events; 4) K, the threshold whose exceedance defines an "event"; 5) N_events, the number of events (see also details); 6) LEQ_T_events, the average amount of sound energy in dB[A] which can be attributed to events; 7) IM_ratio, the intermittancy ratio, a numeric value without unit, see reference below. There is an option to export a visual representation of the time series into a chosen plot_filename.}
\references{
Jean Marc Wunderli, Reto Pieren, Manuel Habermacher, Danielle Vienneau, Christian Cajochen, Nicole Probst-Hensch, Martin Röösli, Mark Brink.
Intermittency ratio: A metric reflecting short-term temporal variations of transportation noise exposure
Journal of Exposure Science and Environmental Epidemiology (2016) 26, 575–585 (2016)
doi:10.1038/jes.2015.56
}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{}
\seealso{}
\examples{
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename<-"V:/EEH/R_functions/NOISEtools/data/sentry_example.csv"
write.table(sentry_example,file=my_filename,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_sentry_file<-import_sentry(filename=my_filename)

#Calculate the intermittancy ratio over the variable "LEQ"
im_ratio_dB(my_sentry_file$LEQ)
#Calculate the intermittancy ratio for 6 different levels of C.
for (i in seq(1:6)){
  stats<-im_ratio_dB(x=my_sentry_file,L="LEQ",C=i,timevar="PosixTime",plot_filename=paste0("V:/EEH/R_functions/NOISEtools/plots/ir_",i,".png"))
  print(paste0("C=",i,", Intermittancy Ratio=",round(stats$IM_ratio,1)))
}
#See how the intermittancy ratio develops as we measure longer at the same spot.
irs_cum=list()
for(i in seq(1:1800)){irs_cum[[i]]<-im_ratio_dB(x=my_sentry_file[1:i,],timevar="PosixTime",L="LEQ")}
irs_cum_df<-as.data.frame(do.call(rbind,irs_cum))
ggplot(data=irs_cum_df,aes(x=N_obs,y=IM_ratio))+
  geom_point()+
  geom_hline(yintercept=tail(irs_cum_df$IM_ratio,1),color="blue")
}
\keyword{}
