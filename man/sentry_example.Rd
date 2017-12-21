\name{sentry_example}
\alias{sentry_example}
\docType{data}
\title{An example dataset as downloaded directly from an Sentry Noise Meter device as raw .csv data.}
\description{This .csv dataset was downloaded from an ExpoM-RF device, chopped after 1min to minimize file size.}
\usage{data("sentry_example")}
\format{
  A data frame with 63 observations on the following variables.
  \describe{
    \item{\code{Time (Date hh:mm:ss.ms)}}{Date and time at which the sample was taken.}
    \item{\code{L-Max dB -A}}{Maximum sound level in dB (A-weighted).}
    \item{\code{LEQ dB -A}}{Equivalent continuous sound level in dB (A-weighted), also known as time average sound level or LAT.}
    \item{\code{L-Min dB -A}}{Minimum sound level in dB (A-weighted).}
  }
}
\details{Data collected with the Noise Sentry RT by Convergence Instruments.}
\source{}
\references{See the instrument producer's website for more information about the device: https://convergenceinstruments.com/pc/sound-level-meter-data-loggers/. See the following website for more information about A-weighting: http://www.acoustic-glossary.co.uk/frequency-weighting.htm.}
\examples{
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/NOISEtools/data/sentry_example.csv"
write.table(sentry_example,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_sentry_file<-import_sentry(filename=my_filename1,prefix="SENTRY_")
str(my_sentry_file)
}
\keyword{}
