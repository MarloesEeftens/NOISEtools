\name{import_sentry}
\alias{import_sentry}
\title{"import_pulsar" is an import function for Sentry Noise meter files, saved in txt format.}
\description{Given the pathname of a file downloaded from the Sentry sound level meter, this function imports the speficied file into the R environment as a dataframe.}
\usage{import_sentry(filename,timeformat,prefix)}
\arguments{
  \item{filename}{The complete filename (including path, name and extension) to the file you want to import into R.}
  \item{timeformat}{(Optional, several default options available.) The time format in Pulsar files changes between devices and computers. The function searches for the correct format. If the correct format is not found automatically, the function will produce an error. You can then provide timeformat yourself.}
  \item{prefix}{(Optional, no default) Sometimes it is handy to import the column names with a prefix (e.g. "SENTRY_") so they are unique within the dataframe if you want to use the time series to compare with another instrument. The result of specifying a prefix is that all columns in the imported dataset (except for PosixTime) are named (e.g. "SENTRY_") SENTRY_LMax, SENTRY_LEQ, etc.}
}
\details{}
\value{The result is an R dataframe of your Sentry file  which is ready to work with and includes the variables:
  \item{\code{PosixTime}}{POSIXct, date and time of each observation.}
  \item{\code{id}}{character, the unique id number of the instrument used for the measurement.}
  \item{\code{LMax}}{Maximum sound level in dB (A-weighted).}
  \item{\code{LEQ}}{Equivalent continuous sound level in dB (A-weighted), also known as time average sound level or LAT.}
  \item{\code{LMin}}{Minimum sound level in dB (A-weighted).}
}
\references{See the instrument producer's website for more information about the device: https://convergenceinstruments.com/pc/sound-level-meter-data-loggers/. See the following website for more information about A-weighting: http://www.acoustic-glossary.co.uk/frequency-weighting.htm.}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{}
\seealso{}
\examples{
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/NOISEtools/data/sentry_example.csv"
write.table(sentry_example,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_sentry_file<-import_sentry(filename=my_filename1,prefix="SENTRY_")
str(my_sentry_file)
}
\keyword{}
