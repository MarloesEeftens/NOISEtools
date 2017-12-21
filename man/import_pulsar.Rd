\name{import_pulsar}
\alias{import_pulsar}
\title{"import_pulsar" is an import function for Pulsar Noise meter files, saved in txt format.}
\description{Given the pathname of a file downloaded from the Pulsar sound level meter, this function imports the speficied file into the R environment as a dataframe. The function extracts entire measurement as a time series which should be custom-cut to the time interval of interest. The function does not extract the summary statistics of the entire measurement.}
\usage{import_pulsar(filename,timeformat,prefix)}
\arguments{
  \item{filename}{The complete filename (including path, name and extension) to the file you want to import into R.}
  \item{timeformat}{(Optional, several default options available.) The time format in Pulsar files changes between devices and computers. The function searches for the correct format. If the correct format is not found automatically, the function will produce an error. You can then provide timeformat yourself.}
  \item{prefix}{(Optional, no default) Sometimes it is handy to import the column names with a prefix (e.g. "PULSAR_") so they are unique within the dataframe if you want to use the time series to compare with another instrument. The result of specifying a prefix is that all columns in the imported dataset (except for PosixTime) are named (e.g. "PULSAR_") PULSAR_LAT, PULSAR_LCT etc.}
}
\details{}
\value{The result is an R dataframe of your Pulsar file  which is ready to work with and includes the variables:
  \item{\code{PosixTime}}{POSIXct, date and time of each observation.}
  \item{\code{id}}{character, the unique id number of the instrument used for the measurement.}
  \item{\code{LAT}}{Time average sound level in dB (A-weighted), also known as equivalent continuous sound level or LEQ.}
  \item{\code{LCT}}{Time average sound level in dB (C-weighted).}
  \item{\code{LZT}}{Time average sound level in dB (Z-weighted).}
}
\references{See the instrument producer's website for more information about the device: https://pulsarinstruments.com/news-detail/pulsar-model-30-sound-investment-for-measuring-industrial-noise/. See the following website for more information about A-weighting, C-weighting and Z-weighting: http://www.acoustic-glossary.co.uk/frequency-weighting.htm.}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{The Pulsar example .txt example file is only available from within SwissTPH. The file as downloaded directly from the Pulsar is not supported by lazyload. This appears to be because the read.table() does not support files which have more columns than column names. Unfortunately, this is just the way the .txt file structure is... But chances are you have your own raw files anyways! If not, just contact me.}
\seealso{}
\examples{
#Location of the pulsar_example.txt file on the Swiss TPH drive:
my_filename1<-"V:/EEH/R_functions/NOISEtools/data/pulsar_example.txt"
#Import using the import function:
my_pulsar_file<-import_pulsar(filename=my_filename1,prefix="PULSAR_")
str(my_pulsar_file)
}
\keyword{}
