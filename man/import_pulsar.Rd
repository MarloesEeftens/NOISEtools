\name{import_pulsar}
\alias{import_pulsar}
\title{"import_pulsar" is an import function for Pulsar Noise meter files, saved in txt format.}
\description{Given the pathname of a file downloaded from the Pulsar sound level meter, this function imports the speficied file into the R environment as a dataframe.}
\usage{import_pulsar(filename,timeformat,prefix)}
\arguments{
  \item{filename}{The complete filename (including path, name and extension) to the file you want to import into R.}
  \item{timeformat}{(Optional, several default options available.) The time format in Pulsar files changes between devices and computers. The function searches for the correct format. If the correct format is not found automatically, the function will produce an error. You can then provide timeformat yourself.}
  \item{prefix}{(Optional, no default) Sometimes it is handy to import the column names with a prefix (e.g. "PULSAR_") so they are unique within the dataframe if you want to use the time series to compare with another instrument. The result of specifying a prefix is that all columns in the imported dataset (except for PosixTime) are named (e.g. "PULSAR_") PULSAR_LAT, PULSAR_LCT etc.}
}
\details{}
\value{The result is an R dataframe of your Pulsar file which is ready to work with.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{The Pulsar example .txt files are only available from within SwissTPH (sorry!). But chances are you have your own anyways! If not, just contact me.}
\seealso{}
\examples{
#Write the example .txt to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/NOISEtools/data/pulsar_example.txt"
write.table(pulsar_example,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_pulsar_file<-import_pulsar(filename=my_filename1,prefix="pulsar_")
str(my_pulsar_file)
}
\keyword{}
