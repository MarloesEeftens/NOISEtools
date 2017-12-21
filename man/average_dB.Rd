\name{average_dB}
\alias{average_dB}
\title{"average_dB" takes a logarithmic mean of sound levels expressed in dB}
\description{Given a vector or a selection of variables from a dataframe, this function produces the  logarithmic mean of the sound levels.}
\usage{average_dB(x,vars)}
\arguments{
  \item{x}{A numeric vector or R dataframe that includes the values for which the logarithmic mean should be calculated.}
  \item{vars}{If x is a dataframe, indicate a single variable name OR a vector of variable names for conversion.}
}
\details{}
\value{If x is a vector, the result is the logarithmic mean of x, a numeric value [dB]. If x is a dataframe, the result is a named vector with the logarithmic mean sound levels for all selected variables [dB]}
\references{The calculation of the logarithmic mean for sound levels is discussed here: Wunderli, Jean Marc, et al. "Intermittency ratio: A metric reflecting short-term temporal variations of transportation noise exposure." Journal of Exposure Science and Environmental Epidemiology 26.6 (2016): 575-585.}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{}
\seealso{}
\examples{
#Calculate the average for a vector of sound level measurements.
my_sound_measurements<-c(50,53,52,56,58)
average_dB(my_sound_measurements)

#Calculate the average sound level across multiple variables in a dataframe
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/NOISEtools/data/sentry_example.csv"
write.table(sentry_example,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_sentry_file<-import_sentry(filename=my_filename1)
#Calculate the logarithmic mean for all columns expressed in dB. (Just for illustration purposes, i doubt it makes much sense to calculate a mean of LMax and LMin in this case...)
average_dB(my_sentry_file,vars=c("LEQ","LMax","LMin"))
}
\keyword{}
