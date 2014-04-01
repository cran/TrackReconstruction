\name{GapFinder}
\alias{GapFinder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find gaps in your data file
}
\description{
The function looks at the DateTime or Date and Time stamps of your data file and sees if any consecutive 
entries have a difference of greater than the timediff parameter.
}
\usage{
GapFinder(rawdata, timediff = 1, timeformat = "\%d-\%b-\%Y \%H:\%M:\%S")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawdata}{
Data with a combined DateTime or seperate Date and Time stamp columns labeled as such.
}
  \item{timediff}{
The amount of time in seconds that is expected between each entry (row).  Typically this would be
your data collection Hz but results will depend on whether or not you have information on decimal seconds.  
One second is probably small enough for most people and will aviod issues with decimal seconds, but this depends on 
the scale that is important to you.
}
\item{timeformat}{
Format of the DateTime or seperate Date and Time stamp after it is pasted together with a space between. 
See the strptime function for codes.  The default value is the value of the example files.
}
}

\value{
Returns a data frame with 4 columns indicating the row that the gap occurs, the amount of time between the gap, 
and the DateTime stamp on either end of the gap.  The data frame holds a maximum of 1000 entries, so if you have
more gaps then you can alter the program to create a larger dataframe.  Hopefully you don't have anywhere near that many gaps.
}

\author{
Brian Battaile
}

\examples{
data(rawdatagap)
gaps<-GapFinder(rawdatagap, timediff = 1, timeformat = "\%d-\%b-\%Y \%H:\%M:\%S")

#how timediff and a lack of decimal seconds information interact given an Hz of 16.
gaps<-GapFinder(rawdatagap, timediff = 1/16, timeformat = "\%d-\%b-\%Y \%H:\%M:\%S")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
