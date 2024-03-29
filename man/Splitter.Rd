\name{Splitter}
\alias{Splitter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Splits large data files by date and time. 
}
\description{
This function takes the large data files inherently produced by accelerometer and magnetometer biologgers and splits
them into smaller files so that the \code{TrackReconstruction} functions can handle them or it splits the data between trips
or GPS locations or however is needed.  The splitting is done via matching Time and Date as character strings.
}
\usage{
Splitter(TagFile,Begin,End,RmL,Hz)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TagFile}{
Matrix or data frame of magnetometer and accelerometer and other data collected by biologgers with column headings specified in Details.
}
  \item{Begin}{
A vector of DateTime such as 2009-08-12 05:45:35.0625 indicating the time that the new files should begin.  Format must be the same as the
Date and Time data in the \code{TagFile}, though the \code{TagFile} Date and Time need not be in the same column.
}
  \item{End}{
A vector of DateTime such as 2009-08-12 05:45:35.0625 indicating the time that the new files should end.  Format must be the same as the
Date and Time data in the \code{TagFile}, though the \code{TagFile} Date and Time need not be in the same column.
}
  \item{RmL}{
Running mean length in seconds, required to calculate the amount of time beyond the \code{Begin} and \code{End} times that is required for the \code{DeadReckoning}
function and is trimmed off by the \code{DeadReckoning} function.
}
  \item{Hz}{
Frequency of Accelerometer data collection in Hz, required to calculate the amount of time beyond the Begin and End times that is required for the \code{DeadReckoning}
function and is trimmed off by the \code{DeadReckoning} function.
}
}
\details{
\code{TagFile} must have columns named DateTime or separated as Date and Time.  \code{Begin} and \code{End} must be in the same format as the DateTime 
column in \code{TagFile} or have the same format as the Date and Time columns when they are pasted together by \code{Splitter}.  For example, if
DateTime is 2009-08-21 14:08:06.0625, then \code{Begin} and \code{End} cannot be Jul/21/2009 14:08:06.0625.  See \code{strptime} for formatting
date and time data.  If \code{TagFile} has a Date column of 2009-08-21 and a Time column of 14:08:06.0625, \code{Splitter} will paste them
together for you to look like 2009-08-21 14:08:06.0625.  You must have enough time on the beginning and end of the \code{TagFile}
equivalent to \code{RmL}*Hz/2 before the first DateTime in the \code{Begin} vector and after the last DateTime in the \code{End} vector.  
If your Hz is greater than 1 and you do not have data on fractions of a second, then each time stamp will have copies equal to your 
sampling Hz.  In such a case, the program matches with the first instance of the DateTime and warnings will be given, this may be important
when calculating \code{RmL}*Hz/2 tails.
}
\value{
Creates a list of data frames in the format of the \code{TagFile}, but with a single DateTime column if the \code{TagFiles} had separate Date and Time
columns.
}
\author{
Brian Battaile
}

\examples{
#data set with 6 associated GPS fixes in the "gpsdata" data set
data(rawdata)
data(gpsdata02)
Begin=gpsdata02$DateTime[2:6]
End=gpsdata02$DateTime[3:7]
splits<-Splitter(rawdata,Begin,End,RmL=2,Hz=16)
\dontrun{
#The following is code to write your many new files
setwd() #first fill in the path to the folder where you want the data to be written to
for(i in 1:length(splits))
{
	num<-i
	num=ifelse(num<10 & length(Begin)>10,paste("0",num,sep=""),num)
		num=ifelse(num<100 & length(Begin)>100,paste("0",num,sep=""),num)
		num=ifelse(num<1000 & length(Begin)>1000,paste("0",num,sep=""),num)
		num=ifelse(num<10000 & length(Begin)>10000,paste("0",num,sep=""),num)
	#Create a name for the file
	Nombre<-paste("Animal01Trip03GPS_Section",num,".txt",sep="")
	write.table(splits[[i]],Nombre,sep="\t",row.names=FALSE,quote=FALSE)
	}
}
}

