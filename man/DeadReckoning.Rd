\name{DeadReckoning}
\alias{DeadReckoning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates Dead Reckoning 
}
\description{
This function takes tri axial magnetometer, accelerometer and optional speed data to estimate 
the path traveled by a tagged animal based on Wilson et al. (2007).
}
\usage{
DeadReckoning(rawdata, betas, decinc, Hz = 16, RmL = 2, DepthHz = 1, SpdCalc=1,
MaxSpd=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rawdata}{
matrix or data frame of magnetometer and accelerometer with column headings specified in Details.
}
  \item{betas}{
matrix or data frame of coefficients (slope and intercept) that normalizes magnetometer and accelerometer data between -1 and +1.
}
  \item{decinc}{
vector of c(Declination,Inclination) values in "degress.decimal degrees" format.
}
  \item{Hz}{
frequency of data collection in Hz, must be the same for all magnetometer and accelerometer channels but not necessarily speed or depth
}
  \item{RmL}{
Running mean length in seconds, required to seperate "dynamic" and "static" acceleration.
}
  \item{DepthHz}{
Frequency of data collection in Hz of Depth channel.
}
  \item{SpdCalc}{
Integer of value 1,2,3 or 4 depending on type of speed data supplied, see details. 
}
  \item{MaxSpd}{
If SpdCalc=3, the max speed of your animal in m/s.  If SpdCalc=4, the constant speed of your animal in m/s.
}
}
\details{
Rawdata must have columns DateTime or seperated as Date and Time, plus Depth, MagSurge, MagHeave, MagSway, AccSurge, 
AccHeave, AccSway and optionl Speed in a data frame with those exact column names (Mag=Magnetometer and Acc=
Accelerometer).  It must have an extra number of rows =Hz*RmL*0.5 at beginning and end that will be trimmed for final
output.  Because some users may program tags to collect acceleration data at a higher Hz than magnetometer data (for
example when looking for a Jerk signal), the DeadReckoning function automatically subsets the data set to those samples (rows)
that do NOT have "NA" in the MagSurge column.

Betas is a [2,6] data frame with intercept in the first row and slope in the second row and can be directly taken from
Standardize function output or user input.  See the help on the Standardize function for more information.

Declination and inclination data can be found at various intenet sites such as the World Magnetic Model 2010 Calculator
from the British Geological Survey (see references).

RmL-please read Shepard et al.(2008) and perhaps Wilson et al.(2007) for information on an appropriate Running Mean 
Length for your data and study animal.

SpdCalc\cr
=1 if Speed is supplied in rawdata at same Hz as Acc and Mag Hz in m/s.\cr
=2 if Speed is supplied in m/s but at Hz lower than Acc and Mag.\cr  
=3 if no speed is supplied and should be estimated from integrating (a running sum) the dynamic portion of the forward 
channel of acceleration and normalized using a linear model to meters/second from 0 to your max speed input.\cr
=4 if no speed is supplied then given constant speed =MaxSpd.\cr
=5 if no speed is supplied and should be estimated from accent and decent rates.  ***Not yet implemented.***\cr
=6 if speed is to be calculated from sound data.  ***Not yet implemented***
}
\value{
data.frame with columns DateTime, Xdim, Ydim, Depth and Speed. DateTime has pasted together the date and time columns 
as character. Xdim and Ydim are cartesian coordinates from the origin. Depth has been linearly interpolated from 
initial Depth Hz to Acceleration Hz and Speed is either that supplied by the user or calculated via one of the methods
outlined above.
}
\references{
One possible source for declination and inclination data \url{http://www.geomag.bgs.ac.uk/data_service/models_compass/wmm_calc.html}

Wilson R.P., Liebsch,N., Davies,I.M., Quintana,F., Weimerskirch,H., Storch,S., Lucke,K., Siebert,U., Zankl,S., Muller,G., Zimmer,I., Scolaro,A., Campagna,C., Plotz,J., Bornemann,H., Teilmann,J. and Mcmahon,C.R. (2007) All at sea with animal tracks; methodological and analytical solutions for the resolution of movement.  Deep-Sea Research II 54:193-210

Shepard E.L.C., Wilson, R.P., Halsey, L.G., Quintana, F., Laich, A.G., Gleiss, A.C., Liebsch, N., Myers, A.E., Norman, B.  (2008)  Derivation of body motion via appropriate smoothing of acceleration data.  Aquatic Biology 4:235-241
}
\author{
Brian Battaile
}

\examples{
#Standardize tag output
betas<-Standardize(1,1,-1,1,1,1,-57.8,68.76,-61.8,64.2,-70.16,58.08,
-10.1,9.55,-9.75,9.72,-9.91,9.43)
#get declination and inclination data for study area
decinc<-c(10.228,65.918)
#data set with 11 associated GPS fixes in the "gpsdata" data set
data(rawdata)
DRoutput<-DeadReckoning(rawdata,betas,decinc,Hz=16,RmL=2,DepthHz=1,SpdCalc=3,MaxSpd=3.5)
plot(DRoutput$Ydim,DRoutput$Xdim)
}
