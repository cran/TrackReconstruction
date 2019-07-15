\name{Standardize}
\alias{Standardize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardize accelerometer and magnetometer data
}
\description{
Calculates intercept and slope values with a linear model to standardize accelerometer and magnetometer data to values between
-1 and +1.
}
\usage{
Standardize(MagOrSR, MagOrHV, MagOrSW, AccOrSR, AccOrHV, AccOrSW, magSRmin, 
magSRmax, magHVmin, magHVmax, magSWmin, magSWmax, accSRmin, accSRmax, accHVmin,
accHVmax, accSWmin, accSWmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MagOrSR}{
Surge Magnetometer Orientation -1 or 1
}
  \item{MagOrHV}{
Heave Magnetometer Orientation -1 or 1
}
  \item{MagOrSW}{
Sway Magnetometer Orientation -1 or 1
}
  \item{AccOrSR}{
Surge Accelerometer Orientation -1 or 1
}
  \item{AccOrHV}{
Heave Accelerometer Orientation -1 or 1
}
  \item{AccOrSW}{
Sway Accelerometer Orientation -1 or 1
}
  \item{magSRmin}{
Surge Magnetometer minimum
}
  \item{magSRmax}{
Surge Magnetometer maximum
}
  \item{magHVmin}{
Heave Magnetometer minimum
}
  \item{magHVmax}{
Heave Magnetometer maximum
}
  \item{magSWmin}{
Sway Magnetometer minimum
}
  \item{magSWmax}{
Sway Magnetometer maximum
}
  \item{accSRmin}{
Surge Accelerometer minimum
}
  \item{accSRmax}{
Surge Accelerometer maximum
}
  \item{accHVmin}{
Heave Accelerometer minimum
}
  \item{accHVmax}{
Heave Accelerometer maximum
}
  \item{accSWmin}{
Sway Accelerometer minimum
}
  \item{accSWmax}{
Sway Accelerometer maximum
}
}
\details{
Standardization/Calibration/normalization/relativizing of accelerometer and magnetometer data.  The right-hand-rule
indicates the orientation (polarity) of the magnetometers and accelerometers required for the pseudotrack reconstruction
algorithm, so that when the front, top or left side of the tag is facing the earth, the accelerometers 
are at the maximal reading (+1) when the tag is not moving.  A similar rule applies for the magnetometers except the
maximal reading for each axis will be when the front, top or left side is facing north and at the angle of 
inclination of the magnetic field (the angle at which the magnetic field enters the earth) at that location on the planet.
If the tag sensors conform to this rule all the orientation parameters should be 1, any sensors that are opposite to this
should have -1 as an orientation value.  Instead of labeling the tag in X,Y and Z dimensions, the directions are labeled
as Surge, Heave and Sway where Surge indicates the front (anterior) to back (posterior) axis, the Heave is the 
top (dorsal) to bottom (ventral) axis and the Sway is the right to left (lateral) axis.
}
\value{
Returns a [2,6] matrix with intercept in the first row and slope in the second row and columns in the order of the 
orientation parameter input.  Row names are c("B0 Intercept","B1 Slope") and column names are
c("MagSurge","MagHeave","MagSway","AccSurge","AccHeave","AccSway").
}

\author{
Brian Battaile
}

\examples{
betas<-Standardize(1,1,-1,1,1,1,-57.8,68.76,-61.8,64.2,-70.16,58.08,-10.1,9.55,-9.75,9.72,
-9.91,9.43)
betas
}

