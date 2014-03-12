GeoReference <-
function(drdata, gpsdata)
{
	###########drdata first entry must line up with the time stamp of the first engty for GPS data#####################
	#Based on Wilson et al 2007  All at sea with Animal tracks...
	#Requires "Navigation Functions"
	#timer=proc.time()[3] # used to determine slow parts of the program and show some output while it is running to indicate where a failure occurs.
	drnrow <- nrow(drdata)
	
	GPSBering=gpsdata[1,6]
	GPSDistance <- gpsdata[2,8]
	GPSXLong <- sin(GPSBering)*GPSDistance*1000
	GPSYLat <- cos(GPSBering)*GPSDistance*1000
	initLat <- gpsdata$Latitude[1] #gpsdata Lat Degrees
	initLong <- gpsdata$Longitude[1] #gpsdata Long Degrees
	
	FinalDRdistance <- sqrt(drdata$Xdim[drnrow]^2+drdata$Ydim[drnrow]^2)
	FinalDRBering<-atan2(drdata$Xdim[drnrow],drdata$Ydim[drnrow])
	FinalDRLat<-CalcLatitude(initLat,FinalDRdistance,FinalDRBering)
	FinalDRLong<-CalcLongitude(initLat,FinalDRLat,initLong,FinalDRdistance,FinalDRBering)
	
	AdjXLong=(GPSXLong-drdata$Xdim[drnrow])/(sum(drdata$Speed)) #drnrow-1
	AdjYLat=(GPSYLat-drdata$Ydim[drnrow])/(sum(drdata$Speed)) #drnrow-1
	
	#rownum=c(0,seq(drnrow-1))
	Spdsum=cumsum(drdata$Speed)
	
	GeoRefLatLong <- matrix(0,drnrow,11,dimnames=list(1:drnrow,c("DateTime","Distance","LatRad","LongRad","Latitude","Longitude","Depth","DRCalcSpd","NewX","NewY","Bering")))
	NewX<-drdata$Xdim+AdjXLong*Spdsum #rownum
	NewY<-drdata$Ydim+AdjYLat*Spdsum #rownum
	Distance<-sqrt(NewX[2:drnrow]^2+NewY[2:drnrow]^2)
	Bering<-atan2(NewX[2:drnrow],NewY[2:drnrow])
	GeoRefLatLong[,9] <- NewX#format(NewX, digits=10, drop0trailing=FALSE )#NewX
	rm(NewX)
	GeoRefLatLong[,10] <- NewY#format(NewY, digits=10, drop0trailing=FALSE )#NewY
	rm(NewY)
	LatRad<-CalcLatitude(initLat,Distance,Bering)
	LongRad<-CalcLongitude(initLat,LatRad,initLong,Distance,Bering)
	LatRad<-c(initLat*2*pi/360,LatRad)
	LongRad<-c(initLong*2*pi/360,LongRad)
	Bering<-c(Bering,0)
	GeoRefLatLong[,11] <- Bering#format(Bering, digits=10, drop0trailing=FALSE )#Bering
	rm(Bering)
	Distance<-c(0,Distance)
	
	
	Latitude<-LatRad/(2*pi)*360
	Longitude<-LongRad/(2*pi)*360
	
	
	#XYZ_Distance=c(0,sqrt(drdata$Speed[2:drnrow]^2+(Distance[2:drnrow]-Distance[1:drnrow-1])^2))
	#GeoRefLatLong[,9] <- XYZ_Distance#format(XYZ_Distance, digits=10, drop0trailing=FALSE )#XYZ_Distance
	#rm(XYZ_Distance)
	GeoRefLatLong[,2] <- Distance#format(Distance, digits=10, drop0trailing=FALSE )#Distance
	rm(Distance)
	GeoRefLatLong[,3] <- LatRad#format(LatRad, digits=10, drop0trailing=FALSE)
	rm(LatRad)
	GeoRefLatLong[,4] <- LongRad#format(Longitude, digits=10, drop0trailing=FALSE)
	rm(LongRad)
	GeoRefLatLong[,5] <- Latitude#format(Latitude, digits=10, drop0trailing=FALSE)
	rm(Latitude)
	GeoRefLatLong[,6] <- Longitude#format(LongDeg, digits=10, drop0trailing=FALSE)
	rm(Longitude)
	GeoRefLatLong[,7] <- drdata$Depth#format(drdata$Depth, digits=5, drop0trailing=FALSE )#Depth
	#GeoRefLatLong[,8] <- format(drdata[,5], digits=10, drop0trailing=FALSE )#ODBA
	GeoRefLatLong[,8] <- drdata$Speed#format(drdata$Speed, digits=10, drop0trailing=FALSE, trim=FALSE)#DRCalcSpeed
	
	#Format the TimeDate stamp	
	GeoRefLatLong <- as.data.frame(GeoRefLatLong)
	GeoRefLatLong[,1] <- as.character(drdata$DateTime)
	#print(c("1",proc.time()[3]-timer)) # Testing and failure information
	return(GeoRefLatLong=GeoRefLatLong)#list(AdjXLong=AdjXLong,AdjYLat=AdjYLat))#for Squish factors
}
