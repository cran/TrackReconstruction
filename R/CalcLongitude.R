CalcLongitude <-
function(initialLat, matchingLat, initialLong, distance, bering) #distance in meters
{
	er<-6371000# earths radius in meters
	initialLat<-initialLat/360*2*pi
	initialLong<-initialLong/360*2*pi
	initialLong + atan2(sin(bering)*sin(distance/er)*cos(initialLat),cos(distance/er)-sin(initialLat)*sin(matchingLat)) #Longitude Rad
}
