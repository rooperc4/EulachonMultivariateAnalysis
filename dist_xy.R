dist_xy<-function(lat1,long1,lat2,long2,unit){
#lat1<-56
#lat2<-57
#long1<-(-174)
#long2<-(-171)
#unit="m"


r<-ifelse(unit == "m", 6371200,6371.2)

la1 = lat1 * pi / 180
la2 = lat2 *pi / 180
lo1 = long1 * pi / 180
lo2 = long2 * pi/ 180


rads <- acos((sin(la1) * sin(la2)) + (cos(la1) * cos(la2) * cos(lo2 - lo1)))
return(r * rads)
}
