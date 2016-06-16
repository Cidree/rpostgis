library(rpostgis)
library(rgeos)
library(wkb)

drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv,dbname='',host='',port='5432',user='',password='')

system.time(pts<-pgGetPts(con,'table',query='limit 10000'))
system.time(ptsbig<-pgGetPts(con,'table',query='limit 100000'))
system.time(pols<-pgGetPolys(con,'table'))
system.time(simplepols<-pgGetPolys(con,'table'))
system.time(lines<-pgGetLines(con,'table'))

for (i in seq(10000,150000,10000))
{
  print(i)
  print(system.time(pts<-pgGetPts(con,'table',query=paste0('limit ',i))))
  print(system.time(wkt<-writeWKT(pts)))
  print(system.time(wkb<-writeWKB(pts)))
}

#tests
#pts (10k records)
#one time
wkt<-writeWKT(pts)
wkb<-writeWKB(pts)

#five times
system.time(replicate(5,wkt<-writeWKT(pts)))
system.time(replicate(5,wkb<-writeWKB(pts)))

#pts (100k records)
#one time
wkt<-writeWKT(ptsbig)
wkb<-writeWKB(ptsbig)

#five times
system.time(replicate(5,wkt<-writeWKT(ptsbig)))
system.time(replicate(5,wkb<-writeWKB(ptsbig)))

#polygons
#one time
wkt<-writeWKT(pols)
wkb<-writeWKB(pols)

#five times
system.time(replicate(5,wkt<-writeWKT(pols)))
system.time(replicate(5,wkb<-writeWKB(pols)))


#polygons
#one time
wkt<-writeWKT(simplepols)
wkb<-writeWKB(simplepols)

#five times
system.time(replicate(5,wkt<-writeWKT(simplepols)))
system.time(replicate(5,wkb<-writeWKB(simplepols)))

#lines
#one time
wkt<-writeWKT(lines)
wkb<-writeWKB(lines)

#five times
system.time(replicate(5,wkt<-writeWKT(lines)))
system.time(replicate(5,wkb<-writeWKB(lines)))
