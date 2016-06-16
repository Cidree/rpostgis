#testing writeWKT vs writeWKB
library(rpostgis)
library(rgeos)
library(wkb)

drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv,dbname='',host='basille-flrec.ad.ufl.edu',port='5432',user='',password='')

system.time(pts<-pgGetPts(con,'main.gps_data_animals',query='limit 10000'))
system.time(ptsbig<-pgGetPts(con,'main.gps_data_animals',query='limit 100000'))
system.time(pols<-pgGetPolys(con,'env_data.ne50m_ctry'))
system.time(simplepols<-pgGetPolys(con,'analysis.fknms_grid_5km'))
system.time(lines<-pgGetLines(con,'analysis.view_trajectories'))

#loop using different numbers of points
for (i in seq(10000,100000,10000)) {
  print(i)
  print(system.time(pts1<-pgGetPts(con,'main.gps_data_animals',query=paste0('limit ',i))))
  print(system.time(wkt<-writeWKT(pts1,byid=TRUE)))
  print(system.time(wkb<-lapply(writeWKB(pts1),function(x) {paste(x,collapse="")})))
  
}

#tests
#pts (10k records)
#one time
system.time(wkt<-writeWKT(pts,byid=TRUE))
system.time(wkb<-writeWKB(pts))

#five times
system.time(replicate(5,wkt<-writeWKT(pts,byid=TRUE)))
#user  system elapsed 
#4.32    0.00    4.32 
system.time(replicate(5,wkb<-lapply(writeWKB(pts),function(x) {paste(x,collapse="")})))
#user  system elapsed 
#4.44    0.00    4.43 

#pts (100k records)
#one time
system.time(wkt<-writeWKT(ptsbig,byid=TRUE))
system.time(wkb<-writeWKB(ptsbig))

#five times
system.time(replicate(5,wkt<-writeWKT(ptsbig,byid=TRUE)))
#user  system elapsed 
#431.69    0.08  433.54 
system.time(replicate(5,wkb<-lapply(writeWKB(ptsbig),function(x) {paste(x,collapse="")})))
#user  system elapsed 
#46.68    0.00   47.19

#polygons (241 country outlines)
#one time
wkt<-writeWKT(pols,byid=TRUE)
wkb<-writeWKB(pols)

#five times
system.time(replicate(5,wkt<-writeWKT(pols,byid=TRUE)))
#user  system elapsed 
#6.75    0.00    6.81 
system.time(replicate(5,wkb<-lapply(writeWKB(pols),function(x) {paste(x,collapse="")})))
#user  system elapsed 
#5.97    0.00    6.00 


#polygons (3096 squares (grid))
#one time
wkt<-writeWKT(simplepols,byid=TRUE)
wkb<-writeWKB(simplepols)

#five times
system.time(replicate(5,wkt<-writeWKT(simplepols,byid=TRUE)))
#user  system elapsed 
#1.06    0.00    1.08 
system.time(replicate(5,wkb<-lapply(writeWKB(simplepols),function(x) {paste(x,collapse="")})))
#user  system elapsed 
#3.11    0.00    3.15 

#lines
#one time
wkt<-writeWKT(lines,byid=TRUE)
wkb<-writeWKB(lines)

#five times
system.time(replicate(5,wkt<-writeWKT(lines,byid=TRUE)))
#user  system elapsed 
#9.31    0.02    9.34 
system.time(replicate(5,wkb<-lapply(writeWKB(lines),function(x) {paste(x,collapse="")})))
#user  system elapsed 
#7.88    0.00    7.91 



###playing around

system.time(pts<-pgGetPts(con,'main.gps_data_animals',query='limit 10'))
system.time(pols<-pgGetPolys(con,'env_data.ne50m_ctry',query='limit 2'))
system.time(simplepols<-pgGetPolys(con,'analysis.fknms_grid_5km',query='limit 2'))
system.time(lines<-pgGetLines(con,'analysis.view_trajectories',query='limit 2'))

wkb<-writeWKB(pts)
wkb
w1<-lapply(wkb,function(x) {paste(x,collapse="")})
           
wkb<-writeWKB(pols)
wkb
w1<-lapply(wkb,function(x) {paste(x,collapse="")})

wkb<-writeWKB(simplepols)
wkb
w1<-lapply(wkb,function(x) {paste(x,collapse="")})
           
wkb<-writeWKB(lines)
wkb
w1<-lapply(wkb,function(x) {paste(x,collapse="")})