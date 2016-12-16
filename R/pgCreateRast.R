library(rpostgis)
cred<-scan("C:/Users/dbucklin/Documents/.pgpass_rpostgis", what = "character")
conn <- dbConnect("PostgreSQL", host = "basille-flrec", dbname = "rpostgisLT_demo",
                  user = cred[3], password = cred[4])

data(db_raster)
rast<-db_raster$corine06
name<-c("env_data","corine")
dbDrop(conn, name)

# from (http://postgis.net/docs/using_raster_dataman.html#RT_Creating_Rasters) 5.1.2.

pgCreateRast <- function(conn, name, rast) {
  
  nameq<-rpostgis:::dbTableNameFix(conn,name)
  
  #1. create raster table
  dbExecute(conn, paste0("CREATE TABLE ",paste(nameq,collapse=".")," (rid serial primary key, rast raster);"))
  
  r1<-rast
  res<-round(res(r1), 10)
  
  # figure out block size
  tr<-blockSize(r1, 10000, minblocks = 1,minrows = 5) # check on this
  
  # loop over blocks
  for (i in 1:tr$n) {
    r<-r1[tr$row[i]:(tr$row[i]+tr$nrows[i]-1),,drop=FALSE]
    ex<-extent(r)
    d<-dim(r)
    #res<-round(res(r),10)
    srid<-suppressMessages(pgSRID(conn,r@crs))
  
    #2. make empty raster
    dbExecute(conn, paste0("INSERT INTO ",paste(nameq,collapse=".")," (rid, rast) VALUES (",i,"
                           , ST_MakeEmptyRaster(",d[2],",",d[1],",",ex[1],",",ex[3],",",res[1],",",res[2],", 0, 0,",srid,") );"))
  
    #3. new band
    dbExecute(conn, paste0("UPDATE ",paste(nameq,collapse=".")," SET rast = ST_AddBand(rast,'16BUI'::text, 1)
              where rid = ",i,";"))
#### make sure bit depth is correct for data values (http://postgis.net/docs/RT_ST_BandPixelType.html)
  
  
    mr<-as.matrix(r)
    r2<-paste(
      rev(apply(mr,1,FUN = function(x) {paste0("[",paste(x,collapse = ","),"]")}))
      ,collapse = ",")
    
    dbExecute(conn, paste0("UPDATE ",paste(nameq,collapse=".")," SET rast = ST_SetValues(rast,1, 1, 1, ARRAY[",r2,"]::double precision[][])
                           where rid = ",i,";"))
  }

  #4. create index
  dbExecute(conn,paste0("CREATE INDEX ",gsub('"','',nameq[2]),"_rast_st_conhull_idx ON ",paste(nameq,collapse=".")," USING gist( ST_ConvexHull(rast) );"))
  
  
  #5. add raster constraints (http://postgis.net/docs/RT_AddRasterConstraints.html)
  dbExecute(conn,paste0("SELECT AddRasterConstraints(",dbQuoteString(conn,name[1]),"::name,",dbQuoteString(conn,name[2]),"::name, 'rast'::name);"))
            
            # can do constraints seperately or imply from data (above)
            #boolean srid=true, boolean scale_x=true, boolean scale_y=true, boolean blocksize_x=true, boolean blocksize_y=true, 
            #boolean same_alignment=true, boolean regular_blocking=false, boolean num_bands=true, boolean pixel_types=true, boolean nodata_values=true , 
            #boolean out_db=true , boolean extent=true 
            #);"))
  
  return(TRUE)

}

pgCreateRast(conn,name,rast)

r2<-pgGetRast(conn, c("env_data","corine"))#, digits = 4)

dbGetQuery(conn,"SELECT srid, scale_x, scale_y, blocksize_x, blocksize_y, num_bands, pixel_types, nodata_values
	FROM raster_columns
	WHERE r_table_name = 'srtm';")