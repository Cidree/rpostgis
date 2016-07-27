# pgSRID
#' This function takes `CRS`-class object and a PostgreSQL database connection (with PostGIS extension),
#' and returns the matching SRID(s) for that CRS. If a match is not found,
#' a new entry can be created in the PostgreSQL `spatial_ref_sys` table using the
#' parameters specified by the CRS. New entries will be created with auth_name = 'rpostgis_custom'.
#
#' @title Find the matching PostGIS SRID for a CRS object (or create a new SRID if not found)
#' @param CRS CRS object, created through a call to `CRS()` from library `sp`.
#' @param conn A connection object to a PostgreSQL database
#' @param create Logical. If no matching SRID is found, should a new SRID be created? 
#' User must have write access on spatial_ref_sys table.
#' @param new.srid integer. Optional SRID to give to a newly created SRID. If left NULL (default),
#' the next open value of `srid` in `spatial_ref_sys` between 880000 and 890000 will be used.
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return SRID code (integer)
#' @importFrom sp CRS
#' @importFrom rgdal showWKT
#' @importFrom rgdal showEPSG
#' @examples
#' 
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'                
#' crs<-CRS("+proj=longlat")
#' pgSRID(crs,conn)
#' 
#' crs2<-CRS(paste("+proj=stere +lat_0=52.15616055555555", 
#' "+lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel", 
#' "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812", 
#' "+units=m"))
#' pgSRID(crs2,conn,create=TRUE)
#' 
#' }

pgSRID<-function(CRS,conn,create=FALSE,new.srid=NULL) {
  
  #use rgdal suggests
  
  if (!suppressMessages(pgPostGIS(conn))) {stop("PostGIS is not enabled on this database.")}
  
  #check object
  if(class(CRS)[1] != "CRS") {
    stop("Object is not of class CRS.")
  }
  
  #extract p4s
  p4s<-CRS@projargs
  
  #if CRS is undefined (NA), return 0
  if(is.na(p4s)) {
    srid<-0
    return(srid)
  }
  
  # check for matching EPSG with showEPSG
  srid<-"OGRERR_UNSUPPORTED_SRS"
  try(srid<-rgdal::showEPSG(p4s))
  
  if(srid != "OGRERR_UNSUPPORTED_SRS") {
    query<-paste0("SELECT srid from spatial_ref_sys where auth_name = 'EPSG' and auth_srid = ",srid,";")
    epsg.srid<-dbGetQuery(conn,query)$srid
    
    if (length(epsg.srid) > 0) {
    return(epsg.srid)
    }
  }
  
  # if EPSG not found, check for matching p4s in spatial_ref_sys (with or without trailing white space)
  query<-paste0("SELECT srid FROM spatial_ref_sys WHERE proj4text = '",p4s,"'
                OR regexp_replace(proj4text,'[[:space:]]+$','') = '",p4s,"';")
  q<-dbGetQuery(conn,query)
  srid<-q$srid
  
  if (length(q) > 0)
  {
  return(srid) 
  } else {
    
    if (!create) {stop("No SRID matches found. Re-run with create=TRUE to create new SRID entry in spatial_ref_sys.")}
    
    if (!is.null(new.srid)) {
      #check if exists
      query<-paste0("SELECT srid FROM spatial_ref_sys WHERE srid = ",new.srid,";")
      check.srid<-dbGetQuery(conn,query)
      if (length(check.srid) > 0) {stop(paste0("SRID ",new.srid," already exists in spatial_ref_sys. 
                                               Select a new new.srid or leave it NULL to select the next open SRID between 880000 and 890000."))}
      srid<-new.srid
    } else {
    #create new SRID
    #find next SRID for custom set (prefix 88, first value = 880001)
    query<-"select min(series) as new from generate_series(880001,890000) as series where series not in
    (SELECT srid FROM spatial_ref_sys WHERE srid > 880000 AND srid < 890000)"
    
    new<-dbGetQuery(conn,query)$new
    
    if (is.na(new)) {
      stop("No available SRIDs between 880001-890000. Delete some or manually set new.srid.")
    } else {
      srid<-new
    }
    }
    
    #insert new SRID 
    query<-paste0("insert into spatial_ref_sys (srid,auth_name,auth_srid,srtext,proj4text) VALUES (",
      srid,",'rpostgis_custom',",srid,",'",rgdal::showWKT(p4s),"','",p4s,"');")
    
    dbSendQuery(conn,query)
    message(paste0("No matches were found in spatial_ref_sys. New SRID created (",srid,")."))
    return(srid)
  }
}


