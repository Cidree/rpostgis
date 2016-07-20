# pgGetRast
#' Retrieve rasters from a PostGIS table
#' 
#' @title Load a raster stored in a PostgreSQL database into R.
#'
#' @param conn A connection object to a PostgreSQL database
#' @param name A character string specifying a PostgreSQL schema (if necessary), 
#' and table or view name for the table holding the 
#' raster (e.g., name = c("schema","table"))
#' @param rast Name of the column in 'name' holding the raster object
#' @param digits numeric, precision for detecting whether points 
#' are on a regular grid (a low number of digits is a low precision) -
#' From rasterFromXYZ function (\code{raster} package)
#' @param boundary \code{sp} object or numeric. A Spatial*
#' object, whose bounding box will be used to select the 
#' part of the raster to import. Alternatively, four numbers
#' (e.g. \code{c(north, south, east, west)}) indicating the projection-specific 
#' limits with which to clip the raster. NULL (default) will
#' return the full raster.
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @importFrom raster rasterFromXYZ
#' @importFrom sp CRS
#' @export
#' @return RasterLayer
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'
#' pgGetRast(conn,c('schema','tablename'))
#' pgGetRast(conn,c('schema','DEM'),digits=9,
#'          boundary=c(55,50,17,12))
#' }

pgGetRast <- function(conn, name, rast = "rast", 
                      digits = 9, boundary = NULL) {
  
  #format table name
  if (length(name) %in% 1:2) {
    name <- paste(name, collapse = ".")
  } else {
    stop("The table name should be \"table\" or c(\"schema\", \"table\").")
  }
  
  #check table exists
  temp.query<-paste0("select r_raster_column as geo from public.raster_columns 
                     where (r_table_schema||'.'||r_table_name) = '",name,"';")
  
  tab.list<-dbGetQuery(conn,temp.query)$geo
  if (is.null(tab.list)) {
    stop(paste0("Table '",name,"' is not listed in public.raster_columns."))
  } else if (!rast %in% tab.list) {
    stop(paste0("Table '",name,"' raster column '",rast,"' not found. Available raster columns: ",paste(tab.list,collapse=", ")))
  }
  
  ## Retrieve the SRID
  temp.query <- paste0("SELECT DISTINCT(ST_SRID(", rast, ")) FROM ", 
                name, " WHERE ", rast, " IS NOT NULL;")
  srid <- dbGetQuery(conn, temp.query)
  
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) > 1) {
    stop("Multiple SRIDs in the raster")}
  else if (nrow(srid) < 1) {
    stop("Database table is empty.")
  }
  
  if (is.null(boundary)) {
    trast <- suppressWarnings(dbGetQuery(conn, paste0("SELECT ST_X(ST_Centroid((gv).geom)) as x, ST_Y(ST_Centroid((gv).geom)) as y, 
                                                      (gv).val FROM (SELECT ST_PixelAsPolygons(", 
                                                      rast, ") as gv FROM ", name, ") a;")))
  } else {
    if (typeof(boundary) != 'double') {
      boundary<-c(boundary@bbox[2,2],boundary@bbox[2,1]
              ,boundary@bbox[1,2],boundary@bbox[1,1])}
    trast <- suppressWarnings(dbGetQuery(conn, paste0("SELECT ST_X(ST_Centroid((gv).geom)) as x, ST_Y(ST_Centroid((gv).geom)) as y, 
                                                      (gv).val FROM (SELECT ST_PixelAsPolygons(ST_Clip(", 
                                                      rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4], 
                                                      " ", boundary[1], ",", boundary[4], " ", boundary[2], ",\n  ", 
                                                      boundary[3], " ", boundary[2], ",", boundary[3], " ", boundary[1], 
                                                      ",", boundary[4], " ", boundary[1], "))'),", srid, "))) as gv FROM ", 
                                                      name, "\n  WHERE ST_Intersects(", rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", 
                                                      boundary[4], " ", boundary[1], ",", boundary[4], " ", boundary[2], 
                                                      ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3], " ", 
                                                      boundary[1], ",", boundary[4], " ", boundary[1], "))'),", srid, 
                                                      "))) a;")))
  }
  
    p4s <- sp::CRS(paste0("+init=epsg:", srid))@projargs
    return(raster::rasterFromXYZ(trast, crs = sp::CRS(p4s), digits = digits))
} 
