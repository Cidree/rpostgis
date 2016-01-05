# pgGetRast
#' Retrieve rasters from a PostGIS table
#' 
#' @title Load a raster stored in a PostgreSQL database into R.
#'
#' @param conn A connection object to a PostgreSQL database
#' @param table A character string specifying a PostgreSQL schema (if necessary), 
#' and table or view name for the table holding the 
#' raster (e.g., table = c("schema","table"))
#' @param rast Name of the column in 'table' holding the raster object
#' @param digits numeric, precision for detecting whether points 
#' are on a regular grid (a low number of digits is a low precision) -
#' From rasterFromXYZ function (\code{raster} package)
#' @param NSEW numeric, clipping box for raster with four arguments
#' (north, south, east, west) indicating the projection-specific 
#' limits with which to clip the raster.
#' @author David Bucklin \email{david.bucklin@gmail.com}
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
#'          NSEW=c(55,50,17,12))
#' }

pgGetRast <- function(conn, table, rast = "rast", 
                      digits = 9, NSEW = c(NULL, NULL, NULL, NULL)) {
  
  #format table name
  if (length(table) %in% 1:2) {
    table <- paste(table, collapse = ".")
  } else {
    stop("The table name should be \"table\" or c(\"schema\", \"table\").")
  }
  
  ## Retrieve the SRID
  str <- paste0("SELECT DISTINCT(ST_SRID(", rast, ")) FROM ", 
                table, " WHERE ", rast, " IS NOT NULL;")
  srid <- dbGetQuery(conn, str)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) != 1) 
    stop("Multiple SRIDs in the raster")
  
  if (is.null(NSEW)) {
    trast <- suppressWarnings(dbGetQuery(conn, paste0("SELECT ST_X(ST_Centroid((gv).geom)) as x, ST_Y(ST_Centroid((gv).geom)) as y, 
                                                      (gv).val FROM (SELECT ST_PixelAsPolygons(", 
                                                      rast, ") as gv FROM ", table, ") a;")))
  } else {
    trast <- suppressWarnings(dbGetQuery(conn, paste0("SELECT ST_X(ST_Centroid((gv).geom)) as x, ST_Y(ST_Centroid((gv).geom)) as y, 
                                                      (gv).val FROM (SELECT ST_PixelAsPolygons(ST_Clip(", 
                                                      rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", NSEW[4], 
                                                      " ", NSEW[1], ",", NSEW[4], " ", NSEW[2], ",\n  ", 
                                                      NSEW[3], " ", NSEW[2], ",", NSEW[3], " ", NSEW[1], 
                                                      ",", NSEW[4], " ", NSEW[1], "))'),", srid, "))) as gv FROM ", 
                                                      table, "\n  WHERE ST_Intersects(", rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", 
                                                      NSEW[4], " ", NSEW[1], ",", NSEW[4], " ", NSEW[2], 
                                                      ",\n  ", NSEW[3], " ", NSEW[2], ",", NSEW[3], " ", 
                                                      NSEW[1], ",", NSEW[4], " ", NSEW[1], "))'),", srid, 
                                                      "))) a;")))
  }
  
    p4s <- CRS(paste0("+init=epsg:", srid))@projargs
    return(rasterFromXYZ(trast, crs = CRS(p4s), digits = digits))
} 
