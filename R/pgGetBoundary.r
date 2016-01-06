# pgGetBoundary
#' Retrieve bounding envelope (rectangle) of all geometries or rasters in a 
#' table in Postgresql.
#
#' @title Returns bounding envelope of all combined geometries or
#' rasters stored in a table in a PostgreSQL database.
#'
#' @param conn A connection object to a PostgreSQL database
#' @param table A character string specifying a PostgreSQL schema (if necessary), 
#' and table or view name for the table holding the geometries/raster(s) 
#' (e.g., table = c("schema","table"))
#' @param geom character, Name of the column in 'table' holding the 
#' geometry or raster object (Default = 'geom')
#' @param raster logical, Set to TRUE if using for raster objects (Default = FALSE)
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return SpatialPolygon
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'
#' pgGetBoundary(conn,c('schema','polys'),geom = 'polygon')
#' pgGetBoundary(conn,c('schema','rasters'),geom='rast',raster=TRUE)
#' }

pgGetBoundary <- function(conn, table, geom = "geom",raster = FALSE) {

  ## Check and prepare the schema.name
  if (length(table) %in% 1:2) {
    table <- paste(table, collapse = ".")
  } else {
    stop("The table name should be \"table\" or c(\"schema\", \"table\").")
  }
  
  ## Retrieve the SRID
  str <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ", 
                table, " WHERE ", geom, " IS NOT NULL;")
  srid <- dbGetQuery(conn, str)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) != 1) 
    stop("Multiple SRIDs in the geometry/raster")

  p4s <- CRS(paste0("+init=epsg:", srid$st_srid))@projargs
  
  if(isTRUE(raster)) {func<-'st_union'} else {func<-'st_collect'}

  wkt<-dbGetQuery(conn,paste0('select st_astext(st_envelope('
                              ,func,'(',geom,'))) from ',table,';'))

  env<-readWKT(wkt$st_astext,p4s=p4s)
  return(env)
}
