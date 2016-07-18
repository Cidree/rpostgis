## pgGetBoundary

##' Retrieve bounding envelope (rectangle) of all geometries or
##' rasters in a table in Postgresql.  title Returns bounding envelope
##' of all combined geometries or rasters stored in a table in a
##' PostgreSQL database.
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema (if
##'     necessary), and table or view name for the table holding the
##'     geometries/raster(s) (e.g., name = c("schema","table"))
##' @param geom character, Name of the column in 'name' holding the
##'     geometry or raster object (Default = 'geom')
##' @author David Bucklin \email{david.bucklin@gmail.com}
##' @importFrom sp CRS
##' @importFrom rgeos readWKT
##' @export
##' @return SpatialPolygon
##' @examples
##' \dontrun{
##' drv <- dbDriver("PostgreSQL")
##' conn <- dbConnect(drv, dbname = "dbname", host = "host", port = "5432",
##'     user = "user", password = "password")
##' pgGetBoundary(conn, c("schema", "polys"), geom = "polygon")
##' pgGetBoundary(conn, c("schema", "rasters"), geom = "rast")
##' }
pgGetBoundary <- function(conn, name, geom = "geom")
{
  ## Check and prepare the schema.name
  if (length(name) %in% 1:2) {
    name <- paste(name, collapse = ".")
  } else {
    stop("The table name should be \"table\" or c(\"schema\", \"table\").")
  }

  ## Check data type
  str<-paste0("SELECT DISTINCT pg_typeof(",geom,") AS type FROM ",name,"
              WHERE ",geom," IS NOT NULL;")
  type<-suppressWarnings(dbGetQuery(conn,str))
  if (type$type == 'raster') {func<-'st_union'} else if
    (type$type == 'geometry') {func<-'st_collect'} else
    {stop(paste0(geom," column does not contain geometries or rasters"))}

  ## Retrieve the SRID
  str <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ",
                name, " WHERE ", geom, " IS NOT NULL;")
  srid <- dbGetQuery(conn, str)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) != 1)
    stop("Multiple SRIDs in the geometry/raster")

  p4s <- sp::CRS(paste0("+init=epsg:", srid$st_srid))@projargs

  #retrieve envelope
  str<-paste0('SELECT st_astext(st_envelope('
              ,func,'(',geom,'))) FROM ',name,';')
  wkt<-suppressWarnings(dbGetQuery(conn,str))

  env<-rgeos::readWKT(wkt$st_astext,p4s=p4s)
  return(env)
}
