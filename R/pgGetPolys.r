# pgGetPolys
#' Retrieve polygon geometries from a PostGIS table, and convert it to
#' a SpatialPolygons or a SpatialPolygonsDataFrame.
#
#' @title Load a polygon geometry stored in a PostgreSQL database into R.
#'
#' @param conn A connection object to a PostgreSQL database
#' @param name A character string specifying a PostgreSQL schema (if necessary), 
#' and table or view name for the table holding the polygon 
#' geometry (e.g., name = c("schema","table"))
#' @param geom character, Name of the column in 'name' holding the 
#' geometry object (Default = 'geom')
#' @param gid character, Name of the column in 'name' holding the ID 
#' for each polygon geometry. Should be unique if additional columns 
#' of unique data are being appended. \code{gid=NULL} 
#' (default) automatically creates a new unique ID for each row in the table.
#' @param other.cols character, names of additional columns from table
#' (comma-seperated) to append to dataset (Default is all columns, 
#' other.cols=NULL returns a SpatialPolygons object)
#' @param query character, additional SQL to append to modify 
#' select query from table
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return SpatialPolygonsDataFrame or SpatialPolygons
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'
#' pgGetPolys(conn,c('schema','tablename'))
#' pgGetPolys(conn,c('schema','states'),geom='statesgeom',gid='state_ID',
#'            other.cols='area,population', 
#'            query = "AND area > 1000000 ORDER BY population LIMIT 10")
#' }

pgGetPolys <- function(conn, name, geom = "geom", gid = NULL, 
                       other.cols = "*", query = NULL) {
  
  ## Check and prepare the schema.name
  if (length(name) %in% 1:2) {
    name <- paste(name, collapse = ".")
  } else {
    stop("The table name should be \"table\" or c(\"schema\", \"table\").")
  }
  
  ## Retrieve the SRID
  str <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ", 
                name, " WHERE ", geom, " IS NOT NULL;")
  srid <- dbGetQuery(conn, str)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) != 1) 
    stop("Multiple SRIDs in the polygon geometry")
  
  if (is.null(gid)) {
    gid <- "row_number() over()"
  }
  
  if (is.null(other.cols)) {
    que <- paste0("select ", gid, " as tgid,st_astext(", 
                  geom, ") as wkt from ", name, " where ", geom, " is not null ", 
                  query, ";")
    dfTemp <- suppressWarnings(dbGetQuery(conn, que))
    row.names(dfTemp) = dfTemp$tgid
  } else {
    que <- paste0("select ", gid, " as tgid,st_astext(", 
                  geom, ") as wkt,", other.cols, " from ", name, " where ", 
                  geom, " is not null ", query, ";")
    dfTemp <- suppressWarnings(dbGetQuery(conn, que))
    row.names(dfTemp) = dfTemp$tgid
  }
  
  p4s <- CRS(paste0("+init=epsg:", srid$st_srid))@projargs
  tt <- mapply(function(x, y, z) readWKT(x, y, z), x = dfTemp[, 
                                                              2], y = dfTemp[, 1], z = p4s)
  
  Spol <- SpatialPolygons(lapply(1:length(tt), function(i) {
    lin <- slot(tt[[i]], "polygons")[[1]]
    slot(lin, "ID") <- slot(slot(tt[[i]], "polygons")[[1]], 
                            "ID")  ##assign original ID to polygon
    lin
  }))
  
  Spol@proj4string <- slot(tt[[1]], "proj4string")
  
  if (is.null(other.cols)) {
    return(Spol)
  } else {
    try(dfTemp[geom] <- NULL)
    try(dfTemp["wkt"] <- NULL)
    spdf <- SpatialPolygonsDataFrame(Spol, dfTemp)
    spdf@data["tgid"] <- NULL
    return(spdf)
  }
} 