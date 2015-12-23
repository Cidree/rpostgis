# pgGetLines
#
#' @title Load a linestring geometry stored in a PostgreSQL database into R.
#'
#' @param conn A connection object created in RPostgreSQL package.
#' @param table character, Name of the schema-qualified table in 
#' Postgresql holding the geometry.
#' @param geom character, Name of the column in 'table' holding
#' the geometry object (Default = 'geom')
#' @param gid character, Name of the column in 'table' holding 
#' the ID for each line. Should be unique if additional columns of 
#' unique data are being appended. (Default = 'gid')
#' @param other.cols character, names of additional columns from 
#' table (comma-seperated) to append to dataset (Default is all 
#' columns, NULL returns a SpatialLines object)
#' @param query character, additional SQL to append to modify 
#' select query from table
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @export
#' @return SpatialLinesDataFrame or SpatialLines
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'
#' pgGetLines(conn,'schema.tablename')
#' pgGetLines(conn,'schema.roads',geom='roadgeom',gid='road_ID',
#'            other.cols=NULL, query = "AND field = \'highway\'")
#' }

pgGetLines <- function(conn, table, geom = "geom", gid = NULL, 
                       other.cols = "*", query = NULL) {
  
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
    stop("Multiple SRIDs in the line geometry")
  
  if (is.null(gid)) {
    gid <- "row_number() over()"
  }
  
  if (is.null(other.cols)) {
    que <- paste0("select ", gid, " as tgid,st_astext(", 
                  geom, ") as wkt from ", table, " where ", geom, " is not null ", 
                  query, ";")
    dfTemp <- suppressWarnings(dbGetQuery(conn, que))
    row.names(dfTemp) = dfTemp$tgid
  } else {
    que <- paste0("select ", gid, " as tgid,st_astext(", 
                  geom, ") as wkt,", other.cols, " from ", table, " where ", 
                  geom, " is not null ", query, ";")
    dfTemp <- suppressWarnings(dbGetQuery(conn, que))
    row.names(dfTemp) = dfTemp$tgid
  }
  
  p4s <- CRS(paste0("+init=epsg:", srid$st_srid))@projargs
  tt <- mapply(function(x, y, z) readWKT(x, y, z), x = dfTemp[,2]
               , y = dfTemp[, 1], z = p4s)
  
  Sline <- SpatialLines(lapply(1:length(tt), function(i) {
    lin <- slot(tt[[i]], "lines")[[1]]
    slot(lin, "ID") <- slot(slot(tt[[i]], "lines")[[1]], 
                            "ID")  ##assign original ID to Line
    lin
  }))
  
  Sline@proj4string <- slot(tt[[1]], "proj4string")
  
  if (is.null(other.cols)) {
    return(Sline)
  } else {
    try(dfTemp[geom] <- NULL)
    try(dfTemp["wkt"] <- NULL)
    spdf <- SpatialLinesDataFrame(Sline, dfTemp)
    spdf@data["tgid"] <- NULL
    return(spdf)
  }
} 