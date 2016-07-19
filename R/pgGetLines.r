# pgGetLines
#' Retrieve line geometries from a PostGIS table, and convert it to
#' a SpatialLines or a SpatialLinesDataFrame.
#
#' @title Load a linestring geometry stored in a PostgreSQL database into R.
#'
#' @param conn A connection object to a PostgreSQL database
#' @param name A character string specifying a PostgreSQL schema (if necessary), 
#' and table or view name for the table holding the lines 
#' geometry (e.g., name = c("schema","table"))
#' @param geom character, Name of the column in 'name' holding
#' the geometry object (Default = 'geom')
#' @param gid character, Name of the column in 'name' holding 
#' the ID for each line. Should be unique if additional columns of 
#' unique data are being appended. \code{gid=NULL} 
#' (default) automatically creates a new unique ID for each row in the table.
#' @param other.cols character, names of additional columns from 
#' table (comma-seperated) to append to dataset (Default is all 
#' columns, NULL returns a SpatialLines object)
#' @param query character, additional SQL to append to modify 
#' select query from table
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @importFrom sp CRS
#' @importFrom sp SpatialLines
#' @importFrom sp SpatialLinesDataFrame
#' @importFrom rgeos readWKT 
#' @importFrom methods slot
#' @export
#' @return SpatialLinesDataFrame or SpatialLines
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' drv<-dbDriver("PostgreSQL")
#' conn<-dbConnect(drv,dbname='dbname',host='host',port='5432',
#'                user='user',password='password')
#'
#' pgGetLines(conn,c('schema','tablename'))
#' pgGetLines(conn,c('schema','roads'),geom='roadgeom',gid='road_ID',
#'            other.cols=NULL, query = "AND field = \'highway\'")
#' }

pgGetLines <- function(conn, name, geom = "geom", gid = NULL, 
                       other.cols = "*", query = NULL) {
  
  ## Check and prepare the schema.name
  if (length(name) %in% 1:2) {
    name <- paste(name, collapse = ".")
  } else {
    stop("The table name should be \"table\" or c(\"schema\", \"table\").")
  }
  
  #check table exists
  tab.list<-dbGetQuery(conn,"select (f_table_schema||'.'||f_table_name) as geo_tables from public.geometry_columns;")
  if (!name %in% tab.list$geo_tables)
  {stop(paste0("Table/view '",name,"' does not exist, or does not have geometry column (not listed in public.geometry_columns."))}
  
  ## Retrieve the SRID
  temp.query <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ", 
                name, " WHERE ", geom, " IS NOT NULL ", 
                query, ";")
  srid <- dbGetQuery(conn, temp.query)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) != 1) {
    stop("Multiple SRIDs in the linestring geometry")}
  
  p4s<-sp::CRS(as.character(NA))@projargs
  try(p4s<-sp::CRS(paste0("+init=epsg:", srid$st_srid))@projargs,silent=TRUE)
  
  if (is.na(p4s)) {warning("Table SRID not found. Projection will be undefined (NA)")}
  
  #check gid
  if (is.null(gid)) {
    gid <- "row_number() over()"
  }
  
  #check other.cols
  if (is.null(other.cols)) {
    temp.query <- paste0("select ", gid, " as tgid,st_astext(", 
                  geom, ") as wkt from ", name, " where ", geom, " is not null ", 
                  query, ";")
    dfTemp <- suppressWarnings(dbGetQuery(conn, temp.query))
    row.names(dfTemp) = dfTemp$tgid
  } else {
    temp.query <- paste0("select ", gid, " as tgid,st_astext(", 
                  geom, ") as wkt,", other.cols, " from ", name, " where ", 
                  geom, " is not null ", query, ";")
    dfTemp <- suppressWarnings(dbGetQuery(conn, temp.query))
    row.names(dfTemp) = dfTemp$tgid
  }

  #make spatiallines
  tt <- mapply(function(x, y, z) rgeos::readWKT(x, y, z), x = dfTemp[,2]
               , y = dfTemp[, 1], z = p4s)
  
  Sline <- sp::SpatialLines(lapply(1:length(tt), function(i) {
    lin <- methods::slot(tt[[i]], "lines")[[1]]
    methods::slot(lin, "ID") <- methods::slot(methods::slot(tt[[i]], "lines")[[1]], 
                            "ID")  ##assign original ID to Line
    lin
  }))
  
  Sline@proj4string <- methods::slot(tt[[1]], "proj4string")
  
  if (is.null(other.cols)) {
    return(Sline)
  } else {
    try(dfTemp[geom] <- NULL)
    try(dfTemp["wkt"] <- NULL)
    spdf <- sp::SpatialLinesDataFrame(Sline, dfTemp)
    spdf@data["tgid"] <- NULL
    return(spdf)
  }
} 