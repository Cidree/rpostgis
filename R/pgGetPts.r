## pgGetPts
##' Retrieve point geometries from a PostGIS table, and convert it to
##' a SpatialPoints or a SpatialPointsDataFrame.
##'
##' @title Retrieve point geometries
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table, view
##' or schema name.
##' @param geom The name of the point geometry column. (Default = 'geom')
##' @param gid Name of the column in 'table' holding the ID. Should be unique
##' if additional columns of unique data are being appended. \code{gid=NULL} 
##' (default) automatically creates a new unique ID for each row in the table.
##' @param other.cols Names of columns in the table to retrieve, comma seperated
##' in one character element (e.g. \code{other.cols='col1,col2'}. Default is to
##' attach all columns in a SpatialPointsDataFrame, \code{other.cols=NULL] 
##' returns a SpatialPoints.}
##' @return A Spatial(Multi)Points or a Spatial(Multi)PointsDataFrame
##' @author David Bucklin \email{david.bucklin@gmail.com}
##' @export
##' @examples
##' \dontrun{
##' ## Retrieve a SpatialPointsDataFrame with all data from table 'fla.bli',
##' with geometry in the column 'geom'
##' pgGetPts(conn, c('fla', 'bli'))
##' ## Return a SpatialPointsDataFrame with columns c1 & c2 as data
##' pgGetPts(conn, c('fla', 'bli'), other.cols = 'c1,c2')
##' ## Return a SpatialPoints, retaining id from table as rownames
##' pgGetPts(conn, c('fla', 'bli'), gid = 'bli_id', other.cols = FALSE)
##' }

pgGetPts <- function(conn, name, geom = "geom", gid = NULL, other.cols = "*", 
                     query = NULL) 
{
  ## Check and prepare the schema.name
  if (length(name) %in% 1:2) {
    table <- paste(name, collapse = ".")
  } else {
    stop("The table name should be \"table\" or c(\"schema\", \"table\").")
  }
  
  ## if ID not specified, set it to generate row numbers
  if (is.null(gid)) {
    gid <- "row_number() over()"
  }
  
  ## Check if MULTI or single geom
  str <- paste0("SELECT DISTINCT ST_GeometryType(", geom, ") AS type FROM ", 
                table, " WHERE ", geom, " IS NOT NULL;")
  typ <- dbGetQuery(conn, str)
  
  ## Retrieve the SRID
  str <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ", table, 
                " WHERE ", geom, " IS NOT NULL;")
  srid <- dbGetQuery(conn, str)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) != 1) 
    stop("Multiple SRIDs in the point geometry")
  
  # make spatialpoints* for single geom types
  if (length(typ$type) == 1 && typ$type == "ST_Point") {
    
    # get data
    if (is.null(other.cols)) {
      str <- paste0("select ", gid, " as tgid,ST_X(", geom, ") AS x, ST_Y(", 
                    geom, ") AS y from ", table, " where ", geom, " is not null ", 
                    query, ";")
    } else {
      str <- paste0("select ", gid, " as tgid,ST_X(", geom, ") AS x, ST_Y(", 
                    geom, ") AS y,", other.cols, " from ", table, " where ", 
                    geom, " is not null ", query, ";")
    }
    dbData <- suppressWarnings(dbGetQuery(conn, str))
    row.names(dbData) = dbData$tgid
    
    ## Generate a SpatialPoints object
    sp <- SpatialPoints(data.frame(x = dbData$x, y = dbData$y, row.names = dbData$tgid), 
                        proj4string = CRS(paste0("+init=epsg:", srid$st_srid)))
    
    ## Append data to spdf if requested
    if (!is.null(other.cols)) {
      cols <- colnames(dbData)
      cols <- cols[!(cols %in% c("tgid", "x", "y", geom))]
      sp <- SpatialPointsDataFrame(sp, dbData[cols], match.ID = TRUE)
    }
  } else {
    
    ## make SpatialMultiPoints(Dataframe) for multi-point types
    
    if (is.null(other.cols)) {
      str <- paste0("select ", gid, " as tgid,st_astext(", geom, 
                    ") as wkt from ", table, " where ", geom, " is not null ", 
                    query, ";")
    } else {
      str <- paste0("select ", gid, " as tgid,st_astext(", geom, 
                    ") as wkt,", other.cols, " from ", table, " where ", geom, 
                    " is not null ", query, ";")
    }
    
    dbData <- suppressWarnings(dbGetQuery(conn, str))
    row.names(dbData) = dbData$tgid
    
    # create spatialMultiPoints
    tt <- mapply(function(x, y, z) readWKT(x, y, z), x = dbData$wkt, 
                 y = dbData$tgid, z = CRS(paste0("+init=epsg:", srid$st_srid))@projargs)
    sp <- SpatialMultiPoints(tt, proj4string = CRS(paste0("+init=epsg:", 
                                                          srid$st_srid)))
    
    ## Append data to spdf if requested
    if (!is.null(other.cols)) {
      cols <- colnames(dbData)
      cols <- cols[!(cols %in% c("tgid", "wkt", geom))]
      sp <- SpatialMultiPointsDataFrame(tt, dbData[cols], proj4string = CRS(paste0("+init=epsg:", 
                                                                                   srid$st_srid)))
    }
  }
  
  return(sp)
}