# pgGetGeom

##' Load a PostGIS geometry from a PostgreSQL table/view/query into R.
##'
##' Retrieve geometries from a PostGIS table/view/query, and convert 
##' it to an R \code{sf} object.
##' 
##' The features of the table to retrieve must have the same geometry type. 
##' The query mode version of \code{pgGetGeom} allows the user to enter a
##' complete SQL query (\code{query}) that returns a Geometry column, and save 
##' the query as a new view (\code{name}) if desired. If (\code{name}) is not
##' specified, a temporary view with name ".rpostgis_TEMPview" is used only
##' within the function execution. In this mode, the other arguments can be used 
##' normally to modify the Spatial* object returned from the query.
##' 
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., \code{name =
##'     c("schema","table")})
##' @param geom The name of the geometry/(geography) column. (Default = \code{"geom"})
##' @param gid Name of the column in \code{name} holding the IDs. Should be
##'     unique for each record to return. \code{gid=NULL} (default) automatically
##'     creates a new unique ID for each row in the \code{sf} object.
##' @param other.cols Names of specific columns in the table to
##'     retrieve, in a character vector (e.g. \code{other.cols.=c("col1","col2")}.)
##'     The default (\code{other.cols = TRUE}) is to attach
##'     all columns. Setting \code{other.cols=FALSE} will return a Spatial-only 
##'     object without attributes (no data frame).
##' @param clauses character, additional SQL to append to modify select
##'     query from table. Must begin with an SQL clause (e.g., "WHERE ...",
##'     "ORDER BY ...", "LIMIT ..."); see below for examples.
##' @param boundary \code{sf}, \code{SpatVector} or \code{sp} object; or numeric. 
##'     If a spatial object is provided, its bounding box will be used to select
##'     geometries to import. Alternatively, a numeric vector (\code{c([top], 
##'     [bottom], [right], [left])}) indicating the projection-specific limits with
##'      which to subset the spatial data. If not value is provided, the default
##'     \code{boundary = NULL} will not apply any boundary subset.
##' @param query character, a full SQL query including a geometry column. 
##'     For use with query mode only (see details).
##' @param returnclass 'sf' by default; 'terra' for \code{SpatVector}; 
##'     or 'sp' for \code{sp} objects.
##' @return sf, SpatVector or sp object
##' @importFrom sf st_crs st_as_sf
##' @export
##' @author David Bucklin \email{david.bucklin@@gmail.com}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @author Adrián Cidre González \email{adrian.cidre@@gmail.com}
##' 
##' 
##' @examples
##' \dontrun{
##' ## Retrieve a sf with all data from table
##' ## 'schema.tablename', with geometry in the column 'geom'
##' pgGetGeom(conn, c("schema", "tablename"))
##' ## Return a sf with columns c1 & c2 as data
##' pgGetGeom(conn, c("schema", "tablename"), other.cols = c("c1","c2"))
##' ## Return a spatial-only (no data frame), 
##' ## retaining id from table as rownames
##' pgGetGeom(conn, c("schema", "tablename"), gid = "table_id",
##'   other.cols = FALSE)
##' ## Return a spatial-only (no data frame), 
##' ## retaining id from table as rownames and with a subset of the data
##' pgGetGeom(conn, c("schema", "roads"), geom = "roadgeom", gid = "road_ID",
##'     other.cols = FALSE, clauses  = "WHERE road_type = 'highway'")
##' ## Query mode
##' pgGetGeom(conn, query = "SELECT r.gid as id, ST_Buffer(r.geom, 100) as geom 
##'                            FROM
##'                              schema.roads r,
##'                              schema.adm_boundaries b
##'                            WHERE 
##'                              ST_Intersects(r.geom, b.geom);")
##' }

pgGetGeom <- function(conn, name, geom = "geom", gid = NULL, 
         other.cols = TRUE, clauses = NULL, boundary = NULL, query = NULL,
         returnclass = "sf") {

  
  ## Check connection and PostGIS extension
  dbConnCheck(conn)
  if (!suppressMessages(pgPostGIS(conn))) {
    stop("PostGIS is not enabled on this database.")
  }
  
  ## If a Query is provided, execute it and return the value
  ## Using pgGetGeomQ
  if (!is.null(query)) {
    if (missing(name)) name <- NULL
    ret <- pgGetGeomQ(conn, query = query, name = name, geom = geom, gid = gid, 
                      other.cols = other.cols, clauses = clauses, boundary = boundary)
    if (is.null(ret)) stop("Query retrieval failed.", call. = FALSE) else return(ret)
  }
  
  ## Check and prepare the schema.name
  name1   <- dbTableNameFix(conn,name)
  nameque <- paste(name1, collapse = ".")
  geomque <- pgCheckGeom(conn, name, geom)
  
  ## Get boundary clause if specified
  if (!is.null(boundary)) {
    ## get srid for boundary box - could be incorrect only if clauses are used to specify a subset by SRID
    srid <- pgGetSRID(conn, name, geom)
    if (typeof(boundary) != "double") {
      ## Manage sp and terra object
      if (!inherits(boundary, "sf")) boundary <- sf::st_as_sf(boundary)
      boundary <- sf::st_bbox(boundary)
      ## Standardize to top-bottom-right-left
      boundary <-  c(boundary[4], boundary[2], boundary[3], boundary[1])
    }
    b.clause <- paste0(" AND ST_Intersects(",
                       geomque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
                       " ", boundary[1], ",", boundary[4], " ", boundary[2],
                       ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
                       " ", boundary[1], ",", boundary[4], " ", boundary[1],
                       "))'),", srid[1], "))")
  } else {
    b.clause <- NULL
  }
  
  ## Prepare clauses (boundary + user clauses)
  if (!is.null(clauses) | !is.null(b.clause)) {
    clauses <- paste(b.clause, sub("^where", "AND", sub(";$","", sub("\\s+$","",clauses)),
                                   ignore.case = TRUE),collapse = "\n") 
  } else {
    if (".db_pkid" %in% dbTableInfo(conn,name)$column_name) {
      clauses <- "ORDER BY \".db_pkid\""
    }
  }
  
  ## Prepare other.cols. If its not logical, user set cols
  if (!is.logical(other.cols)) {
    cols <- paste(DBI::dbQuoteIdentifier(conn, ), 
                  collapse = ",")
  } else {
    ## Else -> TRUE all columns, FALSE only geometry
    if (other.cols & sum(!dbTableInfo(conn,
                                name)$column_name %in% c(".R_rownames",".db_pkid", gid, geom)) > 0) {
      cols <- "*"
    } else {
      cols <- NULL
    }
  }
  
  ## Check PostGIS table geometry type
  tmp.query <- paste0("SELECT DISTINCT a.geo AS type 
                        FROM (SELECT ST_GeometryType(", geomque, 
                      ") as geo FROM ", nameque, " WHERE ", geomque, " IS NOT NULL ", 
                      clauses, ") a;")
  typ <- dbGetQuery(conn, tmp.query)$type
  
  ## Get the geometry object if length typ = 1 (1 geometry type)
  if (length(typ) == 0) {
    stop("No geometries found.")
  } else if (length(typ) >= 1) {
    ## Inform the user about geometry types
    if (length(typ) == 1) {
      message(paste0("Returning PostGIS ", typ, " object into sf object with ",
                     toupper(sub("...", "", typ)), " geometry type"))
    } else {
      message(paste0("Returning PostGIS object into sf object with multiple geometry types"))
    }
    
    ## Prepare additional clauses
    clauses <- sub("^where", "AND", clauses, ignore.case = TRUE)
    
    ## If ID not specified, set it to generate row numbers
    if (is.null(gid)) {
      if (".R_rownames" %in% dbTableInfo(conn,name)$column_name) {
        gid <- DBI::dbQuoteIdentifier(conn, ".R_rownames")
      } else {
        gid <- "row_number() over()"
      }
    } else {
      gid <- DBI::dbQuoteIdentifier(conn, gid)
    }
    
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT a.s as st_srid FROM
                        (SELECT ST_SRID(", geomque, ") as s FROM ",
                        nameque, " WHERE ", geomque, " IS NOT NULL ", clauses , ") a;")
    srid <- dbGetQuery(conn, tmp.query)
    
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) > 1) {
      stop("Multiple SRIDs in the point geometry")
    } else if (nrow(srid) < 1) {
      stop("Database table is empty.")
    }
    
    ## Get SRID
    proj4 <- sf::st_crs(as.character(NA))
    tmp.query <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
                        srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query)$p4s
    if (!is.null(db.proj4)) {
      try(proj4 <- sf::st_crs(db.proj4), silent = TRUE)
    }
    
    ## If SRID was not found, warn about it
    if (is.na(proj4$input)) {
      warning("Table SRID not found. Projection will be undefined (NA)")
    }
    
    ## Query the table
    if (is.null(cols)) {
      tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
                          geomque, ") AS wkt FROM ", nameque, " WHERE ", geomque,
                          " IS NOT NULL ", clauses , ";")
    } else {
      tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
                          geomque, ") AS wkt,", cols, " FROM ", nameque,
                          " WHERE ", geomque, " IS NOT NULL ", clauses , ";")
    }
    
    ## Read the table in R
    dbData <- suppressWarnings(dbGetQuery(conn, tmp.query))
    dbData <- data.frame(dbData, row.names = "tgid")
    
    # Fix columns
    if (!is.null(cols)) {
      cols <- colnames(dbData)
      cols <- cols[!(cols %in% c("wkt", geom))]
      dfr  <- dbData[cols]
      if (gid == "\".R_rownames\"") suppressMessages(dfr <- dbReadDataFrame(conn, name, df = dfr))
      cname      <- c(names(dfr), geom)
      dfr        <- cbind(dfr, dbData[,"wkt"])
      names(dfr) <- cname
      
    } else { 
      if (geom %in% names(dbData)) try(dbData <- dbData[, -which(names(dbData) == geom)], silent = TRUE)
      names(dbData)[names(dbData) == "wkt"] <- "geom"
      dfr <- dbData
    }
    
    ## Create sf object
    sp <- sf::st_as_sf(dfr, wkt = "geom", crs = proj4$input)
    
  }
  
  # Return class
  if (returnclass == "sf") {
    return(sp)
  } else if (returnclass == "sp") {
    return(sf::as_Spatial(sp)) 
  } else if (returnclass == "terra") {
    return(terra::vect(sp))
  } else {
    stop("returnclass must be 'sf' or 'sp'")
  }
  
}


# pgGetGeomQ

#' Load geometries from a full query and return a Spatial* object
#' 
#' @param query character, a full SQL query including a geometry column. 
#' @param name optional character string specifying
#'     a PostgreSQL schema and view name (e.g., \code{name = c("schema","view")}) 
#'     to save the query as. If NULL, a temporary view ".rpostgis_TEMPview" is used
#'     temporarily (only within the function scope).
#' @param ... For \code{pgGetGeomQ}, other arguments as in \code{pgGetGeom}
#' @keywords internal

pgGetGeomQ <- function(conn, query, name = NULL, ...) {
    # set view name
    if (is.null(name)) {
        name <- dbTableNameFix(conn, ".rpostgis_TEMPview", as.identifier = FALSE)
        keep <- FALSE
    } else {
        name <- dbTableNameFix(conn, name , as.identifier = FALSE)
        keep <- TRUE
    }
    dbExecute(conn, "BEGIN;")
    # try to create view and retrieve geometries
    geo <- NULL
    try({
        prequery <- paste0("CREATE OR REPLACE VIEW ", paste(dbQuoteIdentifier(conn, 
            name), collapse = "."), " AS ")
        if (sub(".*(?=.$)", "", sub("\\s+$", "", query), perl = TRUE) == 
            ";") {
            post <- NULL
        } else {
            post <- ";"
        }
        q <- paste0(prequery, query, post)
        dbExecute(conn, q)
        geo <- pgGetGeom(conn, name = name, ...)
        
    })
    # rollback on failed/not saving view, else commit
    if (is.null(geo)) {
        dbExecute(conn, "ROLLBACK;")
    } else {
        if (!keep) { dbExecute(conn, "ROLLBACK;") } else {
          dbExecute(conn, "COMMIT;")
          message(paste0("Created view ",paste(dbQuoteIdentifier(conn, 
            name), collapse = "."),"."))
        }
    }
    return(geo)
}
