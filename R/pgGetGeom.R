# pgGetGeom

##' Load a PostGIS geometry from a PostgreSQL table/view/query into R.
##'
##' Retrieve point, linestring, or polygon geometries from a PostGIS
##' table/view/query, and convert it to an R \code{sp} object (\code{Spatial*} or
##' \code{Spatial*DataFrame}).
##' 
##' The query mode version of \code{pgGetGeom} allows the user to enter a
##' complete SQL query (\code{query}) that returns a Geometry column, and save the query
##' as a new view (\code{name}) if desired. If (\code{name}) is not specified,
##' a temporary view with name ".rpostgis_TEMPview" is used only
##' within the function execution. In this mode, the other arguments can be used 
##' normally to modify the Spatial* object returned from the query.
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., \code{name =
##'     c("schema","table")})
##' @param geom The name of the geometry column. (Default = \code{"geom"})
##' @param gid Name of the column in \code{name} holding the IDs. Should be
##'     unique if additional columns of unique data are being
##'     appended. \code{gid=NULL} (default) automatically creates a
##'     new unique ID for each row in the \code{sp} object.
##' @param other.cols Names of specific columns in the table to
##'     retrieve, in a character vector (e.g. \code{other.cols=c("col1","col2")}.)
##'     The default (\code{other.cols = TRUE}) is to attach
##'     all columns in a Spatial*DataFrame. Setting
##'     \code{other.cols=FALSE} will return a Spatial-only object (no
##'     data frame).
##' @param clauses character, additional SQL to append to modify select
##'     query from table. Must begin with an SQL clause (e.g., "WHERE ...",
##'     "ORDER BY ...", "LIMIT ..."); see below for examples.
##' @param query character, a full SQL query including a geometry column. 
##'     For use with query mode only (see details).
##' @return sp-class (SpatialPoints*, SpatialMultiPoints*, SpatialLines*, or SpatialPolygons*)
##' @export
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' 
##' @examples
##' \dontrun{
##' ## Retrieve a Spatial*DataFrame with all data from table
##' ## 'schema.tablename', with geometry in the column 'geom'
##' pgGetGeom(conn, c("schema", "tablename"))
##' ## Return a Spatial*DataFrame with columns c1 & c2 as data
##' pgGetGeom(conn, c("schema", "tablename"), other.cols = c("c1","c2"))
##' ## Return a Spatial*-only (no data frame), 
##' ## retaining id from table as rownames
##' pgGetGeom(conn, c("schema", "tablename"), gid = "table_id",
##'   other.cols = FALSE)
##' ## Return a Spatial*-only (no data frame), 
##' ## retaining id from table as rownames and with a subset of the data
##' pgGetGeom(conn, c("schema", "roads"), geom = "roadgeom", gid = "road_ID",
##'     other.cols = FALSE, clauses  = "WHERE field = 'highway'")
##' ## Query mode
##' pgGetGeom(conn, query = "SELECT r.gid as id, ST_Buffer(r.geom, 100) as geom 
##'                            FROM
##'                              schema.roads r,
##'                              schema.adm_boundaries b
##'                            WHERE 
##'                              ST_Intersects(r.geom, b.geom);")
##' }

pgGetGeom <- function(conn, name, geom = "geom", gid = NULL, 
    other.cols = TRUE, clauses = NULL, query = NULL) {
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    if (!is.null(query)) {
      if (missing(name)) name <- NULL
      ret <- pgGetGeomQ(conn, query, name = name, geom = geom, gid = gid, 
                        other.cols = other.cols, clauses = clauses)
      if (is.null(ret)) stop("Query retrieval failed.", call. = FALSE) else return(ret)
    }
    ## Check and prepare the schema.name
    nameque <- paste(dbTableNameFix(conn,name), collapse = ".")
    namechar <- gsub("\"\"", "\"", gsub("'", "''", paste(gsub("^\"|\"$", 
        "", dbTableNameFix(conn,name)), collapse = ".")))
    
    geomque <- pgCheckGeom(conn, namechar, geom)
    
    ## prepare clauses
    if (!is.null(clauses)) {
      clauses <- sub("^where", "AND", clauses, ignore.case = TRUE) 
      } else {
        if (".db_pkid" %in% dbTableInfo(conn,name)$column_name) {
          clauses <- "ORDER BY \".db_pkid\""
        }
      }
    ## prepare other.cols
    if (!is.logical(other.cols)) {
        other.cols <- paste(DBI::dbQuoteIdentifier(conn, other.cols), 
            collapse = ",")
    } else {
        if (other.cols) {
            other.cols <- "*"
        } else {
            other.cols <- NULL
        }
    }
    ## check type
    tmp.query <- paste0("SELECT DISTINCT a.geo AS type 
                        FROM (SELECT ST_GeometryType(", geomque, 
                        ") as geo FROM ", nameque, " WHERE ", geomque, " IS NOT NULL ", 
                        clauses, ") a;")
    typ <- dbGetQuery(conn, tmp.query)$type
    # assign to correct function
    if (length(typ) == 0) {
        stop("No geometries found.")
    } else if (length(typ) == 1) {
        if (typ %in% c("ST_Point", "ST_MultiPoint")) {
            ret <- pgGetPts(conn, name, geom, gid, other.cols, 
                clauses)
            message(paste0("Returning ", sub("...", "", typ), 
                " types in SpatialPoints*-class."))
            return(ret)
        } else if (typ %in% c("ST_LineString", "ST_MultiLineString")) {
            ret <- pgGetLines(conn, name, geom, gid, other.cols, 
                clauses)
            message(paste0("Returning ", sub("...", "", typ), 
                " types in SpatialLines*-class."))
            return(ret)
        } else if (typ %in% c("ST_Polygon", "ST_MultiPolygon")) {
            ret <- pgGetPolys(conn, name, geom, gid, other.cols, 
                clauses)
            message(paste0("Returning ", sub("...", "", typ), 
                " types in SpatialPolygons*-class."))
            return(ret)
        } else {
            stop(paste0("Geometry type ", typ, " not supported."))
        }
    } else {
        stop(paste0("Multiple geometry types found: (", paste(typ, 
            collapse = ", "), "). Use
                  \"clauses\" to modify query to select only one geometry type."))
    }
}

## pgGetPts

##' Load a PostGIS point geometry from a PostgreSQL table/view into R.
##' @return Spatial(Multi)PointsDataFrame or Spatial(Multi)Points
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @importFrom sp CRS
##' @importFrom sp SpatialPoints
##' @importFrom sp SpatialPointsDataFrame
##' @importFrom sp SpatialMultiPoints
##' @importFrom sp SpatialMultiPointsDataFrame
##' @importFrom rgeos readWKT
##' @keywords internal
##' @examples
##' \dontrun{
##' ## Retrieve a SpatialPointsDataFrame with all data from table
##' ## 'schema.tablename', with geometry in the column 'geom'
##' pgGetPts(conn, c("schema", "tablename"))
##' ## Return a SpatialPointsDataFrame with columns c1 & c2 as data
##' pgGetPts(conn, c("schema", "tablename"), other.cols = "c1,c2")
##' ## Return a SpatialPoints, retaining id from table as rownames
##' pgGetPts(conn, c("schema", "tablename"), gid = "table_id", other.cols = FALSE)
##' }

pgGetPts <- function(conn, name, geom = "geom", gid = NULL, other.cols = "*",
    clauses = NULL) {
    if (!suppressMessages(pgPostGIS(conn))) {
      stop("PostGIS is not enabled on this database.")
    }
    ## Check and prepare the schema.name
    name1 <- dbTableNameFix(conn,name)
    nameque <- paste(name1, collapse = ".")
    ## prepare additional clauses
    clauses<-sub("^where", "AND",clauses, ignore.case = TRUE)
    ## prepare geom column
    namechar <- gsub("\"\"", "\"", gsub("'", "''", paste(gsub("^\"|\"$", 
        "", dbTableNameFix(conn,name)), collapse = ".")))
    geomque <- pgCheckGeom(conn, namechar, geom)
    ## If ID not specified, set it to generate row numbers
    if (is.null(gid)) {
        if (".R_rownames" %in% dbTableInfo(conn,name)$column_name) {
          gid <- DBI::dbQuoteIdentifier(conn,".R_rownames")
        } else {
          gid <- "row_number() over()"
        }
    } else {
      gid<-DBI::dbQuoteIdentifier(conn,gid)
    }
    ## Check if MULTI or single geom
    tmp.query <- paste0("SELECT DISTINCT a.geo AS type 
                        FROM (SELECT ST_GeometryType(", geomque, 
                        ") as geo FROM ", nameque, " WHERE ", geomque, " IS NOT NULL ", 
                        clauses, ") a;")
    typ <- dbGetQuery(conn, tmp.query)
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
    proj4 <- sp::CRS(as.character(NA))
    tmp.query <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
        srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query)$p4s
    if (!is.null(db.proj4)) {
        try(proj4 <- sp::CRS(db.proj4), silent = TRUE)
    }
    if (is.na(proj4@projargs)) {
        warning("Table SRID not found. Projection will be undefined (NA)")
    }
    ## Make spatialpoints* for single geom types
    if (length(typ$type) == 1 && typ$type == "ST_Point") {
        ## Get data
        if (is.null(other.cols)) {
            tmp.query <- paste0("SELECT ", gid, " AS tgid,ST_X(",
                geomque, ") AS x, ST_Y(", geomque, ") AS y FROM ",
                nameque, " WHERE ", geomque, " IS NOT NULL ", clauses ,
                ";")
        } else {
            tmp.query <- paste0("SELECT ", gid, " AS tgid,ST_X(",
                geomque, ") AS x, ST_Y(", geomque, ") AS y,", other.cols,
                " FROM ", nameque, " WHERE ", geomque, " IS NOT NULL ",
                clauses , ";")
        }
        dbData <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dbData) <- dbData$tgid
        ## Generate a SpatialPoints object
        sp <- sp::SpatialPoints(data.frame(x = dbData$x, y = dbData$y,
            row.names = dbData$tgid), proj4string = proj4)
        ## Append data to spdf if requested
        if (!is.null(other.cols)) {
            cols <- colnames(dbData)[4:length(colnames(dbData))]
            cols <- cols[!(cols %in% c(geom))]
            # column definitions
            suppressMessages(
              dfr<-dbReadDataFrame(conn, name, df = dbData[cols])
            )
            sp <- sp::SpatialPointsDataFrame(sp, dfr,
                match.ID = TRUE)
        }
    } else {
        ## Make SpatialMultiPoints(Dataframe) for multi-point types
        if (is.null(other.cols)) {
            tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
                geomque, ") AS wkt FROM ", nameque, " WHERE ", geomque,
                " IS NOT NULL ", clauses , ";")
        } else {
            tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
                geomque, ") AS wkt,", other.cols, " FROM ", nameque,
                " WHERE ", geomque, " IS NOT NULL ", clauses , ";")
        }
        dbData <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dbData) <- dbData$tgid
        ## Create spatialMultiPoints
        tt <- mapply(function(x, y, z) rgeos::readWKT(x, y, z),
            x = dbData$wkt, y = dbData$tgid, z = proj4@projargs)
        sp <- sp::SpatialMultiPoints(tt, proj4string = proj4)
        ## Append data to spdf if requested
        if (!is.null(other.cols)) {
            cols <- colnames(dbData)[3:length(colnames(dbData))]
            cols <- cols[!(cols %in% c(geom))]
            # column definitions
            suppressMessages(
              dfr<-dbReadDataFrame(conn, name, df = dbData[cols])
            )
            sp <- sp::SpatialMultiPointsDataFrame(tt, dfr,
                proj4string = proj4)
        }
    }
    return(sp)
}


## pgGetLines

##' Load a PostGIS linestring geometry from a PostgreSQL table/view into R.
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @importFrom sp CRS
##' @importFrom sp SpatialLines
##' @importFrom sp SpatialLinesDataFrame
##' @importFrom rgeos readWKT
##' @importFrom methods slot
##' @keywords internal
##' @return SpatialLinesDataFrame or SpatialLines
##' @examples
##' \dontrun{
##' pgGetLines(conn, c("schema", "tablename"))
##' pgGetLines(conn, c("schema", "roads"), geom = "roadgeom", gid = "road_ID",
##'     other.cols = NULL, clauses  = "WHERE field = 'highway'")
##' }

pgGetLines <- function(conn, name, geom = "geom", gid = NULL,
    other.cols = "*", clauses  = NULL) {
    if (!suppressMessages(pgPostGIS(conn))) {
      stop("PostGIS is not enabled on this database.")
    }
    ## Check and prepare the schema.name
    name1 <- dbTableNameFix(conn,name)
    nameque <- paste(name1, collapse = ".")

    ## prepare additional clauses
    clauses<-sub("^where", "AND",clauses, ignore.case = TRUE)
    
    ## prepare geom column
    namechar <- gsub("\"\"", "\"", gsub("'", "''", paste(gsub("^\"|\"$", 
        "", dbTableNameFix(conn,name)), collapse = ".")))
    geomque <- pgCheckGeom(conn, namechar, geom)
    ## Check gid
    if (is.null(gid)) {
        if (".R_rownames" %in% dbTableInfo(conn,name)$column_name) {
          gid <- DBI::dbQuoteIdentifier(conn,".R_rownames")
        } else {
          gid <- "row_number() over()"
        }
    } else {
      gid<-DBI::dbQuoteIdentifier(conn,gid)
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT a.s as st_srid FROM
                        (SELECT ST_SRID(", geomque, ") as s FROM ",
                        nameque, " WHERE ", geomque, " IS NOT NULL ", clauses , ") a;")
    srid <- dbGetQuery(conn, tmp.query)
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) > 1) {
        stop("Multiple SRIDs in the linestring geometry.")
    } else if (nrow(srid) < 1) {
        stop("Database table is empty.")
    }
    p4s <- sp::CRS(as.character(NA))@projargs
    tmp.query <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
        srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query)$p4s
    if (!is.null(db.proj4)) {
        try(p4s <- sp::CRS(db.proj4)@projargs, silent = TRUE)
    }
    if (is.na(p4s)) {
        warning("Table SRID not found. Projection will be undefined (NA)")
    }
    ## Check other.cols
    if (is.null(other.cols)) {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geomque, ") AS wkt FROM ", nameque, " WHERE ", geomque,
            " IS NOT NULL ", clauses , ";")
        dfTemp <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dfTemp) <- dfTemp$tgid
    } else {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geomque, ") AS wkt,", other.cols, " FROM ", nameque,
            " WHERE ", geomque, " IS NOT NULL ", clauses , ";")
        dfTemp <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dfTemp) <- dfTemp$tgid
    }
    ## Make SpatialLines
    tt <- mapply(function(x, y, z) rgeos::readWKT(x, y, z), x = dfTemp[,
        2], y = dfTemp[, 1], z = p4s)
    Sline <- sp::SpatialLines(lapply(1:length(tt), function(i) {
        lin <- methods::slot(tt[[i]], "lines")[[1]]
        ## Assign original ID to Line
        methods::slot(lin, "ID") <- methods::slot(methods::slot(tt[[i]],
            "lines")[[1]], "ID")
        lin
    }))
    Sline@proj4string <- methods::slot(tt[[1]], "proj4string")
    if (is.null(other.cols)) {
        return(Sline)
    } else {
        cols <- colnames(dfTemp)[3:length(colnames(dfTemp))]
        cols <- cols[!(cols %in% c(geom))]
        # column definitions
        suppressMessages(
            dfr<-dbReadDataFrame(conn, name, df = dfTemp[cols])
        )
        
        spdf <- sp::SpatialLinesDataFrame(Sline, dfr)
        return(spdf)
    }
}

## pgGetPolys

##' Load a PostGIS polygon geometry from a PostgreSQL table/view into R.
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @importFrom sp CRS
##' @importFrom sp SpatialPolygons
##' @importFrom sp SpatialPolygonsDataFrame
##' @importFrom rgeos readWKT
##' @importFrom methods slot
##' @keywords internal
##' @return SpatialPolygonsDataFrame or SpatialPolygons
##' @examples
##' \dontrun{
##' pgGetPolys(conn, c("schema", "tablename"))
##' pgGetPolys(conn, c("schema", "states"), geom = "statesgeom",
##'     gid = "state_ID", other.cols = "area,population",
##'     clauses  = "WHERE area > 1000000 ORDER BY population LIMIT 10")
##' }

pgGetPolys <- function(conn, name, geom = "geom", gid = NULL,
    other.cols = "*", clauses  = NULL) {
    if (!suppressMessages(pgPostGIS(conn))) {
      stop("PostGIS is not enabled on this database.")
    }
    ## Check and prepare the schema.name
    name1 <- dbTableNameFix(conn,name)
    nameque <- paste(name1, collapse = ".")
    ## prepare additional clauses
    clauses<-sub("^where", "AND",clauses, ignore.case = TRUE)
    ## prepare geom column
    namechar <- gsub("\"\"", "\"", gsub("'", "''", paste(gsub("^\"|\"$", 
        "", dbTableNameFix(conn,name)), collapse = ".")))
    geomque <- pgCheckGeom(conn, namechar, geom)
    ## Check gid
    if (is.null(gid)) {
        if (".R_rownames" %in% dbTableInfo(conn,name)$column_name) {
          gid <- DBI::dbQuoteIdentifier(conn,".R_rownames")
        } else {
          gid <- "row_number() over()"
        }
    } else {
      gid<-DBI::dbQuoteIdentifier(conn,gid)
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT a.s as st_srid FROM
                        (SELECT ST_SRID(", geomque, ") as s FROM ",
                        nameque, " WHERE ", geomque, " IS NOT NULL ", clauses , ") a;")
    srid <- dbGetQuery(conn, tmp.query)
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) > 1) {
        stop("Multiple SRIDs in the polygon geometry")
    } else if (nrow(srid) < 1) {
        stop("Database table is empty.")
    }
    p4s <- sp::CRS(as.character(NA))@projargs
    tmp.query <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
        srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query)$p4s
    if (!is.null(db.proj4)) {
        try(p4s <- sp::CRS(db.proj4)@projargs, silent = TRUE)
    }
    if (is.na(p4s)) {
        warning("Table SRID not found. Projection will be undefined (NA)")
    }
    ## Check other columns
    if (is.null(other.cols)) {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geomque, ") AS wkt FROM ", nameque, " WHERE ", geomque,
            " IS NOT NULL ", clauses , ";")
        dfTemp <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dfTemp) <- dfTemp$tgid
    } else {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geomque, ") AS wkt,", other.cols, " FROM ", nameque,
            " WHERE ", geomque, " IS NOT NULL ", clauses , ";")
        dfTemp <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dfTemp) <- dfTemp$tgid
    }
    tt <- mapply(function(x, y, z) rgeos::readWKT(x, y, z), x = dfTemp[,
        2], y = dfTemp[, 1], z = p4s)
    Spol <- sp::SpatialPolygons(lapply(1:length(tt), function(i) {
        lin <- methods::slot(tt[[i]], "polygons")[[1]]
        ## Assign original ID to polygon
        methods::slot(lin, "ID") <- methods::slot(methods::slot(tt[[i]],
            "polygons")[[1]], "ID")
        lin
    }))
    Spol@proj4string <- methods::slot(tt[[1]], "proj4string")
    if (is.null(other.cols)) {
        return(Spol)
    } else {
        cols <- colnames(dfTemp)[3:length(colnames(dfTemp))]
        cols <- cols[!(cols %in% c(geom))]
        # column definitions
        suppressMessages(
            dfr<-dbReadDataFrame(conn, name, df = dfTemp[cols])
        )
        spdf <- sp::SpatialPolygonsDataFrame(Spol, dfr)
        return(spdf)
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
        name <- ".rpostgis_TEMPview"
        keep <- FALSE
    } else {
        keep <- TRUE
    }
    dbExecute(conn, "BEGIN;")
    # try to create view and retrieve geometries
    geo <- NULL
    try({
        prequery <- paste0("CREATE OR REPLACE VIEW ", paste(dbQuoteIdentifier(conn, 
            name), collapse = "."), " AS ")
        if (sub(".*(?=.$)", "", sub("\\s+$", "", query), perl = T) == 
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
        if (!keep) 
            dbExecute(conn, "ROLLBACK;") else dbExecute(conn, "COMMIT;")
    }
    return(geo)
}
