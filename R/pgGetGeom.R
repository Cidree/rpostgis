# pgGetGeom

##' Load a PostGIS geometry from a PostgreSQL table/view into R.
##'
##' Retrieve point, linestring, or polygon geometries from a PostGIS
##' table/view, and convert it to an R `sp` object (Spatial* or
##' Spatial*DataFrame).
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., `name =
##'     c("schema","table")`)
##' @param geom The name of the geometry column. (Default = "geom")
##' @param gid Name of the column in `name` holding the IDs. Should be
##'     unique if additional columns of unique data are being
##'     appended. \code{gid=NULL} (default) automatically creates a
##'     new unique ID for each row in the `sp` object.
##' @param other.cols Names of specific columns in the table to
##'     retrieve, in a character vector (e.g. \code{other.cols=c("col1","col2")}.)
##'     The default (\code{other.cols = TRUE}) is to attach
##'     all columns in a Spatial*DataFrame. Setting
##'     \code{other.cols=FALSE} will return a Spatial-only object (no
##'     data frame).
##' @param clauses character, additional SQL to append to modify select
##'     query from table. Must begin with an SQL clause (e.g., "WHERE ...",
##'     "ORDER BY ...", "LIMIT ..."); see below for examples.
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
##' }

pgGetGeom <- function(conn, name, geom = "geom", gid = NULL, 
    other.cols = TRUE, clauses = NULL) {
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    ## Check and prepare the schema.name
    nameque <- paste(dbTableNameFix(name), collapse = ".")
    namechar <- gsub("\"\"", "\"", gsub("'", "''", paste(gsub("^\"|\"$", 
        "", dbTableNameFix(name)), collapse = ".")))
    ## Check table exists
    tmp.query <- paste0("SELECT f_geometry_column AS geo FROM public.geometry_columns\nWHERE 
        (f_table_schema||'.'||f_table_name) = '", 
        namechar, "';")
    tab.list <- dbGetQuery(conn, tmp.query)$geo
    if (is.null(tab.list)) {
        stop(paste0("Table/view '", namechar, "' is not listed in public.geometry_columns."))
    } else if (!geom %in% tab.list) {
        stop(paste0("Table/view '", namechar, "' geometry column not found. Available geometry columns: ", 
            paste(tab.list, collapse = ", ")))
    }
    ## prepare geom column
    geomque <- DBI::dbQuoteIdentifier(conn, geom)
    ## prepare clauses
    clauses <- sub("^where", "AND", clauses, ignore.case = TRUE)
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
    tmp.query <- paste0("SELECT DISTINCT ST_GeometryType(", geomque, 
        ") AS type FROM ", nameque, " WHERE ", geomque, " IS NOT NULL ", 
        clauses, ";")
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
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    ## prepare additional clauses
    clauses<-sub("^where", "AND",clauses, ignore.case = TRUE)
    ## prepare geom column
    geomque<-DBI::dbQuoteIdentifier(conn,geom)
    ## If ID not specified, set it to generate row numbers
    if (is.null(gid)) {
        gid <- "row_number() over()"
    } else {
      gid<-DBI::dbQuoteIdentifier(conn,gid)
    }
    ## Check if MULTI or single geom
    tmp.query <- paste0("SELECT DISTINCT ST_GeometryType(", geomque,
        ") AS type FROM ", nameque, " WHERE ", geomque, " IS NOT NULL ",
        clauses , ";")
    typ <- dbGetQuery(conn, tmp.query)
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geomque, ")) FROM ",
        nameque, " WHERE ", geomque, " IS NOT NULL ", clauses , ";")
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
            cols <- colnames(dbData)
            cols <- cols[!(cols %in% c("tgid", "x", "y", geom))]
            sp <- sp::SpatialPointsDataFrame(sp, dbData[cols],
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
            cols <- colnames(dbData)
            cols <- cols[!(cols %in% c("tgid", "wkt", geom))]
            sp <- sp::SpatialMultiPointsDataFrame(tt, dbData[cols],
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
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")

    ## prepare additional clauses
    clauses<-sub("^where", "AND",clauses, ignore.case = TRUE)
    
    ## prepare geom column
    geomque<-DBI::dbQuoteIdentifier(conn,geom)
    ## Check gid
    if (is.null(gid)) {
        gid <- "row_number() over()"
    } else {
      gid<-DBI::dbQuoteIdentifier(conn,gid)
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geomque, ")) FROM ",
        nameque, " WHERE ", geomque, " IS NOT NULL ", clauses , ";")
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
        try(dfTemp[geom] <- NULL)
        try(dfTemp["wkt"] <- NULL)
        spdf <- sp::SpatialLinesDataFrame(Sline, dfTemp)
        spdf@data["tgid"] <- NULL
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
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    ## prepare additional clauses
    clauses<-sub("^where", "AND",clauses, ignore.case = TRUE)
    ## prepare geom column
    geomque<-DBI::dbQuoteIdentifier(conn,geom)
    ## Check gid
    if (is.null(gid)) {
        gid <- "row_number() over()"
    } else {
      gid<-DBI::dbQuoteIdentifier(conn,gid)
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geomque, ")) FROM ",
        nameque, " WHERE ", geomque, " IS NOT NULL ", clauses , ";")
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
        try(dfTemp[geom] <- NULL)
        try(dfTemp["wkt"] <- NULL)
        spdf <- sp::SpatialPolygonsDataFrame(Spol, dfTemp)
        spdf@data["tgid"] <- NULL
        return(spdf)
    }
}
