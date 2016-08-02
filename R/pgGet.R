## pgGetPts

##' Load a PostGIS geometry in a PostgreSQL table/view into R.
##'
##' Retrieve point, linestring, or polygon geometries from a PostGIS
##' table/view, and convert it to an R `sp` object (Spatial* or
##' Spatial*DataFrame).
##'
##' @aliases pgGet
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., `name =
##'     c("schema","table")`)
##' @param geom The name of the geometry column. (Default = 'geom')
##' @param gid Name of the column in `name` holding the IDs. Should be
##'     unique if additional columns of unique data are being
##'     appended. \code{gid=NULL} (default) automatically creates a
##'     new unique ID for each row in the `sp` object.
##' @param other.cols Names of specific columns in the table to
##'     retrieve, comma seperated in one character element
##'     (e.g. \code{other.cols='col1,col2'}. The default is to attach
##'     all columns in a Spatial*DataFrame. Setting
##'     \code{other.cols=NULL} will return a Spatial-only object (no
##'     data).
##' @param query character, additional SQL to append to modify select
##'     query from table. Must begin with "AND ..."; see below for
##'     examples.
##' @return Spatial(Multi)PointsDataFrame or Spatial(Multi)Points
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @importFrom sp CRS
##' @importFrom sp SpatialPoints
##' @importFrom sp SpatialPointsDataFrame
##' @importFrom sp SpatialMultiPoints
##' @importFrom sp SpatialMultiPointsDataFrame
##' @importFrom rgeos readWKT
##' @export
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
    query = NULL) {
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    namechar <- paste(gsub('^"|"$', '', name),collapse=".")
    ## Check table exists
    tmp.query <- paste0("SELECT f_geometry_column AS geo FROM public.geometry_columns\nWHERE 
        (f_table_schema||'.'||f_table_name) = '",namechar,"';")
    tab.list <- dbGetQuery(conn, tmp.query)$geo
    if (is.null(tab.list)) {
        stop(paste0("Table/view '", namechar, "' is not listed in public.geometry_columns."))
    } else if (!geom %in% tab.list) {
        stop(paste0("Table/view '", namechar, "' geometry column not found. Available geometry columns: ",
            paste(tab.list, collapse = ", ")))
    }
    ## If ID not specified, set it to generate row numbers
    if (is.null(gid)) {
        gid <- "row_number() over()"
    }
    ## Check if MULTI or single geom
    tmp.query <- paste0("SELECT DISTINCT ST_GeometryType(", geom,
        ") AS type FROM ", nameque, " WHERE ", geom, " IS NOT NULL ",
        query, ";")
    typ <- dbGetQuery(conn, tmp.query)
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ",
        nameque, " WHERE ", geom, " IS NOT NULL ", query, ";")
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
                geom, ") AS x, ST_Y(", geom, ") AS y FROM ",
                nameque, " WHERE ", geom, " IS NOT NULL ", query,
                ";")
        } else {
            tmp.query <- paste0("SELECT ", gid, " AS tgid,ST_X(",
                geom, ") AS x, ST_Y(", geom, ") AS y,", other.cols,
                " FROM ", nameque, " WHERE ", geom, " IS NOT NULL ",
                query, ";")
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
                geom, ") AS wkt FROM ", nameque, " WHERE ", geom,
                " IS NOT NULL ", query, ";")
        } else {
            tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
                geom, ") AS wkt,", other.cols, " FROM ", nameque,
                " WHERE ", geom, " IS NOT NULL ", query, ";")
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

##' @rdname pgGetPts
##' @importFrom sp CRS
##' @importFrom sp SpatialLines
##' @importFrom sp SpatialLinesDataFrame
##' @importFrom rgeos readWKT
##' @importFrom methods slot
##' @export
##' @return SpatialLinesDataFrame or SpatialLines
##' @examples
##' \dontrun{
##' pgGetLines(conn, c("schema", "tablename"))
##' pgGetLines(conn, c("schema", "roads"), geom = "roadgeom", gid = "road_ID",
##'     other.cols = NULL, query = "AND field = 'highway'")
##' }

pgGetLines <- function(conn, name, geom = "geom", gid = NULL,
    other.cols = "*", query = NULL) {
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    namechar <- paste(gsub('^"|"$', '', name),collapse=".")
    ## Check table exists
    tmp.query <- paste0("SELECT f_geometry_column AS geo FROM public.geometry_columns\nWHERE 
        (f_table_schema||'.'||f_table_name) = '",namechar,"';")
    tab.list <- dbGetQuery(conn, tmp.query)$geo
    if (is.null(tab.list)) {
        stop(paste0("Table/view '", namechar, "' is not listed in public.geometry_columns."))
    } else if (!geom %in% tab.list) {
        stop(paste0("Table/view '", namechar, "' geometry column not found. Available geometry columns: ",
            paste(tab.list, collapse = ", ")))
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ",
        nameque, " WHERE ", geom, " IS NOT NULL ", query, ";")
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
    ## Check gid
    if (is.null(gid)) {
        gid <- "row_number() over()"
    }
    ## Check other.cols
    if (is.null(other.cols)) {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geom, ") AS wkt FROM ", nameque, " WHERE ", geom,
            " IS NOT NULL ", query, ";")
        dfTemp <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dfTemp) <- dfTemp$tgid
    } else {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geom, ") AS wkt,", other.cols, " FROM ", nameque,
            " WHERE ", geom, " IS NOT NULL ", query, ";")
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

##' @rdname pgGetPts
##' @importFrom sp CRS
##' @importFrom sp SpatialPolygons
##' @importFrom sp SpatialPolygonsDataFrame
##' @importFrom rgeos readWKT
##' @importFrom methods slot
##' @export
##' @return SpatialPolygonsDataFrame or SpatialPolygons
##' @examples
##' \dontrun{
##' pgGetPolys(conn, c("schema", "tablename"))
##' pgGetPolys(conn, c("schema", "states"), geom = "statesgeom",
##'     gid = "state_ID", other.cols = "area,population",
##'     query = "AND area > 1000000 ORDER BY population LIMIT 10")
##' }

pgGetPolys <- function(conn, name, geom = "geom", gid = NULL,
    other.cols = "*", query = NULL) {
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    namechar <- paste(gsub('^"|"$', '', name),collapse=".")
    ## Check table exists
    tmp.query <- paste0("SELECT f_geometry_column AS geo FROM public.geometry_columns\nWHERE 
                        (f_table_schema||'.'||f_table_name) = '",namechar,"';")
    tab.list <- dbGetQuery(conn, tmp.query)$geo
    if (is.null(tab.list)) {
        stop(paste0("Table/view '", namechar, "' is not listed in public.geometry_columns."))
    } else if (!geom %in% tab.list) {
        stop(paste0("Table/view '", namechar, "' geometry column not found. Available geometry columns: ",
            paste(tab.list, collapse = ", ")))
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ",
        nameque, " WHERE ", geom, " IS NOT NULL ", query, ";")
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
    ## Check gid
    if (is.null(gid)) {
        gid <- "row_number() over()"
    }
    ## Check other columns
    if (is.null(other.cols)) {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geom, ") AS wkt FROM ", nameque, " WHERE ", geom,
            " IS NOT NULL ", query, ";")
        dfTemp <- suppressWarnings(dbGetQuery(conn, tmp.query))
        row.names(dfTemp) <- dfTemp$tgid
    } else {
        tmp.query <- paste0("SELECT ", gid, " AS tgid,st_astext(",
            geom, ") AS wkt,", other.cols, " FROM ", nameque,
            " WHERE ", geom, " IS NOT NULL ", query, ";")
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
