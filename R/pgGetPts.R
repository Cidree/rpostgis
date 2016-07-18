## pgGetPts

#' Retrieve point geometries from a PostGIS table, and convert it to a
#' SpatialPoints or a SpatialPointsDataFrame.
#'
#' @title Retrieve point geometries
#' @param conn A connection object to a PostgreSQL database
#' @param name A character string specifying a PostgreSQL schema (if
#'     necessary), and table or view name for the table holding the
#'     points geometry (e.g., name = c("schema","table"))
#' @param geom The name of the point geometry column. (Default =
#'     'geom')
#' @param gid Name of the column in 'name' holding the ID. Should be
#'     unique if additional columns of unique data are being
#'     appended. \code{gid=NULL} (default) automatically creates a new
#'     unique ID for each row in the table.
#' @param other.cols Names of specific columns in the table to
#'     retrieve, comma seperated in one character element
#'     (e.g. \code{other.cols='col1,col2'}. The default is to attach
#'     all columns in a SpatialPointsDataFrame. Setting
#'     \code{other.cols=NULL} will return a SpatialPoints.
#' @param query character, additional SQL to append to modify select
#'     query from table
#' @return A Spatial(Multi)Points or a Spatial(Multi)PointsDataFrame
#' @author David Bucklin \email{david.bucklin@gmail.com}
#' @author Mathieu Basille \email{basille@@ufl.edu}
#' @importFrom sp CRS
#' @importFrom sp SpatialPoints
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialMultiPoints
#' @importFrom sp SpatialMultiPointsDataFrame
#' @importFrom rgeos readWKT
#' @export
#' @examples
#' \dontrun{
#' ## Retrieve a SpatialPointsDataFrame with all data from table 'schema.tablename',
#' with geometry in the column 'geom'
#' pgGetPts(conn, c('schema','tablename'))
#' ## Return a SpatialPointsDataFrame with columns c1 & c2 as data
#' pgGetPts(conn, c('schema','tablename'), other.cols = 'c1,c2')
#' ## Return a SpatialPoints, retaining id from table as rownames
#' pgGetPts(conn, c('schema','tablename'), gid = 'table_id', other.cols = FALSE)
#' }

pgGetPts <- function(conn, name, geom = "geom", gid = NULL, other.cols = "*",
    query = NULL) {
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2) {
        name <- paste(name, collapse = ".")
    } else {
        stop("The table name should be \"table\" or c(\"schema\", \"table\").")
    }
    ## if ID not specified, set it to generate row numbers
    if (is.null(gid)) {
        gid <- "row_number() over()"
    }
    ## Check if MULTI or single geom
    query <- paste0("SELECT DISTINCT ST_GeometryType(", geom,
        ") AS type FROM ", name, " WHERE ", geom, " IS NOT NULL;")
    typ <- dbGetQuery(conn, query)
    ## Retrieve the SRID
    query <- paste0("SELECT DISTINCT(ST_SRID(", geom, ")) FROM ",
        name, " WHERE ", geom, " IS NOT NULL;")
    srid <- dbGetQuery(conn, query)
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) != 1)
        stop("Multiple SRIDs in the point geometry")
    # make spatialpoints* for single geom types
    if (length(typ$type) == 1 && typ$type == "ST_Point") {
        # get data
        if (is.null(other.cols)) {
            query <- paste0("select ", gid, " as tgid,ST_X(",
                geom, ") AS x, ST_Y(", geom, ") AS y from ",
                name, " where ", geom, " is not null ", query,
                ";")
        } else {
            query <- paste0("select ", gid, " as tgid,ST_X(",
                geom, ") AS x, ST_Y(", geom, ") AS y,", other.cols,
                " from ", name, " where ", geom, " is not null ",
                query, ";")
        }
        dbData <- suppressWarnings(dbGetQuery(conn, query))
        row.names(dbData) <- dbData$tgid
        ## Generate a SpatialPoints object
        sp <- sp::SpatialPoints(data.frame(x = dbData$x, y = dbData$y,
            row.names = dbData$tgid), proj4string = sp::CRS(paste0("+init=epsg:",
            srid$st_srid)))
        ## Append data to spdf if requested
        if (!is.null(other.cols)) {
            cols <- colnames(dbData)
            cols <- cols[!(cols %in% c("tgid", "x", "y", geom))]
            sp <- sp::SpatialPointsDataFrame(sp, dbData[cols],
                match.ID = TRUE)
        }
    } else {
        ## make SpatialMultiPoints(Dataframe) for multi-point types
        if (is.null(other.cols)) {
            query <- paste0("select ", gid, " as tgid,st_astext(",
                geom, ") as wkt from ", name, " where ", geom,
                " is not null ", query, ";")
        } else {
            query <- paste0("select ", gid, " as tgid,st_astext(",
                geom, ") as wkt,", other.cols, " from ", name,
                " where ", geom, " is not null ", query, ";")
        }
        dbData <- suppressWarnings(dbGetQuery(conn, query))
        row.names(dbData) <- dbData$tgid
        # create spatialMultiPoints
        tt <- mapply(function(x, y, z) readWKT(x, y, z), x = dbData$wkt,
            y = dbData$tgid, z = sp::CRS(paste0("+init=epsg:",
                srid$st_srid))@projargs)
        sp <- sp::SpatialMultiPoints(tt, proj4string = sp::CRS(paste0("+init=epsg:",
            srid$st_srid)))
        ## Append data to spdf if requested
        if (!is.null(other.cols)) {
            cols <- colnames(dbData)
            cols <- cols[!(cols %in% c("tgid", "wkt", geom))]
            sp <- sp::SpatialMultiPointsDataFrame(tt, dbData[cols],
                proj4string = sp::CRS(paste0("+init=epsg:", srid$st_srid)))
        }
    }
    return(sp)
}
