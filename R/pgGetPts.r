## pgGetPts
##
##' Retrieve point geometries from a PostGIS table, and convert it to
##' a SpatialPoints or a SpatialPointsDataFrame.
##'
##' @title Retrieve point geometries
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table, view
##' or schema name.
##' @param pts The name of the point geometry column.
##' @param colname The name of the columns to include or exclude
##' (defaults to \code{NULL}, i.e. no column).
##' @param include Include or exclude \code{colname} (default
##' \code{TRUE}). If \code{colname = NULL} and \code{include = FALSE},
##' all columns are retrieved.
##' @return A SpatialPoints or a SpatialPointsDataFrame
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' \dontrun{
##' ## Retrieve only the points in the column 'pts_geom'
##' pgGetPts(conn, c("fla", "bli"))
##' ## Return a SpatialPointsDataFrame with columns c1 & c2 as data
##' pgGetPts(conn, c("fla", "bli"), colname = c("c1", "c2"))
##' ## Return a SpatialPointsDataFrame with every column except c1 & c2 as
##' ## data
##' pgGetPts(conn, c("fla", "bli"), colname = c("c1", "c2"), include = FALSE)
##' ## Return a SpatialPointsDataFrame with every column as data
##' pgGetPts(conn, c("fla", "bli"), include = FALSE)}
pgGetPts <- function(conn, name, pts = "pts_geom", colname = NULL,
    include = TRUE)
{
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2)
        table <- paste(name, collapse = ".")
    else stop("The table name should be \"table\" or c(\"schema\", \"table\").")
    ## Retrieve the coordinates
    str <- paste0("SELECT ST_X(", pts, ") AS x, ST_Y(", pts,
        ") AS y FROM ", table, " WHERE ", pts, " IS NOT NULL;")
    coords <- dbGetQuery(conn, str)
    ## Retrieve the SRID
    str <- paste0("SELECT DISTINCT(ST_SRID(", pts, ")) FROM ",
        table, " WHERE ", pts, " IS NOT NULL;")
    srid <- dbGetQuery(conn, str)
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) != 1)
        stop("Multiple SRIDs in the point geometry")
    ## Generate a SpatialPoints if all columns excluded
    if (is.null(colname) & include == TRUE) {
        sp <- SpatialPoints(coords, proj4string = CRS(paste0("+init=epsg:",
            srid)))
    }
    ## Otherwise generate a SpatialPointsDataFrame with data
    else {
        ## Case of explicit columns to add
        if (!is.null(colname) & include == TRUE)
            nm <- colname
        ## Other cases
        else {
            ## All columns
            nm <- dbListFields(conn, name)
            ## Columns to exclude
            if (!is.null(colname) & include == FALSE)
                nm <- nm[!(nm %in% colname)]
        }
        ## Remove the column of points
        nm <- nm[!(nm %in% pts)]
        ## Prepare the string and retrieve the data
        str <- paste0("SELECT \"", paste0(nm, collapse = "\", \""),
            "\" FROM ", table, " WHERE ", pts, " IS NOT NULL;")
        data <- dbGetQuery(conn, str)
        ## Generate a SpatialPointsDataFrame
        sp <- SpatialPointsDataFrame(coords, data = data,
            proj4string = CRS(paste0("+init=epsg:", srid)))
    }
    return(sp)
}
