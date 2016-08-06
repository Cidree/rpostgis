## pgMakePts

##' Add a POINT or LINESTRING geometry field.
##'
##' Add a new POINT or LINESTRING geometry field.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string specifying the name of the new
##'     geometry column.
##' @param x The name of the x/longitude field.
##' @param y The name of the y/latitude field.
##' @param srid A valid SRID for the new geometry.
##' @param index Logical. Whether to create an index on the new
##'     geometry.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return If \code{exec = TRUE}, returns \code{TRUE} if the geometry
##'     field was successfully created.
##' @seealso The PostGIS documentation for \code{ST_MakePoint}:
##'     \url{http://postgis.net/docs/ST_MakePoint.html}, and for
##'     \code{ST_MakeLine}:
##'     \url{http://postgis.net/docs/ST_MakeLine.html}, which are the
##'     main functions of the call.
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## Examples use a dummy connection from DBI package
##' conn <- DBI::ANSI()
##'
##' ## Create a new POINT field called 'pts_geom'
##' pgMakePts(conn, name = c("schema", "table"), colname = "pts_geom",
##'     x = "longitude", y = "latitude", srid = 4326, exec = FALSE)

pgMakePts <- function(conn, name, colname = "geom", x = "x",
    y = "y", srid, index = TRUE, display = TRUE, exec = TRUE) {
    ## Check if PostGIS installed (in case of 'exec = TRUE')
    if (exec) {
        if (!suppressMessages(pgPostGIS(conn))) {
            stop("PostGIS is not enabled on this database.")
        }
    }
    ## Check and prepare the schema.table name
    nameque <- paste(dbTableNameFix(name), collapse = ".")
    ## Prepare column names
    colnameque <- DBI::dbQuoteIdentifier(conn, colname)
    x <- DBI::dbQuoteIdentifier(conn, x)
    y <- DBI::dbQuoteIdentifier(conn, y)
    ## Stop if no SRID
    if (missing(srid))
        stop("A valid SRID should be provided.")
    ## SQL query to add the POINT geometry column
    ## --
    ## ALTER TABLE "<schema>"."<table>" ADD COLUMN "<colname>" geometry(POINT, <srid>);
    ## --
    tmp.query <- paste0("ALTER TABLE ", nameque, " ADD COLUMN ",
        colnameque, " geometry(POINT, ", srid, ");")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query
    if (exec)
        dbSendQuery(conn, tmp.query)
    ## Create an index
    if (index) {
        ## The name of the index is enforced
        idxname <- paste(name[length(name)], colname, "idx",
            sep = "_")
        ## SQL query to create the index
        ## --
        ## CREATE INDEX "<table>_<colname>_idx" ON "<schema>"."<table>" USING GIST ("<colname>");
        ## --
        dbIndex(conn = conn, name = name, colname = colnameque,
            idxname = idxname, method = "gist", display = display,
            exec = exec)
    }
    ## SQL query to populate the POINT geometry field
    ## --
    ## UPDATE "<schema>"."<table>"
    ## SET "<colname>" = ST_SetSRID(ST_MakePoint("<x>", "<y>"), <srid>)
    ## WHERE "<x>" IS NOT NULL AND "<y>" IS NOT NULL;
    ## --
    tmp.query <- paste0("UPDATE ", nameque, "\nSET ", colnameque,
        " = ST_SetSRID(ST_MakePoint(", x, ", ", y, "), ", srid,
        ")\nWHERE ", x, " IS NOT NULL AND ", y, " IS NOT NULL;")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query
    if (exec) {
        dbSendQuery(conn, tmp.query)
        ## Return TRUE
        return(TRUE)
    }
}


## pgMakeStp

##' @rdname pgMakePts
##' @param dx The name of the dx field (i.e. increment in x
##'     direction).
##' @param dy The name of the dy field (i.e. increment in y
##'     direction).
##' @importFrom DBI dbQuoteIdentifier
##' @export
##' @examples
##'
##' ## Create a new LINESTRING field called 'stp_geom'
##' pgMakeStp(conn, name = c("schema", "table"), colname = "stp_geom",
##'     x = "longitude", y = "latitude", dx = "xdiff", dy = "ydiff",
##'     srid = 4326, exec = FALSE)

pgMakeStp <- function(conn, name, colname = "geom", x = "x",
    y = "y", dx = "dx", dy = "dy", srid, index = TRUE, display = TRUE,
    exec = TRUE) {
    if (exec) {
        if (!suppressMessages(pgPostGIS(conn))) {
            stop("PostGIS is not enabled on this database.")
        }
    }
    ## Check and prepare the schema.table name
    nameque <- paste(dbTableNameFix(name), collapse = ".")
    ## Prepare column names
    colnameque <- DBI::dbQuoteIdentifier(conn, colname)
    x <- DBI::dbQuoteIdentifier(conn, x)
    y <- DBI::dbQuoteIdentifier(conn, y)
    dx <- DBI::dbQuoteIdentifier(conn, dx)
    dy <- DBI::dbQuoteIdentifier(conn, dy)
    ## Stop if no SRID
    if (missing(srid))
        stop("A valid SRID should be provided.")
    ## SQL query to add the LINESTRING geometry column
    ## --
    ## ALTER TABLE "<schema>"."<table>" ADD COLUMN "<colname>" geometry(LINESTRING, <srid>);
    ## --
    tmp.query <- paste0("ALTER TABLE ", nameque, " ADD COLUMN ",
        colnameque, " geometry(LINESTRING, ", srid, ");")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query
    if (exec)
        dbSendQuery(conn, tmp.query)
    ## Create an index
    if (index) {
        ## The name of the index is enforced
        idxname <- paste(name[length(name)], colname, "idx",
            sep = "_")
        ## SQL query to create the index
        ## --
        ## CREATE INDEX "<table>_<colname>_idx" ON "<schema>"."<table>" USING GIST ("<colname>");
        ## --
        dbIndex(conn = conn, name = name, colname = colnameque,
            idxname = idxname, method = "gist", display = display,
            exec = exec)
    }
    ## SQL query to populate the LINESTRING geometry field
    ## --
    ## UPDATE "<schema>"."<table>"
    ## SET "<colname>" = ST_SetSRID(ST_MakeLine(
    ##     ARRAY[ST_MakePoint("<x>", "<y>"),
    ##           ST_MakePoint("<x>" + "<dx>", "<y>" + "<dy>")]
    ##     ), <srid>)
    ## WHERE "<x>" IS NOT NULL AND "<y>" IS NOT NULL;
    ## --
    tmp.query <- paste0("UPDATE ", nameque, "\nSET ", colnameque,
        " = ST_SetSRID(ST_MakeLine(\n    ARRAY[ST_MakePoint(",
        x, ", ", y, "), ", "\n          ST_MakePoint(", x, " + ",
        dx, ", ", y, " + ", dy, ")]\n    ), ", srid, ")\nWHERE ",
        dx, " IS NOT NULL AND ", dy, " IS NOT NULL;")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query
    if (exec) {
        dbSendQuery(conn, tmp.query)
        ## Return TRUE
        return(TRUE)
    }
}
