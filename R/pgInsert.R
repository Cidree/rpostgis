## pgInsert

##' Inserts spatial data into a PostgreSQL table.
##'
##' This function takes a take an R \code{sp} object (Spatial* or
##' Spatial*DataFrame), or a regular data frame, and performs the
##' database insert (and table creation, if specified) on the
##' database. The entire data frame is prepared, but if
##' \code{force.match} specifies a database table, the R column names
##' are compared to the \code{force.match} column names, and only
##' exact matches are formatted to be inserted. A new database table
##' can also be created using the \code{create.table} argument. If
##' \code{new.id} is specified, a new sequential integer field is
##' added to the data frame for insert. For \code{Spatial*}-only
##' objects (no data frame), a new.id is created by default with name
##' "gid".
##'
##' If the R package \code{wkb} is installed, this function will use
##' \code{writeWKB} for certain datasets (non-Multi types,
##' non-Linestring), which is faster for large datasets.  In all other
##' cases the \code{rgeos} function \code{writeWKT} is used.
##'
##' If the table is created but the data insert statement fails,
##' \code{create.table} is dropped from the database (a message will
##' be given).
##'
##' @param conn A connection object to a PostgreSQL database
##' @param data.obj A Spatial* or Spatial*DataFrame, or data frame
##' @param geom character string. For Spatial* datasets, the name of
##'     geometry column in the database table.  (existing or to be
##'     created; defaults to \code{geom}).
##' @param create.table Character, schema and table of the PostgreSQL
##'     table to create.  Column names will be converted to
##'     PostgreSQL-compliant names. Default is \code{NULL} (no new
##'     table created).
##' @param force.match Character, schema and table of the PostgreSQL
##'     table to compare columns of data frame with.  If specified,
##'     only columns in the data frame that exactly match the database
##'     table will be kept, and reordered to match the database table.
##' @param new.id Character, name of a new sequential integer ID
##'     column to be added to the table.  (for spatial objects without
##'     data frames, this column is created even if left \code{NULL}
##'     and defaults to the name \code{gid}).  Must match an existing
##'     column name (and numeric type) when used with
##'     \code{force.match}, otherwise it will be discarded.
##' @param alter.names Logical, whether to make database column names
##'     DB-compliant (remove special characters). Default is
##'     \code{TRUE}.  (This should to be set to \code{FALSE} to match
##'     to non-standard names in an existing database table using the
##'     \code{force.match} setting.)
##' @param encoding Character vector of length 2, containing the
##'     from/to encodings for the data (as in the function
##'     \code{iconv}). For example, if the dataset contain certain
##'     latin characters (e.g., accent marks), and the database is in
##'     UTF-8, use \code{encoding = c("latin1","UTF-8")}. Left
##'     \code{NULL}, no conversion will be done.
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @export
##' @return DBIResult
##' @examples
##' \dontrun{
##' library(sp)
##' data(meuse)
##' coords <- SpatialPoints(meuse[, c("x", "y")])
##' spdf <- SpatialPointsDataFrame(coords, meuse)
##'
##' ## Insert data in new database table
##' pgInsert(conn, data.obj = spdf, create.table = c("public", "meuse_data"))
##'
##' ## Insert into already created table
##' pgInsert(conn, data.obj = spdf, force.match = c("public", "meuse_data"))
##' }

pgInsert <- function(conn, data.obj, create.table = NULL, force.match = NULL,
    geom = "geom", new.id = NULL, alter.names = TRUE, encoding = NULL) {
    if (class(data.obj) != "pgi") {
        if (is.null(create.table) & is.null(force.match)) {
            stop("Must specify a database table to create, or match to.")
        }
        if (!is.null(create.table) & !is.null(force.match)) {
            stop("Either create.table or force.match must be NULL.")
        }
    }
    dbSendQuery(conn, "BEGIN TRANSACTION;")
    geo.classes <- c("SpatialPoints", "SpatialPointsDataFrame",
        "SpatialLines", "SpatialLinesDataFrame", "SpatialPolygons",
        "SpatialPolygonsDataFrame")
    cls <- class(data.obj)[1]
    pgi <- NULL
    if (cls %in% geo.classes) {
        try(pgSRID(data.obj@proj4string, conn, create = TRUE,
            new.srid = NULL))
        try(pgi <- pgInsertizeGeom(data.obj, geom, create.table,
            force.match, conn, new.id, alter.names))
    } else if (cls == "data.frame") {
        try(pgi <- pgInsertize(data.obj, create.table, force.match,
            conn, new.id, alter.names))
    } else if (cls == "pgi") {
        pgi <- data.obj
        message("Using previously create pgi object. All arguments except for \"conn\" and \"encoding\" will be ignored.")
        if (is.null(pgi$in.table)) {
            stop("Table to insert into not specified (in pgi$in.table). Set this and re-run.")
        }
        ## Continue
    } else {
        stop("Input data object not of correct class - must be a Spatial*, Spatial*DataFrame, or data frame.")
    }
    if (is.null(pgi)) {
        dbSendQuery(conn, "ROLLBACK;")
        stop("Table preparation failed. No changes made to database.")
    }
    ## Change encoding if specified
    if (!is.null(encoding)) {
        pgi$insert.data <- iconv(pgi$insert.data, encoding[1],
            encoding[2])
    }
    ## Create table if specified
    if (!is.null(pgi$db.new.table)) {
        quet <- NULL
        try(quet <- dbSendQuery(conn, pgi$db.new.table))
        if (is.null(quet)) {
            dbSendQuery(conn, "ROLLBACK;")
            stop("Table creation failed. No changes made to database.")
        }
    }
    
    ## Set name of table
    name <- pgi$in.table
    namechar<-dbTableNameFix(name)

    cols <- pgi$db.cols.insert
    values <- pgi$insert.data
    db.cols <- dbTableInfo(conn, name = name)$column_name
    if (is.null(db.cols)) {
        stop(paste0("Database table ", paste(name,
            collapse = "."), " not found."))
    }
    test <- match(cols, db.cols)
    unmatched <- cols[is.na(test)]
    if (length(unmatched) > 0) {
        stop(paste0("The column(s) (", paste(unmatched, collapse = ","),
            ") are not in the database table."))
    }
    cols2 <- paste0("(\"", paste(cols, collapse = "\",\""), "\")")
    quei <- NULL
    ## Send insert query
    try(quei <- dbSendQuery(conn, paste0("INSERT INTO ", namechar[1],
        ".", namechar[2], cols2, " VALUES ", values, ";")))
    if (!is.null(quei)) {
        dbSendQuery(conn, "COMMIT;")
        print(paste0("Data inserted into table '",
           paste(name, collapse = "."), "'"))
    } else {
        dbSendQuery(conn, "ROLLBACK;")
        stop("Insert failed. No changes made to database.")
    }
}
