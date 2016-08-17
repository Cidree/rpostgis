## pgInsert

##' Inserts data into a PostgreSQL table.
##'
##' This function takes a take an R \code{sp} object (Spatial* or
##' Spatial*DataFrame), or a regular data frame, and performs the
##' database insert (and table creation, when the table doesn't exist)
##' on the database.
##'
##' If \code{new.id} is specified, a new sequential integer field is
##' added to the data frame for insert. For \code{Spatial*}-only
##' objects (no data frame), a new.id is created by default with name
##' "gid".
##'
##' If the R package \code{wkb} is installed, this function will use
##' \code{writeWKB} for certain datasets (non-Multi types,
##' non-Linestring), which is faster for large datasets.  In all other
##' cases the \code{rgeos} function \code{writeWKT} is used.
##'
##' In the event of function or database error, the database uses
##' ROLLBACK to revert to the previous state. 
##' 
##' On database errors, or if the user specifies \code{return.pgi = TRUE},
##' the function will return a \code{pgi} object (see next paragraph). This
##' object can be re-used as the \code{data.obj} in \code{pgInsert}; (e.g., when
##' inserting the exact same data into tables in two separate tables or databases). 
##' If \code{return.pgi = FALSE} (default), the function will return \code{TRUE},
##' indicating successful insert.
##' 
##' pgi objects are a list containing four character strings: (1)
##' in.table, the table name which will be created or inserted
##' into (2) db.new.table, the SQL statement to create the new
##' table, (3) db.cols.insert, a character string of the database column
##' names to insert into, and (4) insert.data, a character string
##' of the data to insert.
##' 
##' 
##' @param conn A connection object to a PostgreSQL database
##' @param name Character, schema and table of the PostgreSQL table to
##'     insert into. If not already existing, the table will be
##'     created. If the table already exists, the function will check
##'     if all R data frame columns match database columns, and if so,
##'     do the insert. If not, the insert will be aborted. The
##'     argument \code{partial.match} allows for inserts with only
##'     partial matches of data frame and database column names, and
##'     \code{overwrite} allows for overwriting the existing database
##'     table.
##' @param data.obj A Spatial* or Spatial*DataFrame, or data frame
##' @param geom character string. For Spatial* datasets, the name of
##'     geometry column in the database table.  (existing or to be
##'     created; defaults to \code{"geom"}).
##' @param partial.match Logical; allow insert on partial column
##'     matches between data frame and database table. If \code{TRUE},
##'     columns in R data frame will be compared with the existing
##'     database table \code{name}.  Columns in the data frame that
##'     exactly match the database table will be inserted into the
##'     database table.
##' @param overwrite Logical; if true, a new table (\code{name}) will
##'     overwrite the existing table (\code{name}) in the database.
##' @param new.id Character, name of a new sequential integer ID
##'     column to be added to the table for insert (for spatial objects without
##'     data frames, this column is created even if left \code{NULL}
##'     and defaults to the name \code{"gid"}). If \code{partial.match
##'     = TRUE} and the column does not exist in the databse table,
##'     it will be discarded.
##' @param alter.names Logical, whether to make database column names
##'     DB-compliant (remove special characters). Default is
##'     \code{TRUE}.  (This should to be set to \code{FALSE} to match
##'     to non-standard names in an existing database table.)
##' @param encoding Character vector of length 2, containing the
##'     from/to encodings for the data (as in the function
##'     \code{iconv}). For example, if the dataset contain certain
##'     latin characters (e.g., accent marks), and the database is in
##'     UTF-8, use \code{encoding = c("latin1", "UTF-8")}. Left
##'     \code{NULL}, no conversion will be done.
##' @param return.pgi Whether to return a formatted list of insert parameters
##'     (i.e., a \code{pgi} object; see function details.)
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @export
##' @return Returns \code{TRUE} if the insertion was successful, or a
##' \code{pgi} object if specified, or in the case of database error.
##' @examples
##' \dontrun{
##' library(sp)
##' data(meuse)
##' coords <- SpatialPoints(meuse[, c("x", "y")])
##' spdf <- SpatialPointsDataFrame(coords, meuse)
##'
##' ## Insert data in new database table
##' pgInsert(conn, name = c("public", "meuse_data"), data.obj = spdf)
##'
##' ## The same command will insert into already created table (if all R
##' ## columns match)
##' pgInsert(conn, name = c("public", "meuse_data"), data.obj = spdf)
##'
##' ## If not all database columns match, need to use partial.match = TRUE
##' colnames(spdf@data)[4] <- "cu"
##' pgInsert(conn, name = c("public", "meuse_data"), data.obj = spdf,
##'     partial.match = TRUE)
##' }

pgInsert <- function(conn, name, data.obj, geom = "geom", partial.match = FALSE, 
    overwrite = FALSE, new.id = NULL, alter.names = TRUE, encoding = NULL, 
    return.pgi = FALSE) {
    ## Check if PostGIS installed
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    # data.obj class
    cls <- class(data.obj)[1]
    if (cls == "pgi") {
      if (is.null(data.obj$in.table)) {
            stop("Table to insert into not specified (in pgi$in.table). Set this and re-run.")
        } else {name<-data.obj$in.table}
    }
    ## Check for existing table
    exists.t <- dbExistsTable(conn, name)
    if (!exists.t) {
        message("Creating new table...")
        create.table <- name
        force.match <- NULL
    } else if (exists.t & overwrite & !partial.match) {
        create.table <- name
        force.match <- NULL
    } else {
        force.match <- name
        create.table <- NULL
    }
    dbSendQuery(conn, "BEGIN TRANSACTION;")
    geo.classes <- c("SpatialPoints", "SpatialPointsDataFrame", 
        "SpatialLines", "SpatialLinesDataFrame", "SpatialPolygons", 
        "SpatialPolygonsDataFrame")
    pgi <- NULL
    if (cls %in% geo.classes) {
        try(suppressMessages(pgSRID(conn, data.obj@proj4string, 
            create.srid = TRUE, new.srid = NULL)), silent = TRUE)
        try(pgi <- pgInsertizeGeom(data.obj, geom, create.table, 
            force.match, conn, new.id, alter.names, partial.match))
    } else if (cls == "data.frame") {
        try(pgi <- pgInsertize(data.obj, create.table, force.match, 
            conn, new.id, alter.names, partial.match))
    } else if (cls == "pgi") {
        pgi <- data.obj
        message("Using previously create pgi object. All arguments except for \"conn\", \"overwrite\", and \"encoding\" will be ignored.")
    } else {
        dbSendQuery(conn, "ROLLBACK;")
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
        if (overwrite) {
            over.t <- dbDrop(conn, name = name, type = "table", 
                ifexists = TRUE)
            if (!over.t) {
                dbSendQuery(conn, "ROLLBACK;")
                message("Could not drop existing table; pgi object returned. No changes made to database.")
                return(pgi)
            }
        }
        quet <- NULL
        try(quet <- dbSendQuery(conn, pgi$db.new.table))
        if (is.null(quet)) {
            dbSendQuery(conn, "ROLLBACK;")
            message("Table creation failed; pgi object returned. No changes made to database.")
            return(pgi)
        }
    } else if (is.null(pgi$db.new.table) & overwrite) {
      message("No create table definition in pgi object (pgi$db.new.table);
              not dropping existing table...")
    }
    
    ## Set name of table
    name <- pgi$in.table
    nameque <- dbTableNameFix(name)
    cols <- pgi$db.cols.insert
    values <- pgi$insert.data
    db.cols <- dbTableInfo(conn, name = name)$column_name
    if (is.null(db.cols)) {
        dbSendQuery(conn, "ROLLBACK;")
        message(paste0("Database table ", paste(name, collapse = "."), 
            " not found; pgi object returned. No changes made to database."))
        return(pgi)
    }
    test <- match(cols, db.cols)
    unmatched <- cols[is.na(test)]
    if (length(unmatched) > 0) {
        dbSendQuery(conn, "ROLLBACK;")
        message(paste0("The column(s) (", paste(unmatched, collapse = ","), 
            ") are not in the database table; pgi object returned. No changes made to database."))
        return(pgi)
    }
    cols2 <- paste0("(\"", paste(cols, collapse = "\",\""), "\")")
    quei <- NULL
    ## Send insert query
    try(quei <- dbSendQuery(conn, paste0("INSERT INTO ", nameque[1], 
        ".", nameque[2], cols2, " VALUES ", values, ";")))
    if (!is.null(quei)) {
        dbSendQuery(conn, "COMMIT;")
        message("Data inserted into table.")
        ## Return TRUE
        if (return.pgi) {
            return(pgi)
        } else {
            return(TRUE)
        }
    } else {
        dbSendQuery(conn, "ROLLBACK;")
        message("Insert failed; pgi object returned. No changes made to database.")
        return(pgi)
    }
}

## print.pgi

##' @rdname pgInsert
##' @param x A list of class \code{pgi}
##' @param ... Further arguments not used.
##' @export
print.pgi <- function(x, ...) {
    cat("pgi object: PostgreSQL insert object from pgInsertize* function in rpostgis. Use with pgInsert() to insert into database table.")
    cat("\n************************************\n")
    if (!is.null(x$in.tab)) {
        cat(paste0("Insert table: ", paste(x$in.tab, collapse = ".")))
        cat("\n************************************\n")
    }
    if (!is.null(x$db.new.table)) {
        cat(paste0("SQL to create new table: ", x$db.new.table))
        cat("\n************************************\n")
    }
    cat(paste0("Columns to insert into: ", paste(x$db.cols.insert, 
        collapse = ",")))
    cat("\n************************************\n")
    cat(paste0("Formatted insert data: ", substr(x$insert.data, 
        0, 1000)))
    if (nchar(x$insert.data) > 1000) {
        cat("........Only the first 1000 characters shown")
    }
}
