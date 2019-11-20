## pgInsert

##' Inserts data into a PostgreSQL table.
##'
##' This function takes a take an R \code{sp} object (\code{Spatial*} or
##' \code{Spatial*DataFrame}), or a regular \code{data.frame}, and performs the
##' database insert (and table creation, when the table does not exist)
##' on the database.
##'
##' If \code{new.id} is specified, a new sequential integer field is
##' added to the data frame for insert. For \code{Spatial*}-only
##' objects (no data frame), a new ID column is created by default with name
##' \code{"gid"}.
##'
##' If the R package \code{wkb} is installed, this function will use
##' \code{\link[wkb]{writeWKB}} for certain datasets (non-Multi types,
##' non-Linestring), which is faster for large datasets.  In all other
##' cases the \code{rgeos} function \code{\link[rgeos]{writeWKT}} is used.
##'
##' In the event of function or database error, the database uses
##' ROLLBACK to revert to the previous state. 
##' 
##' If the user specifies \code{return.pgi = TRUE}, and data preparation is
##' successful, the function will return 
##' a \code{pgi} object (see next paragraph), regardless of whether the
##' insert was successful or not. This object can be useful for debugging, 
##' or re-used as the \code{data.obj} in \code{pgInsert}; 
##' (e.g., when data preparation is slow, and the exact same data 
##' needs to be inserted into tables in two separate
##' tables or databases). If \code{return.pgi = FALSE}
##' (default), the function will return \code{TRUE} for successful insert and
##' \code{FALSE} for failed inserts.
##' 
##' Use this function with code{df.mode = TRUE} to save data frames from
##' \code{Spatial*}-class objects to the database in "data frame mode". Along with normal 
##' \code{dbwriteDataFrame} operation, the proj4string of the spatial 
##' data will also be saved, and re-attached to the data when using 
##' \code{pgGetGeom} to import the data. Note that other attributes
##' of \code{Spatial*} objects are \strong{not} saved (e.g., \code{coords.nrs},
##' which is used to specify the column index of x/y columns in \code{SpatialPoints*}).
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
##' @param name A character string specifying a PostgreSQL schema and
##'     table name (e.g., \code{name = c("schema","table")}). 
##'     If not already existing, the table will be
##'     created. If the table already exists, the function will check
##'     if all R data frame columns match database columns, and if so,
##'     do the insert. If not, the insert will be aborted. The
##'     argument \code{partial.match} allows for inserts with only
##'     partial matches of data frame and database column names, and
##'     \code{overwrite} allows for overwriting the existing database
##'     table.
##' @param data.obj A \code{Spatial*} or \code{Spatial*DataFrame}, or \code{data.frame}
##' @param geom character string. For \code{Spatial*} datasets, the name of
##'     geometry/(geography) column in the database table.  (existing or to be
##'     created; defaults to \code{"geom"}). The special name "geog" will
##'     automatically set \code{geog} to TRUE. 
##' @param df.mode Logical; Whether to write the (Spatial) data frame in data frame mode 
##'     (preserving data frame column attributes and row.names).
##'     A new table must be created with this mode (or overwrite set to TRUE),
##'     and the \code{row.names}, \code{alter.names}, and \code{new.id} arguments will
##'     be ignored (see \code{\link[rpostgis]{dbWriteDataFrame}} for more information).
##' @param partial.match Logical; allow insert on partial column
##'     matches between data frame and database table. If \code{TRUE},
##'     columns in R data frame will be compared with the existing
##'     database table \code{name}.  Columns in the data frame that
##'     exactly match the database table will be inserted into the
##'     database table.
##' @param overwrite Logical; if true, a new table (\code{name}) will
##'     overwrite the existing table (\code{name}) in the database. Note:
##'     overwriting a view must be done manually (e.g., with \code{\link[rpostgis]{dbDrop}}).
##' @param new.id Character, name of a new sequential integer ID
##'     column to be added to the table for insert (for spatial objects without
##'     data frames, this column is created even if left \code{NULL}
##'     and defaults to the name \code{"gid"}). If \code{partial.match
##'     = TRUE} and the column does not exist in the database table,
##'     it will be discarded.
##' @param row.names Whether to add the data frame row names to the 
##'     database table. Column name will be '.R_rownames'.
##' @param upsert.using Character, name of the column(s) in the database table 
##'     or constraint name used to identify already-existing rows in the table, which will
##'     be updated rather than inserted. The column(s) must have a unique constraint
##'     already created in the database table (e.g., a primary key). 
##'     Requires PostgreSQL 9.5+.
##' @param alter.names Logical, whether to make database column names
##'     DB-compliant (remove special characters/capitalization). Default is
##'     \code{FALSE}.  (This must be set to \code{FALSE} to match
##'     with non-standard names in an existing database table.)
##' @param encoding Character vector of length 2, containing the
##'     from/to encodings for the data (as in the function
##'     \code{\link[base]{iconv}}). For example, if the dataset contain certain
##'     latin characters (e.g., accent marks), and the database is in
##'     UTF-8, use \code{encoding = c("latin1", "UTF-8")}. Left
##'     \code{NULL}, no conversion will be done.
##' @param return.pgi Whether to return a formatted list of insert parameters
##'     (i.e., a \code{pgi} object; see function details.)
##' @param df.geom Character vector, name of a character column in an R data.frame
##'     storing PostGIS geometries, this argument can be used to insert a geometry
##'     stored as character type in a data.frame (do not use with Spatial* data types).
##'     If only the column name is used (e.g., \code{df.geom = "geom"}), 
##'     the column type will be a generic (GEOMETRY); use a two-length character vector 
##'     (e.g., \code{df.geom = c("geom", "(POINT,4326)")} to also specify a 
##'     specific PostGIS geometry type and SRID for the column. Only recommended for
##'     for new tables/overwrites, since this method will change the 
##'     existing column type.
##' @param geog Logical; Whether to write the spatial data as a PostGIS 
##'     'GEOGRAPHY' type. By default, FALSE, unless \code{geom = "geog"}.
##' @author David Bucklin \email{david.bucklin@@gmail.com}
##' @export
##' @return Returns \code{TRUE} if the insertion was successful,
##' \code{FALSE} if failed, or a \code{pgi} object if specified.
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
##' ## If not all database columns match, need to use partial.match = TRUE,
##' ## where non-matching columns are not inserted
##' colnames(spdf@data)[4] <- "cu"
##' pgInsert(conn, name = c("public", "meuse_data"), data.obj = spdf,
##'     partial.match = TRUE)
##' }

pgInsert <- function(conn, name, data.obj, geom = "geom", df.mode = FALSE, partial.match = FALSE, 
    overwrite = FALSE, new.id = NULL, row.names = FALSE, upsert.using = NULL,
    alter.names = FALSE, encoding = NULL, return.pgi = FALSE, df.geom = NULL, geog = FALSE) {
  
    # auto-geog
    if (geom == "geog") geog <- TRUE
  
    if (df.mode) {
      if (!dbExistsTable(conn,name, table.only = TRUE) | overwrite) {
        # set necessary argument values
        partial.match <- FALSE
        new.id <- ".db_pkid"
        row.names <- TRUE
        upsert.using <- NULL
        alter.names <- FALSE
      } else if (!overwrite & dbExistsTable(conn,name, table.only = TRUE)) {
        stop("df.mode = TRUE only allowed for new tables or with overwrite = TRUE.")
      }
    }
  
    dbConnCheck(conn)
    ## Check if PostGIS installed
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    ## Check version for upserts
    if (!is.null(upsert.using)) {
      ver<-dbVersion(conn)
      if (ver[1] < 9 | (ver[1] == 9 && ver[2] < 5)) {
        stop("'Upsert' not supported in your PostgreSQL version (",paste(ver,collapse = "."),
             "). Requires version 9.5 or above.")
      }
    }
    # data.obj class
    cls <- class(data.obj)[1]
    if (cls == "pgi") {
        if (is.null(data.obj$in.table)) {
            stop("Table to insert into not specified (in pgi$in.table). Set this and re-run.")
        } else {
            name <- data.obj$in.table
        }
    }
    ## Check for existing table
    exists.t <- dbExistsTable(conn, name, table.only = TRUE)
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
    geo.classes <- c("SpatialPoints", "SpatialPointsDataFrame", 
        "SpatialLines", "SpatialLinesDataFrame", "SpatialPolygons", 
        "SpatialPolygonsDataFrame")
    pgi <- NULL
    if (cls %in% geo.classes) {
      if (geog) data.obj <- sp::spTransform(data.obj, CRS("+proj=longlat +datum=WGS84 +no_defs", doCheckCRSArgs = FALSE))
        try(suppressMessages(pgSRID(conn, data.obj@proj4string, 
            create.srid = TRUE, new.srid = NULL)), silent = TRUE)
        try(pgi <- pgInsertizeGeom(data.obj, geom, create.table, 
            force.match, conn, new.id, row.names, alter.names, partial.match, df.mode, geog))
    } else if (cls == "data.frame") {
        try(pgi <- pgInsertize(data.obj, create.table, force.match, 
            conn, new.id, row.names, alter.names, partial.match, df.mode))
    } else if (cls == "pgi") {
        pgi <- data.obj
        message("Using previously create pgi object. All arguments except for \"conn\", \"overwrite\", and \"encoding\" will be ignored.")
    } else {
        #dbExecute(conn, "ROLLBACK;")
        stop("Input data object not of correct class - must be a Spatial*, Spatial*DataFrame, or data frame.")
    }
    if (is.null(pgi)) {
        #dbExecute(conn, "ROLLBACK;")
        stop("Table preparation failed. No changes made to database.")
    }
    
    dbExecute(conn, "BEGIN TRANSACTION;")
    ## Change encoding if specified
    if (!is.null(encoding)) {
        pgi$insert.data <- iconv(pgi$insert.data, encoding[1], 
            encoding[2])
    }
    ## Create table if specified
    if (!is.null(pgi$db.new.table)) {
        if (overwrite & exists.t) {
            over.t <- dbDrop(conn, name = name, type = "table", 
                ifexists = TRUE)
            if (!over.t) {
                dbExecute(conn, "ROLLBACK;")
                message("Could not drop existing table. No changes made to database.")
                if (return.pgi) {
                  return(pgi)
                } else {
                  return(FALSE)
                }
            }
        }
        quet <- NULL
        try({
          for (q in pgi$db.new.table) quet <- dbExecute(conn, q)
          })
        if (is.null(quet)) {
            dbExecute(conn, "ROLLBACK;")
            message("Table creation failed. No changes made to database.")
            if (return.pgi) {
                return(pgi)
            } else {
                return(FALSE)
            }
        }
    } else if (is.null(pgi$db.new.table) & overwrite) {
        message("No create table definition in pgi object (pgi$db.new.table);
              not dropping existing table...")
    }
    
    ## Set name of table
    name <- pgi$in.table
    nameque <- dbTableNameFix(conn,name)
    # df with geom add column
    if (!is.null(df.geom)) {
      if (alter.names) df.geom[1]<-tolower(gsub("[+-.,!@$%^&*();/|<>]", "_", df.geom[1]))
      if (length(df.geom) == 1) df.geom <- list(df.geom, NULL) else df.geom <- as.list(df.geom)
      try(dbExecute(conn, paste0("ALTER TABLE ", nameque[1], 
            ".", nameque[2], " ALTER COLUMN ",dbQuoteIdentifier(conn, df.geom[[1]]),
            " TYPE GEOMETRY",df.geom[[2]],";")))
    }
    # end df.geom
    cols <- pgi$db.cols.insert
    values <- pgi$insert.data
    db.cols <- dbTableInfo(conn, name = name)$column_name
    if (is.null(db.cols)) {
        dbExecute(conn, "ROLLBACK;")
        message(paste0("Database table ", paste(name, collapse = "."), 
            " not found; No changes made to database."))
        if (return.pgi) {
            return(pgi)
        } else {
            return(FALSE)
        }
    }
    test <- match(cols, db.cols)
    unmatched <- cols[is.na(test)]
    if (length(unmatched) > 0) {
        dbExecute(conn, "ROLLBACK;")
        message(paste0("The column(s) (", paste(unmatched, collapse = ","), 
            ") are not in the database table. No changes made to database."))
        if (return.pgi) {
            return(pgi)
        } else {
            return(FALSE)
        }
    }
    #upsert
    up.query<-NULL
    if (is.null(pgi$db.new.table) && !is.null(upsert.using)) {
      excl<-dbQuoteIdentifier(conn,pgi$db.cols.insert[!pgi$db.cols.insert %in% upsert.using])
      excl2<-paste(excl, " = excluded.",excl,sep="")
      excl.q<-paste(excl2,collapse = ", ")
      up<-dbQuoteIdentifier(conn,upsert.using)
      if(length(excl) == length(pgi$db.cols.insert)) {
        message("Upserting using constraint name...")
        up.query<-paste0(" ON CONFLICT ON CONSTRAINT ",paste(up,collapse = ",")," DO UPDATE SET ",
                       excl.q) 
        } else {
        message("Upserting using column name(s)...")
        up.query<-paste0(" ON CONFLICT (",paste(up,collapse = ","),") DO UPDATE SET ",
                       excl.q)
        }
    }
    cols2 <- paste0("(", paste(dbQuoteIdentifier(conn,cols), collapse = ","), ")")
    quei <- NULL
    ## Send insert query
    temp.query<-paste0("INSERT INTO ", nameque[1], 
        ".", nameque[2], cols2, " VALUES ", values, up.query,";")
    try(quei <- dbExecute(conn, temp.query))
    if (!is.null(quei)) {
        if (df.mode) {suppressMessages(dbAddKey(conn, name, colname = ".db_pkid", type = "primary"))}
        dbExecute(conn, "COMMIT;")
        message(paste0("Data inserted into table ",nameque[1],".",nameque[2]))
        ## Return TRUE
        if (return.pgi) {
            return(pgi)
        } else {
            return(TRUE)
        }
    } else {
        dbExecute(conn, "ROLLBACK;")
        message("Insert failed. No changes made to database.")
        if (return.pgi) {
            return(pgi)
        } else {
            return(FALSE)
        }
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
