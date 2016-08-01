## pgInsertizeGeom

##' These functions take an R \code{sp} object (Spatial* or
##' Spatial*DataFrame; for \code{pgInsertizeGeom}) or data frame (for
##' \code{pgInsertize}) and return a \code{pgi} list object, which can
##' be used in the function \code{pgInsert} to insert rows of the
##' object into the database table. (Note that these functions do not
##' do any modification of the database, it only prepares the data for
##' insert.) The function \code{pgInsert} is a wrapper around these
##' functions, so \code{pgInsertize*} should only be used in
##' situations where data preparation and insert need to be seperated.
##'
##' The entire data frame is prepared by default, unless
##' \code{force.match} specifies a database table (along with a
##' database connection \code{conn}), in which case the R column names
##' are compared to the \code{force.match} column names, and only
##' exact matches are formatted to be inserted.
##'
##' A new database table can also be prepared to be created using the
##' \code{create.table} argument.  If \code{new.id} is specified, a
##' new sequential integer field is added to the data frame.  For
##' \code{Spatial*}-only objects (no data frame), a new.id is created
##' by default with name \code{gid}.  For \code{pgInsertizeGeom}, if
##' the R package \code{wkb} is installed, this function uses
##' \code{writeWKB} to translate the geometries for some spatial types
##' (faster with large datasets), otherwise the \code{rgeos} function
##' \code{writeWKT} is used.
##'
##' @title Format R data objects (data frames, spatial data frames, or
##'     spatial-only objects) for insert into a PostgreSQL table (for
##'     use with pgInsert).
##' @param data.obj A Spatial* or Spatial*DataFrame, or data frame for
##'     \code{pgInsertize}.
##' @param geom character string, the name of geometry column in the
##'     database table. (existing or to be created; defaults to
##'     'geom').
##' @param create.table character, schema and table of the PostgreSQL
##'     table to create (actual table creation will be done in later
##'     in pgInsert().) Column names will be converted to
##'     PostgreSQL-compliant names. Default is \code{NULL} (no new
##'     table created).
##' @param force.match character, schema and table of the PostgreSQL
##'     table to compare columns of data frame with.  If specified,
##'     only columns in the data frame that exactly match the database
##'     table will be kept, and reordered to match the database
##'     table. If \code{NULL}, all columns will be kept in the same
##'     order given in the data frame.
##' @param conn A database connection (if a table is given in for
##'     "force.match" parameter)
##' @param new.id character, name of a new sequential integer ID
##'     column to be added to the table.  (for spatial objects without
##'     data frames, this column is created even if left \code{NULL}
##'     and defaults to the name \code{"gid"}).
##' @param alter.names Logical, whether to make database column names
##'     DB-compliant (remove special characters). Defualt is
##'     \code{TRUE}.  (This should to be set to \code{FALSE} to match
##'     to non-standard names in an existing database table using the
##'     \code{force.match} setting.)
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @importFrom stats na.omit
##' @importFrom rgeos writeWKT
##' @export
##' @return pgi A list containing four character strings: (1)
##'     in.table, the table name which will be created or inserted
##'     into, if specifed by either create.table or force.match (else
##'     NULL) (2) db.new.table, the SQL statement to create the new
##'     table, if specified in create.table (else NULL), (3)
##'     db.cols.insert, a character string of the database column
##'     names to insert into, and (4) insert.data, a character string
##'     of the data to insert. See examples for usage within the
##'     \code{pgInsert} function.
##' @examples
##' \dontrun{
##' library(sp)
##' data(meuse)
##' coords <- SpatialPoints(meuse[, c("x", "y")])
##' spdf <- SpatialPointsDataFrame(coords, meuse)
##'
##' ## Format data for insert
##' pgi.new <- pgInsertizeGeom(spdf, geom = "point_geom", create.table = c("schema",
##'     "table"), new.id = "pt_gid")
##' print(pgi.new)
##'
##' ## Insert data in database table (note that an error will be given if
##' ## all insert columns do not have exactly matching database table
##' ## columns)
##' pgInsert(conn = conn, data.obj = pgi.new)
##'
##' ## Inserting into existing table
##' pgi.existing <- pgInsertizeGeom(spdf, geom = "point_geom", force.match = c("schema",
##'     "table"), conn = conn)
##' ## A warning message is given, since the "dist.m" column is not found
##' ## in the database table (it was changed to "dist_m" in pgi.new to
##' ## make name DB-compliant). All other columns are prepared for insert.
##' print(pgi.existing)
##'
##' pgInsert(conn = conn, data.obj = pgi.existing)
##' }

pgInsertizeGeom <- function(data.obj, geom = "geom", create.table = NULL,
    force.match = NULL, conn = NULL, new.id = NULL, alter.names = TRUE) {
    ## Load wkb package if available
    wkb.t <- suppressWarnings(require("wkb", quietly = TRUE))
    ## wkb.t <- FALSE Check multi
    mx <- 1
    ## If points
    try(mx <- max(unlist(lapply(sapply(slot(data.obj, "polygons"),
        slot, "Polygons"), function(x) {
        length(x)
    }))), silent = TRUE)
    try(mx <- max(unlist(lapply(sapply(slot(data.obj, "lines"),
        slot, "Lines"), function(x) {
        length(x)
    }))), silent = TRUE)
    if (mx > 1) {
        multi <- TRUE
    } else {
        multi <- FALSE
    }
    ## If spatial*dataframe, extract data frame
    dat <- data.frame()
    try(dat <- data.obj@data, silent = TRUE)
    gid <- 1:length(data.obj)
    ## If data frame doesn't exist (not a spatial*dataframe object),
    ## populate it with seq. id
    if (length(colnames(dat)) == 0) {
        dat <- data.frame(gid = gid)
        if (!is.null(new.id)) {
            names(dat)[1] <- new.id
        }
        message(paste0("No data frame; creating sequential id column (",
            colnames(dat), ")."))
    } else {
        ## else if it exists, add seq. id if new.id is not null
        if (!is.null(new.id)) {
            if (new.id %in% colnames(dat)) {
                stop(paste0("'", new.id, "' is already a column name in the data frame. Pick a unique name for new.id or leave it null (no new ID created)."))
            }
            dat <- cbind(gid, dat)
            names(dat)[1] <- new.id
        }
    }
    replace <- "[+-.,!@$%^&*();/|<>]"
    in.tab <- NULL
    new.table <- NULL
    ## Make db compliant names
    if (alter.names) {
        message("Making column names DB-compliant (lowercase and replacing special characters with '_').")
        ## Make column names DB-compliant
        t.names <- tolower(gsub(replace, "_", colnames(dat)))
        colnames(dat) <- t.names
        geom <- tolower(gsub(replace, "_", geom))
    }
    ## Handle projections - first check if connection given; if so,
    ## try to resolve SRID if it doesn't exist, try to create (if no
    ## writing for user on spatial_ref_sys, will fail quietly)
    proj <- NULL
    if (!is.null(conn)) {
        try(proj <- pgSRID(data.obj@proj4string, conn = conn,
            create = TRUE), silent = TRUE)
    }
    ## If (user didn't specify conn, or pgSRID failed) AND rgdal is
    ## installed, then try to get EPSG
    if (is.null(proj) & suppressWarnings(require("rgdal", quietly = TRUE))) {
        ## Extract proj
        proj <- "OGRERR_UNSUPPORTED_SRS"
        try(proj <- rgdal::showEPSG(as.character(data.obj@proj4string)),
            silent = TRUE)
        if (proj == "OGRERR_UNSUPPORTED_SRS") {
            proj <- NA
        }
    }
    if (!is.null(create.table) & !is.null(force.match)) {
        stop("Either create.table or force.match must be null.")
    }
    if (!is.null(create.table)) {
        nt <- dbTableNameFix(create.table)
        drv <- dbDriver("PostgreSQL")
        ## Make create table statement
        new.table <- postgresqlBuildTableDefinition(drv, name = gsub("\"",
            "", nt), obj = dat, row.names = FALSE)
        in.tab <- nt
        ## Create and append add geometry field statement. Create
        ## match table
        typematch <- data.frame(sp = c("SpatialPoints", "SpatialLines",
            "SpatialPolygons", "SpatialMultiPoints"), pgis = c("Point",
            "LineString", "Polygon", "Point"), stringsAsFactors = FALSE)
        g.typ <- class(data.obj)[1]
        sptype <- pmatch(typematch$sp, g.typ)
        pgtype <- stats::na.omit(typematch$pgis[sptype == 1])[1]
        if (multi) {
            pgtype <- paste0("Multi", pgtype)
        }
        if (!is.na(proj)) {
            pgtype <- paste0(pgtype, ",", proj)
        }
        add.geom <- paste0("ALTER TABLE ", in.tab[1], ".", in.tab[2],
            " ADD COLUMN ", geom, " geometry(", pgtype, ");")
        new.table <- paste0(new.table, "; ", add.geom)
    }
    if (!is.null(force.match)) {
        if (is.null(conn)) {
            stop("Database connection must be specified when using force.match.")
        }
        ## Name fixing
        nt <- dbTableNameFix(force.match)
        in.tab <- nt
        db.cols <- dbColumnInfo(conn, name = gsub("\"", "", nt))$column_name
        if (is.null(db.cols)) {
            stop(paste0("Database table ", paste(nt, collapse = "."),
                " not found."))
        }
        if (is.na(match(geom, db.cols))) {
            stop("Geometry column name not found in database table.")
        }
        rcols <- colnames(dat)
        db.cols.match <- db.cols[!is.na(match(db.cols, rcols))]
        db.cols.insert <- c(db.cols.match, geom)
        ## Reorder data frame columns
        dat <- dat[db.cols.match]
        message(paste0(length(colnames(dat)), " out of ", length(rcols),
            " columns of the data frame match database table columns and will be formatted."))
    } else {
        db.cols.insert <- c(colnames(dat), geom)
    }
    ## Data string open and close characters (NULLed when only
    ## geometry is inserted)
    open <- "'"
    close <- "',"
    if (!wkb.t | multi | class(data.obj)[1] %in% c("SpatialLines",
        "SpatialLinesDataFrame")) {
        ## wkt conversion, multipolygons not handled correctly by wkb
        ## at this time, and wkb only outputs MULTILINESTRINGS (so all
        ## linestrings are sent using WKT)
        message("Using writeWKT from rgeos package...")
        geom.1 <- rgeos::writeWKT(data.obj, byid = TRUE)
        if (length(colnames(dat)) == 0) {
            df <- data.frame(geom.1)
            open <- NULL
            close <- NULL
        } else {
            df <- cbind(dat, geom.1)
        }
        df[] <- lapply(df, as.character)
        ## Set all NA to NULL
        df[is.na(df)] <- "NULL"
        ## Double all single ' to escape. Format rows of data frame
        if (!is.na(proj)) {
            if (multi == TRUE) {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "ST_Multi(ST_GeomFromText('", x[length(colnames(df))],
                  "',", proj, ")))"))
            } else {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "ST_GeomFromText('", x[length(colnames(df))],
                  "',", proj, "))"))
            }
        } else {
            warning("Spatial projection is unknown/unsupported and will be NA in insert object (SRID = 0).")
            if (multi == TRUE) {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "ST_Multi(ST_GeomFromText('", x[length(colnames(df))],
                  "')))"))
            } else {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "ST_GeomFromText('", x[length(colnames(df))],
                  "'))"))
            }
        }
    } else {
        ## wkb conversion
        message("Using writeWKB from wkb package...")
        geom.1 <- unlist(lapply(wkb::writeWKB(data.obj), function(x) {
            paste(x, collapse = "")
        }))
        if (length(colnames(dat)) == 0) {
            df <- data.frame(geom.1)
            open <- NULL
            close <- NULL
        } else {
            df <- cbind(dat, geom.1)
        }
        df[] <- lapply(df, as.character)
        ## Set all NA to NULL
        df[is.na(df)] <- "NULL"
        ## Double all single ' to escape. Format rows of data frame.
        if (!is.na(proj)) {
            if (multi == TRUE) {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "ST_Multi(ST_SetSRID('", x[length(colnames(df))],
                  "'::geometry,", proj, ")))"))
            } else {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "ST_SetSRID('", x[length(colnames(df))], "'::geometry,",
                  proj, "))"))
            }
        } else {
            warning("Spatial projection is unknown/unsupported and will be NA in insert object (SRID = 0).")
            if (multi == TRUE) {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "ST_Multi('", x[length(colnames(df))], "'))"))
            } else {
                d1 <- apply(df, 1, function(x) paste0("(", open,
                  toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                    1], fixed = TRUE), collapse = "','")), close,
                  "'", x[length(colnames(df))], "')"))
            }
        }
    }
    d1 <- gsub("'NULL'", "NULL", d1)
    d1 <- paste(d1, collapse = ",")
    lis <- list(in.table = in.tab, db.new.table = new.table,
        db.cols.insert = db.cols.insert, insert.data = d1)
    class(lis) <- "pgi"
    return(lis)
}



## pgInsertize

##' @rdname pgInsertizeGeom
##' @export
##' @examples
##' \dontrun{
##' ## Format regular (non-spatial) data frame for insert using
##' ## pgInsertize connect to database
##' data <- data.frame(a = c(1, 2, 3), b = c(4, NA, 6), c = c(7,
##'     "text", 9))
##'
##' ## Format non-spatial data frame for insert
##' values <- pgInsertize(data.obj = data)
##'
##' ## Insert data in database table (note that an error will be given if
##' ## all insert columns do not match exactly to database table columns)
##' pgInsert(conn, data.obj = values, name = c("schema", "table"))
##'
##' ## Run with forced matching of database table column names
##' values <- pgInsertize(data.obj = data, force.match = c("schema",
##'     "table"), conn = conn)
##'
##' pgInsert(conn, data.obj = values)
##' }

pgInsertize <- function(data.obj, create.table = NULL, force.match = NULL,
    conn = NULL, new.id = NULL, alter.names = TRUE) {
    if (!is.data.frame(data.obj)) {
        stop("data.obj must be a data frame.")
    }
    replace <- "[+-.,!@$%^&*();/|<>]"
    in.tab <- NULL
    new.table <- NULL
    if (!is.null(create.table) & !is.null(force.match)) {
        stop("Either create.table or force.match must be null.")
    }
    ## Add new ID column if new.id is set
    if (!is.null(new.id)) {
        ## Check if new.id column name is already in data frame
        if (new.id %in% colnames(data.obj)) {
            stop(paste0("'", new.id, "' is already a column name in the data frame. Pick a unique name for new.id or leave it null (no new ID created)."))
        }
        id.num <- 1:length(data.obj[, 1])
        data.obj <- cbind(id.num, data.obj)
        names(data.obj)[1] <- new.id
    }
    ## Make DB compliant names
    if (alter.names) {
        message("Making column names DB-compliant (lowercase and replacing special characters with '_').")
        t.names <- tolower(gsub(replace, "_", colnames(data.obj)))
        colnames(data.obj) <- t.names
    }
    ## Create new table statement if set
    if (!is.null(create.table)) {
        nt <- dbTableNameFix(create.table)
        drv <- dbDriver("PostgreSQL")
        ## Make create table statement
        new.table <- postgresqlBuildTableDefinition(drv, name = gsub("\"",
            "", nt), obj = data.obj, row.names = FALSE)
        in.tab <- nt
    }
    ## Match columns to DB table if set
    if (!is.null(force.match)) {
        if (is.null(conn)) {
            stop("Database connection must be specified when using force.match.")
        }
        ## Name fixing
        nt <- dbTableNameFix(force.match)
        in.tab <- nt
        db.cols <- dbColumnInfo(conn, name = gsub("\"", "", nt))$column_name
        if (is.null(db.cols)) {
            stop(paste0("Database table ", paste(nt, collapse = "."),
                " not found."))
        }
        rcols <- colnames(data.obj)
        db.cols.match <- db.cols[!is.na(match(db.cols, rcols))]
        db.cols.insert <- db.cols.match
        ## Reorder data frame columns
        data.obj <- data.obj[db.cols.match]
        if (length(colnames(data.obj)) == 0) {
            stop("No column name matches found in database table.")
        }
        message(paste0(length(colnames(data.obj)), " out of ",
            length(rcols), " columns of the data frame match database table columns and will be formatted for database insert."))
    } else {
        db.cols.insert <- colnames(data.obj)
    }
    ## Set NA to user-specified NULL value
    data.obj[] <- lapply(data.obj, as.character)
    data.obj[is.na(data.obj)] <- "NULL"
    ## Format rows of data frame
    d1 <- apply(data.obj, 1, function(x) paste0("('", toString(paste(gsub("'",
        "''", x, fixed = TRUE), collapse = "','")), "')"))
    d1 <- gsub("'NULL'", "NULL", d1)
    d1 <- paste(d1, collapse = ",")
    ## Create pgi object
    lis <- list(in.table = in.tab, db.new.table = new.table,
        db.cols.insert = db.cols.insert, insert.data = d1)
    class(lis) <- "pgi"
    return(lis)
}



## print.pgi

##' @rdname pgInsertizeGeom
##' @param x A list of class \code{pgi}, output from the pgInsertize()
##'     or pgInsertizeGeom() functions from the rpostgis package.
##' @param ... Further arguments not used.
##' @export
print.pgi <- function(x, ...) {
    cat("pgi object: PostgreSQL insert object from pgInsertize* function in rpostgis. Use with pgInsert() to insert into database table.")
    cat("\n************************************\n")
    if (!is.null(pgi$in.tab)) {
        cat(paste0("Insert table: ", paste(pgi$in.tab, collapse = ".")))
        cat("\n************************************\n")
    }
    if (!is.null(pgi$db.new.table)) {
        cat(paste0("SQL to create new table: ", pgi$db.new.table))
        cat("\n************************************\n")
    }
    cat(paste0("Columns to insert into: ", paste(pgi$db.cols.insert,
        collapse = ",")))
    cat("\n************************************\n")
    cat(paste0("Formatted insert data: ", substr(pgi$insert.data,
        0, 1000)))
    if (nchar(pgi$insert.data) > 1000) {
        cat("........Only the first 1000 characters shown")
    }
}
