## pgInsertizeGeom

##' Format R data objects for insert into a PostgreSQL table.
##'
##' These are internal rpostgis functions that take an R \code{sp} or \code{sf} object (Spatial*,
##' Spatial*DataFrame or sf; for \code{pgInsertizeGeom}), or data frame (for
##' \code{pgInsertize}) and return a \code{pgi} list object, which can
##' be used in the function \code{pgInsert} to insert rows of the
##' object into the database table. (Note that these functions do not
##' do any modification of the database, it only prepares the data for
##' insert.) The function \code{pgInsert} is a wrapper around these
##' functions, so \code{pgInsertize*} should only be used in
##' situations where data preparation and insert need to be separated.
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
##'
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
##'     table to compare columns of data frame with.  If specified with
##'     \code{partial.match = TRUE}
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
##' @param row.names Whether to add the data frame row names to the 
##'     database table. Column name will be '.R_rownames'.
##' @param alter.names Logical, whether to make database column names
##'     DB-compliant (remove special characters). Default is
##'     \code{TRUE}.  (This should to be set to \code{FALSE} to match
##'     to non-standard names in an existing database table using the
##'     \code{force.match} setting.)
##' @param partial.match Logical; if force.match is set and  true, 
##'     columns in R data frame will be compared with an the 
##'     existing database table \code{name}. Only columns in the 
##'     data frame that exactly match the database
##'     table will be inserted into the database table.
##' @param df.mode Logical; Whether to write data in data frame mode 
##'     (preserving data frame column attributes and row.names).
##'     A new table must be created with this mode (or overwrite set to TRUE),
##'     and the row.names, alter.names, and new.id arguments will
##'     be ignored (see \code{dbWriteDataFrame} for more information.
##' @param geog Logical; Whether to write the spatial data as a PostGIS 
##' 'GEOGRAPHY' type.
##' @author David Bucklin \email{david.bucklin@@gmail.com}
##' @keywords internal
##' @importFrom stats na.omit
##' @importFrom sf st_geometry_type st_as_sf st_drop_geometry st_crs st_as_text st_as_binary st_geometry
##' @importFrom DBI dbDriver
##' @return pgi A list containing four character strings: (1)
##'     in.table, the table name which will be created or inserted
##'     into, if specified by either create.table or force.match (else
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

pgInsertizeGeom <- function(data.obj, geom = "geometry", create.table = NULL,
                            force.match = NULL, conn = NULL, new.id = NULL, row.names = FALSE,
                            alter.names = FALSE, partial.match = FALSE, df.mode = FALSE, geog = FALSE) {
  
  ## Convert to sf object while sp available
  # if (!inherits(data.obj, "sf")) data.obj <- sf::st_as_sf(data.obj)
  
  ## Check if geometry is simple or multi
  # if (grepl("^MULTI", as.character(sf::st_geometry_type(data.obj, by_geometry = FALSE)))) {
  #   multi <- TRUE
  # } else {
  #   multi <- FALSE
  # }
  
  ## Extract data frame 
  try(dat <- sf::st_drop_geometry(data.obj), silent = TRUE)
  
  ## New ID column which will be primary key in the database
  gid <- 1:nrow(data.obj)
  
  ## If data frame doesn't exist populate it with GID
  if (length(colnames(dat)) == 0) {
    dat <- data.frame(gid = gid)
    if (!is.null(new.id)) {
      names(dat)[1] <- new.id
    }
    message(paste0("No data frame; creating sequential id column (",
                   colnames(dat), ")."))
  } else if (!is.null(new.id)) {
    ## else if it exists, add GID if new.id is not null
    if (new.id %in% colnames(dat)) {
      stop(paste0("'", new.id, "' is already a column name in the data frame.\nPick a unique name for new.id or leave it null (no new ID created)."))
    }
    dat <- cbind(gid, dat)
    names(dat)[1] <- new.id
  }
  
  
  ## Manage alter.names = TRUE. This modifies column names to be DB compliant
  replace   <- "[+-.,!@$%^&*();/|<>]"
  in.tab    <- NULL
  new.table <- NULL
  ## Make db compliant names
  if (alter.names) {
    message("Making column names DB-compliant (lowercase and replacing special characters with '_').")
    t.names <- tolower(gsub(replace, "_", colnames(dat)))
    colnames(dat) <- t.names
    geom <- tolower(gsub(replace, "_", geom))
  }
  
  ## Add .R_rownames if specified to save row names as new column in the database
  if (row.names) {
    dat <- data.frame(dat, .R_rownames = attr(dat, "row.names"), stringsAsFactors = FALSE)
  }
  
  ## Handle projections - first check if connection given; if so,
  ## try to resolve SRID if it doesn't exist, try to create (if no
  ## writing for user on spatial_ref_sys, will fail quietly)
  proj <- NULL
  if (!is.null(conn)) {
    try(suppressMessages(proj <- pgSRID(conn, sf::st_crs(data.obj), 
                                        create.srid = TRUE)), silent = TRUE)
  }
  ## If user didn't specify conn, or pgSRID failed, try to get EPSG
  if (is.null(proj)) {
    ## Extract proj
    try(proj <- sf::st_crs(data.obj, parameters = T)$epsg, 
        silent = TRUE)
    ## Turn NA if still NULL
    if (is.null(proj)) proj <- NA
  } 
  
  ## Throw error if none of create table and force match is NULL
  if (!is.null(create.table) & !is.null(force.match)) {
    stop("Either create.table or force.match must be null.")
  }
  
  ## When create.table != NULL, create it
  if (!is.null(create.table)) {
    ## Get schema.name
    nt <- dbTableNameFix(conn, create.table)
    in.tab <- create.table
    
    ## Make create table statement
    new.table <- dbBuildTableQuery(conn, name = in.tab,
                                   obj = dat, row.names = FALSE)
    
    ## Create and append add geometry field statement. Create
    ## match table
    typematch <- data.frame(
      sf = c("POINT", "LINESTRING", "POLYGON", "MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON",
             "CURVE", "MULTICURVE", "SURFACE", "MULTISURFACE", "GEOMETRYCOLLECTION",
             "COMPOUNDCURVE", "CURVEPOLYGON"), 
      pgis = c("Point","LineString", "Polygon", "MultiPoint", "MultiLineString", "MultiPolygon",
               "Curve", "MultiCurve", "Surface", "MultiSurface", "GeometryCollection",
               "CompoundCurve", "CurvePolygon"), 
      stringsAsFactors = FALSE)
    g.typ <- as.character(sf::st_geometry_type(data.obj, by_geometry = FALSE))
    sptype <- pmatch(typematch$sf, g.typ)
    pgtype <- stats::na.omit(typematch$pgis[sptype == 1])[1]
    
    ## Append projection if it exists
    if (!is.null(proj[1])) {
      pgtype <- paste0(pgtype, ",", proj[1])
    }
    
    ## Manage query for geometry/geography
    if (!geog) {
      add.geom <- paste0("ALTER TABLE ", nt[1], ".", nt[2],
                         " ADD COLUMN ", geom, " geometry(", pgtype, ");") 
    } else {
      add.geom <- paste0("ALTER TABLE ", nt[1], ".", nt[2],
                         " ADD COLUMN ", geom, " geography(", pgtype, ");") 
    }
    new.table <- c(new.table, add.geom)
    
    ## Data frame mode ------- Review df.mode TRUE
    if (df.mode) {
      # add geom column with attribute proj4string
      eval(parse(text = paste0("dat<-data.frame(dat,.rpostgis.geom.",geom," = 1)")))
      eval(parse(text = paste0("dat$.rpostgis.geom.",geom," <- '",
                               as.character(sf::st_crs(data.obj)$proj4string),"'")))
      dat <- try(dbWriteDataFrame(conn, in.tab, dat , only.defs = TRUE)[,1:length(names(dat)) - 1], silent = TRUE )# dat.na not used further
    } else {
      # Remove existing defs if table exists (defs for the new table)
      if (dbExistsTable(conn, ".R_df_defs", table.only = TRUE)) {
        sql_query <- paste0("DELETE FROM \".R_df_defs\" WHERE table_nm = ",
                            dbQuoteString(conn, in.tab[length(in.tab)]),";")
        dbExecute(conn, sql_query)
      }
    }
  }
  
  ## Manage when force match is or is not NULL ----- Review force.match NULL
  if (!is.null(force.match)) {
    if (is.null(conn)) {
      stop("Database connection must be specified when using force.match.")
    }
    ## Name fixing
    nt      <- dbTableNameFix(conn, force.match)
    in.tab  <- force.match
    db.cols <- dbTableInfo(conn, name = in.tab)$column_name
    if (is.null(db.cols)) {
      stop(paste0("Database table ", paste(nt, collapse = "."),
                  " not found."))
    }
    if (is.na(match(geom, db.cols))) {
      stop("Geometry column name not found in database table.")
    }
    rcols          <- colnames(dat)
    db.cols.match  <- db.cols[!is.na(match(db.cols, rcols))]
    db.cols.insert <- c(db.cols.match, geom)
    
    ## Reorder data frame columns
    dat <- dat[db.cols.match]
    
    ## stop if colnames do not all match and partial.match = FALSE
    if (!(length(colnames(dat)) == length(rcols)) & !partial.match)  {
      stop(paste0((length(rcols) - length(colnames(dat))),
                  " column(s) in data frame are missing in database table (",
                  paste(rcols[is.na(match(rcols, colnames(dat)))],collapse = ", "),"). Rename data frame columns 
            or set partial.match = TRUE to only insert to matching colunns."))
    }
    
    message(paste0(length(colnames(dat)), " out of ", length(rcols),
                   " columns of the data frame match database table columns and will be formatted."))
  } else {
    db.cols.insert <- c(colnames(dat), geom)
  }
  
  ## Data string open and close characters (NULLed when only
  ## geometry is inserted)
  open  <- "'"
  close <- "',"
  
  ## Conversion of geometries -------------- TRY ALL
  # point ok
  # Multipolygon ok
  # multiline and line ok
  if (geog) cast <- "::geography" else cast <- NULL
  
  ## Get geometry in WKT format
  geom.1 <- sf::st_as_text(sf::st_geometry(data.obj))
  
  ## Get data frame ready for database
  if (length(colnames(dat)) == 0) {
    df    <- data.frame(geom.1)
    open  <- NULL
    close <- NULL
  } else {
    df    <- cbind(dat, geom.1)
  }
  df[] <- lapply(df, as.character)
  
  ## Set all NA to NULL
  df[is.na(df)] <- "NULL"
  ## Double all single ' to escape. Format rows of data frame
  if (!is.na(proj[1])) {
    if (multi == TRUE) {
      d1 <- apply(df, 1, function(x) paste0("(", open,
                                            toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                                                                               1], fixed = TRUE), collapse = "','")), close,
                                            "ST_Multi(ST_GeomFromText('", x[length(colnames(df))],
                                            "',", proj[1], "))", cast, ")"))
    } else {
      d1 <- apply(df, 1, function(x) paste0("(", open,
                                            toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                                                                               1], fixed = TRUE), collapse = "','")), close,
                                            "ST_GeomFromText('", x[length(colnames(df))],
                                            "',", proj[1], ")", cast, ")"))
    }
  } else {
    warning("Spatial projection is unknown/unsupported and will be NA in insert object (SRID = 0).")
    if (multi == TRUE) {
      d1 <- apply(df, 1, function(x) paste0("(", open,
                                            toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                                                                               1], fixed = TRUE), collapse = "','")), close,
                                            "ST_Multi(ST_GeomFromText('", x[length(colnames(df))],
                                            "'))", cast, ")"))
    } else {
      d1 <- apply(df, 1, function(x) paste0("(", open,
                                            toString(paste(gsub("'", "''", x[1:length(colnames(df)) -
                                                                               1], fixed = TRUE), collapse = "','")), close,
                                            "ST_GeomFromText('", x[length(colnames(df))],
                                            "')", cast, ")"))
    }
  }
  
  ## Fix NULLs, collapse with commas, and generate the pgi object
  d1 <- gsub("'NULL'", "NULL", d1)
  d1 <- paste(d1, collapse = ",")
  lis <- list(in.table = in.tab, db.new.table = new.table,
              db.cols.insert = db.cols.insert, insert.data = d1)
  class(lis) <- "pgi"
  return(lis)
}

## pgInsertize

##' @rdname pgInsertizeGeom
##' @keywords internal
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
                        conn = NULL, new.id = NULL, row.names = FALSE, alter.names = FALSE,
                        partial.match = FALSE, df.mode = FALSE) {
  ## Must be data frame
  if (!is.data.frame(data.obj)) {
    stop("data.obj must be a data frame.")
  }
  
  ## Throw error if none of create table and force match is NULL
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
      stop(paste0("'", new.id, "' is already a column name in the data frame.\nPick a unique name for new.id or leave it null (no new ID created)."))
    }
    id.num <- 1:nrow(data.obj)
    data.obj <- cbind(id.num, data.obj)
    names(data.obj)[1] <- new.id
  }
  
  ## Make DB compliant names
  if (alter.names) {
    message("Making column names DB-compliant (lowercase and replacing special characters with '_').")
    t.names <- tolower(gsub(replace, "_", colnames(data.obj)))
    colnames(data.obj) <- t.names
  }
  
  ## Add .R_rownames if specified to save row names as new column in the database
  if (row.names) {
    data.obj <- data.frame(data.obj, .R_rownames = attr(data.obj, "row.names"), stringsAsFactors = FALSE)
  }
  
  ## When create.table != NULL, create it
  if (!is.null(create.table)) {
    ## Get schema.name
    nt <- dbTableNameFix(conn,create.table)
    in.tab <- create.table
    ## Make create table statement
    new.table <- dbBuildTableQuery(conn, name = in.tab, 
                                   obj = data.obj, row.names = FALSE)
    
    ## Data frame mode ------- Review df.mode TRUE
    if (df.mode) {
      data.obj <- dbWriteDataFrame(conn, in.tab, data.obj, only.defs = TRUE)
    } else {
      ## Remove existing defs if table exists (defs for the new table)
      if (dbExistsTable(conn, ".R_df_defs", table.only = TRUE)) {
        sql_query <- paste0("DELETE FROM \".R_df_defs\" WHERE table_nm = ",
                            dbQuoteString(conn, in.tab[length(in.tab)]),";")
        dbExecute(conn, sql_query)
      }
    }
  }
  
  ## Match columns to DB table if set
  if (!is.null(force.match)) {
    if (is.null(conn)) {
      stop("Database connection must be specified when using force.match.")
    }
    ## Name fixing
    nt      <- dbTableNameFix(conn,force.match)
    in.tab  <- force.match
    db.cols <- dbTableInfo(conn, name = in.tab)$column_name
    if (is.null(db.cols)) {
      stop(paste0("Database table ", paste(nt, collapse = "."), 
                  " not found."))
    }
    rcols          <- colnames(data.obj)
    db.cols.match  <- db.cols[!is.na(match(db.cols, rcols))]
    db.cols.insert <- db.cols.match
    ## Reorder data frame columns
    data.obj <- data.obj[db.cols.match]
    if (length(colnames(data.obj)) == 0) {
      stop("No column name matches found in database table.")
    }
    
    ## stop if colnames do not all match and partial.match = FALSE
    if (!(length(colnames(data.obj)) == length(rcols)) & 
        !partial.match) {
      stop(glue::glue("{length(rcols - length(colnames(data.obj)))} column(s) in data frame are missing in
           database table ({paste(rcols[is.na(match(rcols, colnames(data.obj)))], collapse = ', ')}).
           Rename data frame columns or set partial.match = TRUE to only insert the matching columns."))
    }
    
    message(glue::glue("{length(colnames(data.obj))} out of {length(rcols)} columns of the data frame match database table columns
           and will be formatted for database insert."))
  } else {
    db.cols.insert <- colnames(data.obj)
  }
  
  ## Set NULL to user-specified NA value
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
