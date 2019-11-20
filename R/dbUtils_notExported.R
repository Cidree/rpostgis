## dbTableNameFix

##' Format input for database schema/table names.
##'
##' Internal rpostgis function to return common (length = 2) schema
##' and table name vector from various table and schema + table name
##' inputs.
##' 
##' @param conn A connection object. Must be provided but can be set NULL,
##' where a dummy connection will be used.
##' @param t.nm Table name string, length 1-2.
##' @param as.identifier Boolean whether to return (schema,table) name as database
##' sanitized identifiers (TRUE) or as regular character (FALSE)
##' @return character vector of length 2. Each character element is in
##'     (escaped) double-quotes when as.identifier = TRUE.
##' @keywords internal
##' @importFrom DBI dbQuoteIdentifier
##' @importFrom DBI dbQuoteString
##' @examples
##' \dontrun{
##' name<-c("schema","table")
##' dbTableNameFix(conn,name)
##' 
##' #current search path schema is added to single-length character object (if only table is given)
##' name<-"table"
##' dbTableNameFix(conn,name)
##' 
##' #schema or table names with double quotes should be given exactly as they are 
##' (make sure to wrap in single quotes in R):
##' name<-c('sch"ema','"table"')
##' dbTableNameFix(conn,name)
##' }

dbTableNameFix <- function(conn=NULL, t.nm, as.identifier = TRUE) {
    ## case of no schema provided
      if (length(t.nm) == 1 && !is.null(conn) && !inherits(conn, what = "AnsiConnection")) {
        schemalist<-dbGetQuery(conn,"select nspname as s from pg_catalog.pg_namespace;")$s
        user<-dbGetQuery(conn,"SELECT current_user as user;")$user
        schema<-dbGetQuery(conn,"SHOW search_path;")$search_path
        schema<-gsub(" ","",unlist(strsplit(schema,",",fixed=TRUE)),fixed=TRUE)
        # use user schema if available
        if ("\"$user\"" == schema[1] && user %in% schemalist) {
          sch<-user
        } else {
          sch<-schema[!schema=="\"$user\""][1]
        } 
        t.nm <- c(sch, t.nm)
      }
      if (length(t.nm) > 2)
      {
        stop("Invalid PostgreSQL table/view name. Must be provided as one ('table') or two-length c('schema','table') character vector.")
      }
    if (is.null(conn)) {conn<-DBI::ANSI()}
    if (!as.identifier) {return(t.nm)} else {
    t.nm<-DBI::dbQuoteIdentifier(conn, t.nm)
    return(t.nm)
    }
}

## dbVersion

##' Returns major.minor version of PostgreSQL (for version checking)
##'
##' @param conn A PostgreSQL connection
##' @return numeric vector of length 3 of major,minor,bug version.
##' @keywords internal

dbVersion<- function (conn) {
      pv<-dbGetQuery(conn,"SHOW server_version;")$server_version
      nv<-unlist(strsplit(pv,".",fixed=TRUE))
      return(as.numeric(nv))
}


## dbBuildTableQuery
##' Builds CREATE TABLE query for a data frame object.
##' 
##' @param conn A PostgreSQL connection
##' @param name Table name string, length 1-2.
##' @param obj A data frame object.
##' @param field.types optional named list of the types for each field in \code{obj}
##' @param row.names logical, should row.name of \code{obj} be exported as a row_names field? Default is FALSE
##' 
##' @note Adapted from RPostgreSQL::postgresqlBuildTableDefinition
##' @keywords internal

dbBuildTableQuery <- function (conn = NULL, name, obj, field.types = NULL, row.names = FALSE) {
    if (is.null(conn)) {
      conn <- DBI::ANSI()
      nameque <- dbQuoteIdentifier(conn,name)
    } else {
      nameque<-paste(dbTableNameFix(conn, name),collapse = ".")
    }
  
    if (!is.data.frame(obj)) 
        obj <- as.data.frame(obj)
    if (!is.null(row.names) && row.names) {
        obj <- cbind(row.names(obj), obj)
        names(obj)[1] <- "row_names"
    }
    if (is.null(field.types)) {
        field.types <- sapply(obj, dbDataType, dbObj = conn)
    }
    i <- match("row_names", names(field.types), nomatch = 0)
    if (i > 0) 
        field.types[i] <- dbDataType(conn, row.names(obj))
    flds <- paste(dbQuoteIdentifier(conn ,names(field.types)), field.types)
    
    paste("CREATE TABLE ", nameque , "\n(", paste(flds, 
        collapse = ",\n\t"), "\n);")
}

## dbExistsTable
##' Check if a PostgreSQL table/view exists
##' 
##' @param conn A PostgreSQL connection
##' @param name Table/view name string, length 1-2.
##' 
##' @keywords internal

dbExistsTable <- function (conn, name, table.only = FALSE) {
    if (!table.only) to<-NULL else to<-" AND table_type = 'BASE TABLE'"
    full.name<-dbTableNameFix(conn,name, as.identifier = FALSE)
    chk<-dbGetQuery(conn, paste0("SELECT 1 FROM information_schema.tables 
               WHERE table_schema = ",dbQuoteString(conn,full.name[1]),
               " AND table_name = ",dbQuoteString(conn,full.name[2]),to,";"))[1,1]
    if (length(chk) == 1 && is.na(chk)) chk <- NULL
    if (is.null(chk)) {
      exists.t <- FALSE
      # check version (matviews >= 9.3)
      ver<-dbVersion(conn)
      if (!table.only & !(ver[1] < 9 | (ver[1] == 9 && ver[2] < 3))) {
        # matview case - not in information_schema
        chk2<-dbGetQuery(conn, paste0("SELECT oid::regclass::text, relname
                FROM pg_class
                WHERE relkind = 'm'
                AND relname = ",dbQuoteString(conn,full.name[2]),";"))
        if (length(names(chk2)) > 0) {
          sch<-gsub(paste0(".",chk2[1,2]),"",chk2[,1])
          if (full.name[1] %in% sch) exists.t<-TRUE else exists.t<-FALSE
        } else {
          exists.t<-FALSE
        }
      }
    } else {
    exists.t<-TRUE
    }
  return(exists.t)
}


## dbConnCheck
##' Check if a supported PostgreSQL connection
##' 
##' @param conn A PostgreSQL connection
##' 
##' @keywords internal

dbConnCheck <- function(conn) {
  if (inherits(conn, c("PostgreSQLConnection")) | inherits(conn, "PqConnection")) {
          return(TRUE)
      } else {
        return(stop("'conn' must be connection object: <PostgreSQLConnection> from `RPostgreSQL`, or <PqConnection> from `RPostgres`"))
      }
}

## dbGetDefs
##' Get definitions for data frame mode reading
##' 
##' @param conn A PostgreSQL connection
##' @param name Table/view name string, length 1-2.
##' 
##' @keywords internal

dbGetDefs <- function(conn, name) {
    name <- dbTableNameFix(conn, name, as.identifier = FALSE)
    if (dbExistsTable(conn, c(name[1], ".R_df_defs"), table.only = TRUE)) {
      sql_query <- paste0("SELECT unnest(df_def[1:1]) as nms,
                              unnest(df_def[2:2]) as defs,
                              unnest(df_def[3:3]) as atts
                              FROM ",
              dbQuoteIdentifier(conn, name[1]), ".\".R_df_defs\" WHERE table_nm = ",
              dbQuoteString(conn, name[2]), ";")
      defs <- dbGetQuery(conn, sql_query)
      return(defs)
    } else {
      return(data.frame())
    }
}


## pg* non-exported functions

## pgCheckGeom
##' Check if geometry or geography column exists in a table,
##' and return the column name for use in a query. 
##' 
##' @param conn A PostgreSQL connection
##' @param namechar A table name formatted for use in a query
##' @param geom a geometry or geography column name
##' 
##' @keywords internal

pgCheckGeom <- function(conn, name, geom) {
  
    namechar <- dbQuoteString(conn, 
                  paste(dbTableNameFix(conn,name, as.identifier = FALSE), collapse = "."))
    ## Check table exists geom
    tmp.query <- paste0("SELECT f_geometry_column AS geo FROM geometry_columns\nWHERE 
        (f_table_schema||'.'||f_table_name) = ", 
        namechar, ";")
    tab.list <- dbGetQuery(conn, tmp.query)$geo
    ## Check table exists geog
    tmp.query <- paste0("SELECT f_geography_column AS geo FROM geography_columns\nWHERE 
        (f_table_schema||'.'||f_table_name) = ", 
        namechar, ";")
    tab.list.geog <- dbGetQuery(conn, tmp.query)$geo
    tab.list <- c(tab.list, tab.list.geog)
    
    if (is.null(tab.list)) {
        stop(paste0("Table/view ", namechar, " is not listed in geometry_columns or geography_columns."))
    } else if (!geom %in% tab.list) {
        stop(paste0("Table/view ", namechar, " geometry/geography column not found. Available columns: ", 
            paste(tab.list, collapse = ", ")))
    }
    ## prepare geom column
    if (geom %in% tab.list.geog) {
          # geog version
          geomque <- paste0(DBI::dbQuoteIdentifier(conn, geom),"::GEOMETRY")
        } else { 
          geomque <- DBI::dbQuoteIdentifier(conn, geom)
        }
    return(geomque)
}

## pgGetSRID
##' Get SRID(s) from a geometry/geography column in a full table
##' 
##' @param conn A PostgreSQL connection
##' @param name A schema/table name
##' @param geom a geometry or geography column name
##' 
##' @keywords internal


pgGetSRID <- function(conn, name, geom) {
    
    ## Check and prepare the schema.name
    nameque <- paste(dbTableNameFix(conn,name), collapse = ".")
    ## prepare geom column
    geomque <- pgCheckGeom(conn, name, geom)
    
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT a.s as st_srid FROM
                        (SELECT ST_SRID(", geomque, ") as s FROM ",
                        nameque, " WHERE ", geomque, " IS NOT NULL) a;")
    srid <- dbGetQuery(conn, tmp.query)
    
    return(srid$st_srid)
}

## bs
##' Return indexes for an exact number of blocks for a raster
##' 
##' @param r a raster
##' @param blocks Number of desired blocks (columns, rows)
##' 
##' @importFrom raster nrow ncol
##' @keywords internal

bs <- function(r, blocks) {
  blocks <- as.integer(blocks)
  if (any(is.na(blocks)) || length(blocks) > 2) stop("blocks must be a 1- or 2-length integer vector.")
  if (any(blocks == 0)) stop("Invalid number of blocks (0).")
  if (length(blocks) == 1) blocks <- c(blocks, blocks)
  r <- r[[1]]
  
  cr <- list()
  tr <- list()
  
  # cr
  b <- blocks[1]
  n.r <- raster::ncol(r)
  if (b == 1) {
    cr$row <- 1
    cr$nrows <- n.r
    cr$n <- 1
  } else {
    if (b >= n.r) b <- n.r
    if (n.r%%b == 0) {
      by <- n.r/b
      cr$row <- seq(1, to = n.r, by = by)
      cr$nrows <- rep(by, b)
      cr$n <- length(cr$row)
    } else {
      by <- floor(n.r/b)
      cr$row <- c(1,seq(1+by+(n.r%%b), to = n.r, by = by))
      cr$nrows <- c(cr$row[2:length(cr$row)], n.r+1) - cr$row
      cr$n <- length(cr$row)
    }
  }
  
  # tr
  b <- blocks[2]
  n.r <- raster::nrow(r)
  if (b == 1) {
    tr$row <- 1
    tr$nrows <- n.r
    tr$n <- 1
  } else {
    if (b >= n.r) b <- n.r
    if (n.r%%b == 0) {
      by <- n.r/b
      tr$row <- seq(1, to = n.r, by = by)
      tr$nrows <- rep(by, b)
      tr$n <- length(tr$row)
    } else {
      by <- floor(n.r/b)
      tr$row <- c(1,seq(1+by+(n.r%%b), to = n.r, by = by))
      tr$nrows <- c(tr$row[2:length(tr$row)], n.r+1) - tr$row
      tr$n <- length(tr$row)
    }
  }
  return(list(cr = cr, tr = tr))
}