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
##' @param as.identifier Boolean where to return (schema,table) name as database
##' sanitized identifers (TRUE) or as regular character (FALSE)
##' @return character vector of length 2. Each character element is in
##'     (escaped) double-quotes.
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
      if (length(t.nm) == 1 && !is.null(conn)) {
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

dbBuildTableQuery <- function (conn = NULL, name, obj, field.types = NULL, row.names = FALSE) 
{
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
##' Check if a PostgreSQL table exists
##' 
##' @param conn A PostgreSQL connection
##' @param name Table name string, length 1-2.
##' 
##' @note Adapted from RPostgreSQL::dbExistsTable
##' @keywords internal

dbExistsTable <- function (conn, name) {
    full.name<-dbTableNameFix(conn,name, as.identifier = FALSE)
    chk<-dbGetQuery(conn, paste0("SELECT 1 FROM information_schema.tables 
               WHERE table_schema = ",dbQuoteString(conn,full.name[1]),
               " AND table_name = ",dbQuoteString(conn,full.name[2]),";"))[1,1]
    if (is.null(chk) || is.na(chk)) {exists.t<-FALSE} else {exists.t<-TRUE}
    return(exists.t)
}

## dbConnCheck
##' Check if a supported PostgreSQL connection
##' 
##' @param conn A PostgreSQL connection
##' 
##' @keywords internal

dbConnCheck <- function(conn) {
  if (inherits(conn, c("PostgreSQLConnection"))) {
          return(TRUE)
      } else {
        return(stop("'conn' must be a <PostgreSQLConnection> object."))
      }
}