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
    t.nm<-DBI::dbQuoteIdentifier(conn, DBI::dbQuoteIdentifier(conn, t.nm))
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