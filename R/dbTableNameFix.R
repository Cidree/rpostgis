## dbTableNameFix

##' Format input for database schema/table names.
##'
##' Internal rpostgis function to return common (length = 2) schema
##' and table name vector from various table and schema + table name
##' inputs.
##'
##' @param t.nm Table name string, length 1-2.
##' @return character vector of length 2. Each character element is in
##'     (escaped) double-quotes.
##' @keywords internal
##' @importFrom DBI ANSI
##' @importFrom DBI dbQuoteIdentifier
##' @examples
##' \dontrun{
##' name<-c("schema","table")
##' dbTableNameFix(name)
##' 
##' #default schema (public) is added to single-length characters (only table is given)
##' name<-"table"
##' dbTableNameFix(name)
##' 
##' #schema or table names with double quotes should be given exactly as they are 
##' (make sure to wrap in single quotes in R):
##' name<-c('sch"ema','"table"')
##' dbTableNameFix(name)
##' }

dbTableNameFix <- function(t.nm) {
    ## Cases
      if (length(t.nm) == 1) {
        t.nm <- c("public", t.nm)
      }
      if (length(t.nm) > 2)
      {
        stop("Invalid PostgreSQL table/view name. Must be provided as one ('table') or two-length c('schema','table') character vector.")
      }
    t.nm<-DBI::dbQuoteIdentifier(DBI::ANSI(), DBI::dbQuoteIdentifier(DBI::ANSI(), t.nm))
    return(t.nm)
}
