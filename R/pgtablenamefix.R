# pgtablenamefix
#' @title Format input for database schema/table names
#' @description Internal rpostgis function to return common (length = 2) schema and table
#' name vector from various table and schema + table name inputs.
#' @param t.nm Table name string, length 1-2.
#' @return character vector of length 2. Each character element is in (escaped) double-quotes.
#' @export
#' @examples 
#' name<-c("schema","table")
#' pgtablenamefix(name)
#' 
#' name<-"schema.table"
#' pgtablenamefix(name)
#' 
#' #default schema (public) is added to tables
#' name<-"table"
#' pgtablenamefix(name)
#' 
#' #schema or table names with "." need to be given in two length vectors:
#' name<-c("schema","ta.ble")
#' pgtablenamefix(name)

pgtablenamefix<-function(t.nm){
  
  #cases
  if (length(t.nm) == 1 & length(strsplit(t.nm,".",fixed=T)[[1]]) == 2) {
      t.nm<-strsplit(t.nm,".",fixed=T)[[1]] 
    } else if (length(t.nm) == 1 & length(strsplit(t.nm,".",fixed=T)[[1]]) == 1) {
      t.nm<-c("public",t.nm)
    } else if (length(t.nm) > 2 | length(strsplit(t.nm,".",fixed=T)[[1]]) > 2) {
      stop("Invalid PostgreSQL table name. Schema/table names with non-standard characters in a two-length vector, as ('schem.a','tabl.e').")
    }
  
  #remove existing begin/end double quotes
  t.nm<-gsub('^"|"$', '', t.nm)
  
  #add double quotes
  t.nm<-c(paste0('"',t.nm[1],'"'),paste0('"',t.nm[2],'"'))
  return(t.nm)
}