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

dbTableNameFix <- function(t.nm) {
    ## Cases
    if (length(t.nm) == 1 & length(strsplit(t.nm, ".", fixed = T)[[1]]) ==
        2) {
        t.nm <- strsplit(t.nm, ".", fixed = T)[[1]]
    } else if (length(t.nm) == 1 & length(strsplit(t.nm, ".", fixed = T)[[1]]) ==
        1) {
        t.nm <- c("public", t.nm)
    } else if (length(t.nm) > 2 | length(strsplit(t.nm, ".", fixed = T)[[1]]) >
        2) {
        stop("Invalid PostgreSQL table name. Schema/table names with non-standard characters in a two-length vector, as ('schem.a','tabl.e').")
    }
    ## Remove existing begin/end double quotes
    t.nm <- gsub("^\"|\"$", "", t.nm)
    ## Add double quotes
    t.nm <- c(paste0("\"", t.nm[1], "\""), paste0("\"", t.nm[2],
        "\""))
    return(t.nm)
}
