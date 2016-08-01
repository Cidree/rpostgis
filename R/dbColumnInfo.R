## dbColumnInfo

##' Get information about columns.
##'
##' Get information about columns in a PostgreSQL table.
##'
##' @param conn A connection object to a PostgreSQL database.
##' @param name A character string specifying a PostgreSQL schema (if
##'     necessary), and table or view name geometry (e.g., \code{name
##'     = c("schema", "table")}).
##' @param allinfo Logical, Get all information on table? Default is
##'     column names, types, nullable, and maximum length of character
##'     columns.
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @export
##' @return data frame
##' @examples
##' \dontrun{
##' dbColumnInfo(conn, c("schema", "table"))
##' }

dbColumnInfo <- function(conn, name, allinfo = FALSE) {
    name <- rpostgis::dbTableNameFix(name)
    name <- gsub("\"", "", name)
    if (allinfo) {
        cols <- "*"
    } else {
        cols <- "column_name,data_type,is_nullable,character_maximum_length"
    }
    df <- dbGetQuery(conn, paste0("SELECT ", cols, " FROM information_schema.columns\nWHERE table_schema = '",
        name[1], "' AND table_name = '", name[2], "';"))
    return(df)
}
