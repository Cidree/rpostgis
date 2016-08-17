## dbTableInfo

##' Get information about table columns.
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
##' @return data frame
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @export
##' @examples
##' \dontrun{
##' dbTableInfo(conn, c("schema", "table"))
##' }

dbTableInfo <- function(conn, name, allinfo = FALSE) {
    ## only check if valid (error if not)
    name.fix <- dbTableNameFix(name)
    #add public if length == 1
    if (length(name) == 1) {name<-c("public",name)}
    if (allinfo) {
        cols <- "*"
    } else {
        cols <- "column_name,data_type,is_nullable,character_maximum_length"
    }
    df <- dbGetQuery(conn, paste0("SELECT ", cols, " FROM information_schema.columns\nWHERE table_schema = '",
        name[1], "' AND table_name = '", name[2], "';"))
    return(df)
}