## pgAsDate
##
##' Convert a date field to a timestamp with or without time zone.
##'
##' @title Converts to timestamp
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param date A character string specifying the date field.
##' @param tz A character string specifying the time zone, in
##' \code{"EST"}, \code{"America/New_York"}, \code{"EST5EDT"},
##' \code{"-5"}.
##' @param display Logical. Whether to display the query (defaults to
##' \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##' \code{TRUE}).
##' @seealso The PostgreSQL documentation:
##' \url{http://www.postgresql.org/docs/current/static/datatype-datetime.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' pgAsDate(name = c("fla", "bli"), date = "date", tz = "GMT", exec = FALSE)
pgAsDate <- function(conn, name, date = "date", tz = NULL, display = TRUE,
    exec = TRUE)
{
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2)
        name <- paste(name, collapse = ".")
    else stop("The table name should be \"table\" or c(\"schema\", \"table\").")
    ## With or without time zones?
    timestamp <- ifelse(is.null(tz), "timestamp", "timestamptz")
    ## What time zone?
    tz <- ifelse(is.null(tz), "", paste0(" AT TIME ZONE '", tz,
        "'"))
    ## Build the query
    str <- paste0("ALTER TABLE ", name, " ALTER COLUMN ", date,
        " TYPE ", timestamp, " USING ", date, "::timestamp",
        tz, ";")
    ## Display the query
    if (display)
        cat(paste0("Query ", ifelse(exec, "", "not "), "executed:\n",
            str, "\n--\n"))
    ## Execute the query
    if (exec)
        dbSendQuery(conn, str)
    ## Return nothing
    return(invisible())
}
