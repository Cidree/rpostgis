## dbAsDate

##' Converts to timestamp.
##'
##' Convert a date field to a timestamp with or without time zone.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param date A character string specifying the date field.
##' @param tz A character string specifying the time zone, in
##'     \code{"EST"}, \code{"America/New_York"}, \code{"EST5EDT"},
##'     \code{"-5"}.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return If \code{exec = TRUE}, returns \code{TRUE} if the
##'     conversion was successful.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/datatype-datetime.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## Example uses a dummy connection from DBI package
##' conn <- DBI::ANSI()
##' dbAsDate(conn, name = c("schema", "table"), date = "date", tz = "GMT",
##'     exec = FALSE)

dbAsDate <- function(conn, name, date = "date", tz = NULL, display = TRUE,
    exec = TRUE) {
    ## Check and prepare the schema.name and date column
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    date <- DBI::dbQuoteIdentifier(conn, date)
    ## With or without time zones?
    timestamp <- ifelse(is.null(tz), "timestamp", "timestamptz")
    ## What time zone?
    tz <- ifelse(is.null(tz), "", paste0(" AT TIME ZONE '", tz,
        "'"))
    ## SQL query
    ## --
    ## ALTER TABLE '<schema>'.'<table>'
    ##     ALTER COLUMN '<date>' TYPE timestamptz
    ##     USING
    ##         '<date>'::timestamp AT TIME ZONE '<tz>';
    ## --
    tmp.query <- paste0("ALTER TABLE ", nameque, "\n    ALTER COLUMN ",
        date, " TYPE ", timestamp, "\n    USING\n        ", date,
        "::timestamp", tz, ";")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query and return TRUE
    if (exec) {
        dbSendQuery(conn, tmp.query)
        return(TRUE)
    }
}
