## pgComment

##' Comment on a table, a view or a schema.
##'
##' @title Comment table/view/schema
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table, view
##'     or schema name.
##' @param comment A character string specifying the comment.
##' @param type The type of the object to comment, either \code{table}
##'     or \code{view}
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the comment was successfully applied.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-comment.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' pgComment(name = c("fla", "bli"), comment = "Comment on a view.",
##'     type = "view", exec = FALSE)
##' pgComment(name = "fla", comment = "Comment on a schema.", type = "schema",
##'     exec = FALSE)

pgComment <- function(conn, name, comment, type = c("table",
    "view", "schema"), display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2) {
        name <- paste(name, collapse = ".")
    } else stop("The name should be \"table\", \"schema\" or c(\"schema\", \"table\").")
    ## Check and prepare the type
    type <- toupper(match.arg(type))
    ## Build the query
    tmp.query <- paste0("COMMENT ON ", type, " ", name, " IS '",
        comment, "';")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query
    if (exec)
        dbSendQuery(conn, tmp.query)
    ## Return nothing
    return(TRUE)
}
