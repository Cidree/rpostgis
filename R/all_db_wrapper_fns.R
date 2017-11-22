## dbAddKey

##' Add key.
##'
##' Add a primary or foreign key to a table column.
##'
##' @param conn A connection object.
##' @param name A character string, or a character vector, specifying
##'     a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##'     column to which the key will be assign; alternatively, a
##'     character vector specifying the name of the columns for keys
##'     spanning more than one column.
##' @param type The type of the key, either \code{"primary"} or
##'     \code{"foreign"}
##' @param reference A character string specifying a foreign table
##'     name to which the foreign key will be associated (ignored if
##'     \code{type == "primary"}).
##' @param colref A character string specifying the name of the
##'     primary key in the foreign table to which the foreign key will
##'     be associated; alternatively, a character vector specifying
##'     the name of the columns for keys spanning more than one column
##'     (ignored if \code{type == "primary"}).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the key was successfully added.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-altertable.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## Examples use a dummy connection from DBI package
##' conn <- DBI::ANSI()
##'
##' ## Primary key
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = "id1", exec = FALSE)
##'
##' ## Primary key using multiple columns
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = c("id1", "id2",
##'     "id3"), exec = FALSE)
##'
##' ## Foreign key
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = "id", type = "foreign",
##'     reference = c("sch2", "tbl2"), colref = "id", exec = FALSE)
##'
##' ## Foreign key using multiple columns
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = c("id1", "id2"),
##'     type = "foreign", reference = c("sch2", "tbl2"), colref = c("id3",
##'         "id4"), exec = FALSE)
dbAddKey <- function(conn, name, colname, type = c("primary",
    "foreign"), reference, colref, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name and column name
    name <- dbTableNameFix(conn, name)
    nameque <- paste(name, collapse = ".")
    colname <- paste(DBI::dbQuoteIdentifier(conn, colname), collapse = ", ")
    ## Check 'type' and set it to upper case
    type <- toupper(match.arg(type))
    ## If primary key, both 'reference' and 'colref' are ignored
    ## (empty strings)
    if (type == "PRIMARY") {
        colref <- ""
        references <- ""
    ## If foreign key, check identifiers for 'reference' and 'colref'
    } else if (type == "FOREIGN") {
        colref <- paste(DBI::dbQuoteIdentifier(conn, colref),
            collapse = ", ")
        reference <- dbTableNameFix(conn, reference)
        references <- paste0(" REFERENCES ", paste(reference,
            collapse = "."), " (", colref, ")")
    }
    ## Build the query
    tmp.query <- paste0("ALTER TABLE ", nameque, " ADD ", type,
        " KEY (", colname, ")", references, ";")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
    }
    ## Execute the query and return TRUE
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
        return(TRUE)
    }
}


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
    name <- dbTableNameFix(conn,name)
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
        #message("--")
    }
    ## Execute the query and return TRUE
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
        return(TRUE)
    }
}

## dbColumn

##' Add or remove a column.
##'
##' Add or remove a column to/from a table.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##'     column
##' @param action A character string specifying if the column is to be
##'     added (\code{"add"}, default) or removed (\code{"drop"}).
##' @param coltype A character string indicating the type of the
##'     column, if \code{action = "add"}.
##' @param cascade Logical. Whether to drop foreign key constraints of
##'     other tables, if \code{action = "drop"}.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the column was successfully added or
##'     removed.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-altertable.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' ## Add an integer column
##' dbColumn(conn, name = c("schema", "table"), colname = "field", exec = FALSE)
##' ## Drop a column (with CASCADE)
##' dbColumn(conn, name = c("schema", "table"), colname = "field", action = "drop",
##'     cascade = TRUE, exec = FALSE)

dbColumn <- function(conn, name, colname, action = c("add", "drop"),
    coltype = "integer", cascade = FALSE, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name
    name <- dbTableNameFix(conn,name)
    nameque <- paste(name, collapse = ".")
    colname<-DBI::dbQuoteIdentifier(conn,colname)
    ## Check and translate to upper case the action
    action <- toupper(match.arg(action))
    ## 'args' for the coltype or cascade
    args <- ifelse(action == "ADD", coltype, ifelse(cascade,
        "CASCADE", ""))
    ## Build the query
    tmp.query <- paste0("ALTER TABLE ", nameque, " ", action, " COLUMN ",
        colname, " ", args, ";")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        #message("--")
    }
    ## Execute the query
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
    }
    ## Return TRUE
    if(exec) return(TRUE)
}

## dbComment

##' Comment table/view/schema.
##'
##' Comment on a table, a view or a schema.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table, view
##'     or schema name.
##' @param comment A character string specifying the comment.
##' @param type The type of the object to comment, either \code{"table"}, \code{"view"},
##'     or \code{"schema"}
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
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' dbComment(conn, name = c("schema", "table"), comment = "Comment on a view.",
##'     type = "view", exec = FALSE)
##' dbComment(conn, name = "test_schema", comment = "Comment on a schema.", type = "schema",
##'     exec = FALSE)

dbComment <- function(conn, name, comment, type = c("table",
    "view", "schema"), display = TRUE, exec = TRUE) {
    ## Check and prepare the type
    type <- toupper(match.arg(type))
    ## Check and prepare name
    if (type %in% c("TABLE","VIEW")) {
      name <- dbTableNameFix(conn,name)
      nameque <- paste(name, collapse = ".")
    } else {
      if (length(name) > 1) {stop("Schemas should be a character of length = 1.")}
      nameque<-DBI::dbQuoteIdentifier(conn,name)
    }
    ## Escape single "'"
    comment<-gsub("'","''",comment)
    ## Build the query
    tmp.query <- paste0("COMMENT ON ", type, " ", nameque, " IS '",
        comment, "';")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        #message("--")
    }
    ## Execute the query
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
    }
    ## Return true
    if(exec) return(TRUE)
}

## dbDrop

##' Drop table/view/schema.
##'
##' Drop a table, a view or a schema.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table, schema, or view name.
##' @param type The type of the object to drop, either \code{"table"}, \code{"schema"},
##'     \code{"view"}, or \code{"materialized view"}.
##' @param ifexists Do not throw an error if the object does not
##'     exist. A notice is issued in this case.
##' @param cascade Automatically drop objects that depend on the object
##'     (such as views).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the table/schema/view was successfully
##'     dropped.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-droptable.html},
##'     \url{http://www.postgresql.org/docs/current/static/sql-dropview.html},
##'     \url{http://www.postgresql.org/docs/current/static/sql-dropschema.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' dbDrop(conn, name = c("schema", "view_name"), type = "view", exec = FALSE)
##' dbDrop(conn, name = "test_schema", type = "schema", cascade = "TRUE", exec = FALSE)

dbDrop <- function(conn, name, type = c("table", "schema", "view", "materialized view"),
    ifexists = FALSE, cascade = FALSE, display = TRUE, exec = TRUE) {
    type <- toupper(match.arg(type))
    ## Check and prepare name
    if (type %in% c("TABLE","VIEW","MATERIALIZED VIEW")) {
      name <- dbTableNameFix(conn,name)
      nameque <- paste(name, collapse = ".")
    } else {
      if (length(name) > 1) {stop("Schemas should be a character of length = 1.")}
      nameque<-DBI::dbQuoteIdentifier(conn,name)
    }
    ## Argument IF EXISTS
    ifexists <- ifelse(ifexists, " IF EXISTS ", " ")
    ## Argument CASCADE
    cascade <- ifelse(cascade, " CASCADE", "")
    ## Build the query
    tmp.query <- paste0("DROP ", type, ifexists, nameque, cascade,
        ";")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        #message("--")
    }
    ## Execute the query
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
    }
    ## Return true
    if(exec) return(TRUE)
}


## dbIndex

##' Create an index.
##'
##' Defines a new index on a PostgreSQL table.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string, or a character vector
##'     specifying the name of the column to which the key will be
##'     associated; alternatively, a character vector specifying the
##'     name of the columns to build the index.
##' @param idxname A character string specifying the name of the index
##'     to be created. By default, this uses the name of the table
##'     (without the schema) and the name of the columns as follows:
##'     \code{<table_name>_<column_names>_idx}.
##' @param unique Logical. Causes the system to check for duplicate
##'     values in the table when the index is created (if data already
##'     exist) and each time data is added. Attempts to insert or
##'     update data which would result in duplicate entries will
##'     generate an error.
##' @param method The name of the method to be used for the
##'     index. Choices are \code{"btree"}, \code{"hash"},
##'     \code{"rtree"}, and \code{"gist"}. The default method is
##'     \code{"btree"}, although \code{"gist"} should be the index of
##'     choice for PostGIS spatial types (geometry, geography,
##'     raster).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the index was successfully created.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-createindex.html};
##'     the PostGIS documentation for GiST indexes:
##'     \url{http://postgis.net/docs/using_postgis_dbmanagement.html#id541286}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## Examples use a dummy connection from DBI package
##' conn <- DBI::ANSI()
##'
##' ## GIST index
##' dbIndex(conn, name = c("sch", "tbl"), colname = "geom", method = "gist",
##'     exec = FALSE)
##'
##' ## Regular BTREE index on multiple columns
##' dbIndex(conn, name = c("sch", "tbl"), colname = c("col1", "col2",
##'     "col3"), exec = FALSE)
dbIndex <- function(conn, name, colname, idxname, unique = FALSE,
    method = c("btree", "hash", "rtree", "gist"), display = TRUE,
    exec = TRUE) {
    ## Check and prepare the name of the index
    if (missing(idxname)) {
        idxname <- DBI::dbQuoteIdentifier(conn, paste(name[length(name)],
            paste(colname, collapse = "_"), "idx", sep = "_"))
    } else {
        idxname <- DBI::dbQuoteIdentifier(conn, idxname)
    }
    ## Check and prepare the schema.name and column name
    name <- dbTableNameFix(conn, name)
    nameque <- paste(name, collapse = ".")
    colname <- paste(DBI::dbQuoteIdentifier(conn, colname), collapse = ", ")
    ## Argument UNIQUE
    unique <- ifelse(unique, "UNIQUE ", "")
    ## Check and prepare the method for the index
    method <- match.arg(method)
    usemeth <- ifelse(method == "btree", "", paste(" USING",
        toupper(method)))
    ## Build the query
    tmp.query <- paste0("CREATE ", unique, "INDEX ", idxname,
        " ON ", nameque, usemeth, " (", colname, ");")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query and return TRUE
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
        return(TRUE)
    }
}


## dbSchema

##' Check and create schema.
##'
##' Checks the existence, and if necessary, creates a schema.
##'
##' @param conn A connection object (required, even if \code{exec =
##'     FALSE}).
##' @param name A character string specifying a PostgreSQL schema
##'     name.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}). Note: if \code{exec = FALSE}, the function still
##'     checks the existence of the schema, but does not create it if
##'     it does not exists.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-createschema.html}
##' @return \code{TRUE} if the schema exists (whether it was already
##'     available or was just created).
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' \dontrun{
##'     dbSchema(name = "schema", exec = FALSE)
##' }

dbSchema <- function(conn, name, display = TRUE, exec = TRUE) {
    dbConnCheck(conn)
    ## Check the name of the schema
    if (length(name) != 1)
        stop("The schema name should be of length 1.")
    ## make schema name
    namechar<-DBI::dbQuoteString(conn,name)
    nameque<-DBI::dbQuoteIdentifier(conn,name)
    ## Check existence of the schema
    tmp.query <- paste0("SELECT EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = ",
        namechar, ");")
    schema <- dbGetQuery(conn, tmp.query)[1, 1]
    ## If exists, return TRUE, otherwise create the schema
    if (isTRUE(schema))
        return(TRUE) else {
        ## Build the query
        tmp.query <- paste0("CREATE SCHEMA ", nameque[1], ";")
        ## Display the query
        if (display) {
            message(paste0("Query ", ifelse(exec, "", "not "),
                "executed:"))
            message(tmp.query)
            #message("--")
        }
        ## Execute the query
        if (exec)
            dbSendQuery(conn, tmp.query)
        ## Return true
        if(exec) return(TRUE)
    }
}

## dbTableInfo

##' Get information about table columns.
##'
##' Get information about columns in a PostgreSQL table.
##'
##' @param conn A connection object to a PostgreSQL database.
##' @param name A character string specifying a PostgreSQL schema (if
##'     necessary), and table or view name (e.g., \code{name
##'     = c("schema", "table")}).
##' @param allinfo Logical, Get all information on table? Default is
##'     column names, types, nullable, and maximum length of character
##'     columns.
##' @return data frame
##' @author David Bucklin \email{david.bucklin@@gmail.com}
##' @export
##' @examples
##' \dontrun{
##' dbTableInfo(conn, c("schema", "table"))
##' }

dbTableInfo <- function(conn, name, allinfo = FALSE) {
    dbConnCheck(conn)
    ## only check if valid (error if not)
    name <- dbTableNameFix(conn,name,as.identifier=FALSE)
    if (allinfo) {
        cols <- "*"
    } else {
        cols <- "column_name,data_type,is_nullable,character_maximum_length"
    }
    df <- dbGetQuery(conn, paste0("SELECT ", cols, " FROM information_schema.columns\nWHERE table_schema = ",
        DBI::dbQuoteString(conn,name[1]), " AND table_name = ", DBI::dbQuoteString(conn,name[2]), ";"))
    return(df)
}

## dbVacuum

##' Vacuum.
##'
##' Performs a VACUUM (garbage-collect and optionally analyze) on a
##' table.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param full Logical. Whether to perform a "full" vacuum, which can
##'     reclaim more space, but takes much longer and exclusively
##'     locks the table.
##' @param verbose Logical. Whether to print a detailed vacuum
##'     activity report for each table.
##' @param analyze Logical. Whether to update statistics used by the
##'     planner to determine the most efficient way to execute a query
##'     (default to \code{TRUE}).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-vacuum.html}
##' @return TRUE if query is successfully executed.
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' dbVacuum(conn, name = c("schema", "table"), full = TRUE, exec = FALSE)

dbVacuum <- function(conn, name, full = FALSE, verbose = FALSE,
    analyze = TRUE, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name
    name <- dbTableNameFix(conn,name)
    nameque <- paste(name, collapse = ".")
    ## Full VACUUM?
    full <- ifelse(full, "FULL ", "")
    ## Argument VERBOSE
    verbose <- ifelse(verbose, "VERBOSE ", "")
    ## Argument ANALYZE
    analyze <- ifelse(analyze, "ANALYZE ", "")
    ## Build the query
    tmp.query <- paste0("VACUUM ", full, verbose, analyze, nameque,
        ";")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        #message("--")
    }
    ## Execute the query
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
    }
    ## Return true
    if(exec) return(TRUE)
}
