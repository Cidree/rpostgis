# dbWriteDataFrame

#' Write/read in data frame mode to/from database table.
#'
#' Write \code{data.frame} to database table, with column definitions,
#' row names, and a new integer primary key column. Read back into R
#' with \code{dbReadDataFrame}, which recreates original data frame.
#'
#' Writing in data frame mode is only for new database tables (or for
#' overwriting an existing one). It will save all column names as they
#' appear in R, along with column data types and attributes.  This is
#' done by adding metadata to a lookup table in the table's schema
#' named ".R_df_defs" (will be created if not present).  It also adds
#' two fields with fixed names to the database table: ".R_rownames"
#' (storing the row.names of the data frame), and ".db_pkid", which is
#' a new integer primary key. Existing columns in the data.frame
#' matching these names will be automatically changed.
#'
#' The \code{rpostgis} database table read functions
#' \code{dbReadDataFrame} and \code{pgGetGeom} will use the metadata
#' created in data frame mode to recreate a data.frame in R, if it is
#' available. Otherwise, it will be imported using default
#' \code{RPostgreSQL::dbGetQuery} methods.
#' 
#' All \code{Spatial*DataFrame}s must be written with \code{\link[rpostgis]{pgInsert}}.
#' For more flexible writing of \code{data.frame}s to the database
#' (including all writing into existing database tables), use
#' \code{\link[rpostgis]{pgInsert}} with \code{df.mode = FALSE}.
#'
#' @param conn A connection object to a PostgreSQL database
#' @param name Character, schema and table of the PostgreSQL table
#' @param df The data frame to write (for \code{dbReadDataFrame}, this
#'     allows to update an existing \code{data.frame} with definitions
#'     stored in the database)
#' @param overwrite Logical; if TRUE, a new table (\code{name}) will
#'     overwrite the existing table (\code{name}) in the database. Note:
#'     overwriting a view must be done manually (e.g., with \code{\link[rpostgis]{dbDrop}}).
#' @param only.defs Logical; if \code{TRUE}, only the table
#'     definitions will be written.
#' @author David Bucklin \email{david.bucklin@@gmail.com}
#' @aliases dbWriteDF
#' @export
#' @return \code{TRUE} for successful write with
#'     \code{dbWriteDataFrame}, \code{data.frame} for
#'     \code{dbReadDataFrame}
#' @examples
#' \dontrun{
#' library(sp)
#' data(meuse)
#'
#' ## Write the data.frame to the database:
#' dbWriteDataFrame(conn, name = "meuse_data", df = meuse)
#'
#' ## Reads it back into a different object:
#' me2 <- dbReadDataFrame(conn, name = "meuse_data")
#'
#' ## Check equality:
#' all.equal(meuse, me2)
#' ## Should return TRUE.
#' }

dbWriteDataFrame <- function(conn, name, df, overwrite = FALSE,
    only.defs = FALSE) {

    nameque <- dbTableNameFix(conn, name)
    name <- dbTableNameFix(conn, name, as.identifier = FALSE)

    if (!only.defs) {
        d <- data.frame(df, .R_rownames = attr(df, "row.names"),
            stringsAsFactors = FALSE)
    } else {
        d <- df
    }

    if (!only.defs) {
        if (dbExistsTable(conn, name, table.only = TRUE)) {
            if (!overwrite) {
                stop("Table ", paste(nameque, collapse = "."),
                  " already exists. Use overwrite = TRUE to replace it.")
            } else {
                dbDrop(conn, name, type = "table")
            }
        }
    }

    ## create defs table
    if (!dbExistsTable(conn, c(name[1], ".R_df_defs"), table.only = TRUE)) {
        sql_query <- paste0("CREATE TABLE ", nameque[1], ".\".R_df_defs\" (table_nm character varying, df_def text[]);")
        dbExecute(conn, sql_query)
        suppressMessages({
            dbAddKey(conn, c(name[1], ".R_df_defs"), colname = "table_nm",
                type = "primary")
            dbComment(conn, c(name[1], ".R_df_defs"), comment = "Table holding R data frame column definitions
                (for import/export using rpostgis::db(Read/Write)DataFrame).")
        })
        message("New R data frame definitions table created (",
            nameque[1], ".\".R_df_defs\").")
    }

    ## get data types
    types <- unlist(lapply(d, function(x) {
        class(x)[1]
    }))

    ## modular handling of different data type attributes (add new
    ## below)

    ## handle attribute (time zones)
    attr2 <- lapply(d[1, ], function(x) {
        attr(x, "tzone")[1]
    })
    badtz <- unlist(lapply(attr2, function(x) {
        any(is.null(x), !x %in% OlsonNames())
    }))
    attr2[badtz] <- "NULL"
    attr2 <- unlist(attr2)

    ## convert non-matching tz time to db tz (runs but values unchanged in posixlt objects)
    pgtz <- dbGetQuery(conn, "SHOW timezone;")[1, 1]
    tzl <- names(attr2[attr2 != "NULL" & attr2 != pgtz])
    for (t in tzl) {
        eval(parse(text = paste0("attributes(d$", t, ")$tzone <- pgtz")))
    }

    ## handle attribute (factor levels)
    fact <- unlist(lapply(d[1, ], function(x) {
        paste0("/*/", paste(attr(x, "levels"), collapse = "/*/"),
            "/*/")
    }))
    fact <- gsub(",", "\\,", fact, fixed = TRUE)
    attr2[!fact == "/*//*/"] <- fact[!fact == "/*//*/"]
    ## end factor
    
    ## handle spatial p4s (found using .rpostgis.geom. in column name, from pgInsert)
    sp.index <- grep(".rpostgis.geom.",names(d))
    if (length(sp.index) > 0) {
      types[sp.index] <- "Spatial"
      attr2[sp.index] <- paste0('"',d[,sp.index][1],'"') # double quote for array
      names(d)[sp.index] <- gsub(".rpostgis.geom.","",names(d)[sp.index])
    }
    ## end spatial

    ## end modular handling of different data type attributes

    ## make array of columns, types, and attributes
    defs <- paste0("{{", paste(names(d), collapse = ","), "},{",
        paste(as.character(types), collapse = ","), "},{", paste(as.character(attr2),
            collapse = ","), "}}")

    defs2 <- data.frame(table_nm = name[2], df_def = defs)

    suppressMessages({
        ## send column defs to .R_df_defs
        pgInsert(conn, c(name[1], ".R_df_defs"), defs2, upsert.using = "table_nm",
            row.names = FALSE)

        ## send data to main table (only.defs = TRUE is for when used
        ## in pgInsert)
        if (!only.defs) {
            d <- data.frame(.db_pkid = 1:length(d[, 1]), d)
            # dbWriteTable(conn, name, d, row.names = FALSE) # doesn't work w/ RPostgres
            pgInsert(conn, name, data.obj = d)
            dbAddKey(conn, name, colname = ".db_pkid", type = "primary")
            message("Data frame written to table ", paste(nameque,
                collapse = "."), ".")
        }
    })

    if (only.defs)
        return(d) else return(TRUE)
}


# dbReadDataFrame

#' @rdname dbWriteDataFrame
#' @aliases dbReadDF
#' @export

dbReadDataFrame <- function(conn, name, df = NULL) {

    nameque <- dbTableNameFix(conn, name)
    name <- dbTableNameFix(conn, name, as.identifier = FALSE)

    if (!dbExistsTable(conn, name)) {
        stop("Table ", paste(name, collapse = "."), " not found.")
    }

    if (!dbExistsTable(conn, c(name[1], ".R_df_defs"), table.only = TRUE)) {
        message("R data frame definitions table not found. Using standard import...")
        if (is.null(df)) {
            nmq <- paste(dbTableNameFix(conn, name, T), collapse = ".")
            df <- dbGetQuery(conn, paste0("SELECT * FROM ", nmq, ";"))
            if (".R_rownames" %in% colnames(df)) {
              # still read rownames if exist
               try({
                 row.names(df) <- df$.R_rownames
                 df$.R_rownames <- NULL
                })
            }
            return(df)
        } else {
            if (".R_rownames" %in% colnames(df)) {
                  # still read rownames if exist
                  try({
                  row.names(df) <- df$.R_rownames
                  df$.R_rownames <- NULL
                  })
                }
            return(df)
        }
    } else {
        defs <- dbGetDefs(conn, name)
        
        if (length(defs) == 0) {
            message("R data frame definitions not found. Using standard import...")
            if (is.null(df)) {
                nmq <- paste(dbTableNameFix(conn, name, T), collapse = ".")
                df <- dbGetQuery(conn, paste0("SELECT * FROM ", nmq, ";"))
                if (".R_rownames" %in% colnames(df)) {
                  # still read rownames if exist
                  try({
                  row.names(df) <- df$.R_rownames
                  df$.R_rownames <- NULL
                  })
                }
                return(df)
            } else {
                if (".R_rownames" %in% colnames(df)) {
                  # still read rownames if exist
                  try({
                  row.names(df) <- df$.R_rownames
                  df$.R_rownames <- NULL
                  })
                }              
                return(df)
            }
        }

        if (is.null(df)) {
            sql_query <- paste0("SELECT * FROM ", paste(nameque,
                collapse = "."), " ORDER BY \".db_pkid\";")
            d <- dbGetQuery(conn, sql_query)
        } else {
            d <- df
        }

        ## assign types
        for (i in names(d)) {
            att <- defs[defs$nms == i, ]
            if (length(att[, 1]) == 0) {
                next
            }
            if (!is.na(att$atts)) {
                ## begin modular handling of different data type attributes
                ## (add new below) handle factors
                if (att$defs %in% c("factor", "ordered")) {
                  levs <- unlist(strsplit(att$atts, "/*/", fixed = TRUE))
                  ordered <- ifelse(att$defs == "ordered", TRUE,
                    FALSE)
                  d[, i] <- factor(as.character(d[, i]), levels = levs[levs !=
                    ""], ordered = ordered)
                }
                ## handle POSIX time zones
                if (att$defs %in% c("POSIXct", "POSIXt")) {
                  d[, i] <- list(eval(parse(text = paste0("as.",
                    att$defs, "(as.character(d[,i]),
                                      tz='",
                    Sys.timezone(), "')"))))
                  ## assign R tz
                  eval(parse(text = paste0("attributes(d$", i,
                    ")$tzone <- att$atts")))
                }
                if (att$defs == "POSIXlt") {
                    d[, i] <- list(eval(parse(text = paste0("as.", 
                      att$defs, "(as.character(d[,i]),
                                          tz=att$atts)"))))
                }
                ## end modular handling of different data types
            } else {
                d[, i] <- do.call(paste0("as.", att$defs), list(d[,
                  i]))
            }
        }

        row.names(d) <- d$.R_rownames
        d$.R_rownames <- NULL
        d$.db_pkid <- NULL

        return(d)
    }
}
