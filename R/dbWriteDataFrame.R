# dbWriteDataFrame

#' Write/read in data frame mode to/from database table.
#'
#' Write \code{data.frame} or similar (e.g. \code{tibble}) to database table,
#' with column definitions, row names, and a new integer primary key column.
#' Read back into R with \code{dbReadDataFrame}, which recreates original
#' data.
#'
#' Writing in data frame mode is only for new database tables (or for
#' overwriting an existing one). It will save all column names as they
#' appear in R, along with column data types and attributes. This is
#' done by adding metadata to a lookup table in the table's schema
#' named ".R_df_defs" (will be created if not present). It also adds
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
#' All spatial objects must be written with \code{\link[rpostgis]{pgWriteGeom}}.
#' For more flexible writing of \code{data.frame}s to the database
#' (including all writing into existing database tables), use
#' \code{\link[rpostgis]{pgWriteGeom}} with \code{df.mode = FALSE}.
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
#' @author Adrián Cidre González \email{adrian.cidre@@gmail.com}
#' @aliases dbWriteDF
#' @export
#' @return \code{TRUE} for successful write with
#'     \code{dbWriteDataFrame}, \code{data.frame} for
#'     \code{dbReadDataFrame}
#' @examples
#' \dontrun{
#' library(datasets)
#'
#' ## Write the mtcars data.frame to the database:
#' dbWriteDataFrame(conn, name = "mtcars_data", df = mtcars)
#'
#' ## Reads it back into a different object:
#' mtcars2 <- dbReadDataFrame(conn, name = "mtcars_data")
#'
#' ## Check equality:
#' all.equal(mtcars, mtcars2)
#' ## Should return TRUE.
#' }

dbWriteDataFrame <- function(conn, name, df, overwrite = FALSE,
                             only.defs = FALSE) {

  ## Get table name
  nameque <- dbTableNameFix(conn, name)
  name    <- dbTableNameFix(conn, name, as.identifier = FALSE)

  ## Extract data frame from spatial objects
  if ("sf" %in% class(df)) {
    df <- sf::st_drop_geometry(df)
  } else if ("SpatVector" %in% class(df)) {
    df <- as.data.frame(df)
  }

  ## Drop table if it exists, or inform to use overwrite = TRUE if not specified
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

  ## If we save only defs, do not create .R_rownames
  if (!only.defs) {
    d <- data.frame(df, .R_rownames = attr(df, "row.names"),
                    stringsAsFactors = FALSE)
  } else {
    d <- df
  }

  ## Create defs table if it doesnt exist
  if (!dbExistsTable(conn, c(name[1], ".R_df_defs"), table.only = TRUE)) {
    sql_query <- paste0("CREATE TABLE ", nameque[1], ".\".R_df_defs\" (table_nm character varying, df_def text[]);")
    dbExecute(conn, sql_query)

    ## Add prmary key and comment to new df definitions table
    suppressMessages({
      dbAddKey(conn, c(name[1], ".R_df_defs"), colname = "table_nm", type = "primary")
      dbComment(conn, c(name[1], ".R_df_defs"), comment = "Table holding R data frame column definitions
                (for import/export using rpostgis::db(Read/Write)DataFrame).")
    })
    message(paste0("New R data frame definitions table created (", nameque[1], ".\".R_df_defs\")."))
  }


  ## get data types
  types <- purrr::map_vec(d, \(x) class(x)[1])

  ## MODULAR HANDING OF DIFFERENT DATA TYPES -----------------------------

  ## handle attribute (time zones)
  attr2 <- purrr::map(d[1,], \(x) attr(x, "tzone")[1])
  badtz <- purrr::map_vec(attr2, \(x) any(is.null(x), !x %in% OlsonNames()))
  attr2[badtz] <- "NULL"
  attr2 <- unlist(attr2)

  ## convert non-matching tz time to db tz (runs but values unchanged in posixlt objects)
  pgtz <- dbGetQuery(conn, "SHOW timezone;")[1, 1]
  tzl <- names(attr2[attr2 != "NULL" & attr2 != pgtz])
  for (t in tzl) {
    eval(parse(text = paste0("attributes(d$", t, ")$tzone <- pgtz")))
  }

  ## handle attribute (FACTOR levels)
  fact <- purrr::map_vec(d, \(x)
                         paste0("/*/", paste(levels(x), collapse = "/*/"), "/*/"))

  fact <- gsub(",", "\\,", fact, fixed = TRUE)
  attr2[!fact == "/*//*/"] <- fact[!fact == "/*//*/"]
  ## end factor

  ## handle spatial p4s (found using .rpostgis.geom. in column name, from pgWriteGeom)
  sp.index <- grep(".rpostgis.geom.",names(d))
  if (length(sp.index) > 0) {
    types[sp.index] <- "Spatial"
    attr2[sp.index] <- paste0('"',d[,sp.index][1],'"') # double quote for array
    names(d)[sp.index] <- gsub(".rpostgis.geom.","",names(d)[sp.index])
  }
  ## end spatial

  ## END MODULAR HANDING ---------------------------------------------------

  # Create array of column names, data type, and attributes
  defs <- paste0("{{", paste(names(d), collapse = ","), "},{",
                 paste(as.character(types), collapse = ","), "},{", paste(as.character(attr2),
                                                                          collapse = ","), "}}")

  ## Data frame with table name and definitions
  defs2 <- data.frame(table_nm = name[2], df_def = defs)

  ## Insert definitions and data frame to PostgreSQL database

  ## Insert table definitions in .R_df_defs
  suppressMessages({
    pgWriteGeom(conn, c(name[1], ".R_df_defs"), defs2,
                upsert.using = "table_nm", row.names = FALSE)
  })

  ## Send data to PostgreSQL database (if only defs, do not send)
  if (!only.defs) {
    suppressMessages({
      ## Create ".db_pkid" -> unique ID as primary key
      d <- data.frame(.db_pkid = 1:length(d[, 1]), d)
      # dbWriteTable(conn, name, d, row.names = FALSE) # doesn't work w/ RPostgres
      pgWriteGeom(conn, name, data.obj = d)
      dbAddKey(conn, name, colname = ".db_pkid", type = "primary")
    })
    message("Data frame written to table ",
            paste(nameque, collapse = "."), ".")
  }


  if (only.defs)
    return(d) else return(TRUE)
}


# dbReadDataFrame

#' @rdname dbWriteDataFrame
#' @aliases dbReadDF
#' @export

dbReadDataFrame <- function(conn, name, df = NULL) {

  ## Get table name
  nameque <- dbTableNameFix(conn, name)
  name    <- dbTableNameFix(conn, name, as.identifier = FALSE)

  ## Check if table exists (throw error if not)
  if (!dbExistsTable(conn, name)) {
    stop("Table ", paste(name, collapse = "."), " not found.")
  }

  ## Check definitions
  defs <- dbGetDefs(conn, name)

  ## Check if table definitions exists. If not, import without definitions.
  ## Check also length of defs. If it's equal to 0, there are no defs for the table
  if (!dbExistsTable(conn, c(name[1], ".R_df_defs"), table.only = TRUE)
      | length(defs) == 0) {

    ## Message depending on condition
    if (!dbExistsTable(conn, c(name[1], ".R_df_defs"), table.only = TRUE)) {
      message("R data frame definitions table not found. Using standard import...")
    } else {
      message("R data frame definitions not found. Using standard import...")
    }

    ## Read data depending on is.null(df) ----------------------------

    ## Read data when df is NULL and without definitions
    if (is.null(df)) {
      nmq <- paste(dbTableNameFix(conn, name, T), collapse = ".")
      df  <- dbGetQuery(conn, paste0("SELECT * FROM ", nmq, ";"))
      if (".R_rownames" %in% colnames(df)) {
        # still read rownames if exist
        try({
          row.names(df) <- df$.R_rownames
          df$.R_rownames <- NULL
        })
      }
      return(df)
    } else {
      ## Read data when df is not NULL and without definitions
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
    ## When there are definitions for the table -----------------------

    ## Get data depending on df is NULL
    if (is.null(df)) {
      d <- NULL
      sql_query <- paste0("SELECT * FROM ",
                          paste(nameque, collapse = "."), " ORDER BY \".db_pkid\";")
      try(d <- dbGetQuery(conn, sql_query), silent = TRUE)

      if (is.null(d)) {
        sql_query <- paste0("SELECT * FROM ",
                            paste(nameque, collapse = "."), ";")
        d <- dbGetQuery(conn, sql_query)
      }

    } else {
      d <- df
    }

    ## Assign data types to the retrieved data
    for (i in names(d)) {
      ## Get attributes for column i
      att <- defs[defs$nms == i, ]

      ## Go next if attributes doesn't exist
      if (length(att[, 1]) == 0) {
        next
      }

      ## If the column has attributes, restore them
      if (!is.na(att$atts)) {
        ## Begin modular handling of different data type attributes
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
        ## Else for other data types (character, integer, ...)
        d[, i] <- do.call(paste0("as.", att$defs), list(d[,
                                                          i]))
      }
    }

    ## Get rownames back, and eliminate columns .R_rownames and .db_pkid
    row.names(d)  <- d$.R_rownames
    d$.R_rownames <- NULL
    d$.db_pkid    <- NULL

    return(d)

  }
}


