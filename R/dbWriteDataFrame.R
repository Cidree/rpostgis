# dbWriteDataFrame

#' Write/read in data.frame mode to/from database tables
#' 
#' Write data.frame to database table, with column definitions, row.names,
#' and a new integer primary key column. Read back into R with
#' dbReadDataFrame, which recreates original data frame.
#' 
#' Writing in data.frame mode is only for new database tables (or for
#' overwriting an existing one). It will save all column names as they
#' appear in R, along with column data types and attributes. 
#' This is done by adding metadata to a lookup table in the table's
#' schema named ".R_df_defs" (will be created if not present). 
#' It also adds two fixed names to the database table: ".R_rownames" (storing
#' the row.names of the data frame), and ".db_pkid", which is a new
#' integer primary key. Existing columns in the data.frame matching these
#' names will be automatically changed. For more flexible writing of
#' data.frames to the database, users should 
#' use \code{\link[rpostgis]{pgInsert}} with \code{df.mode = FALSE}.
#' 
#' \code{dbReadDataFrame} and \code{pgGetGeom} will use this metadata to
#' recreate a data.frame in R, if it is available. Otherwise, 
#' it will be imported using default \code{RPostgreSQL} methods.
#' 
#' @param conn A connection object to a PostgreSQL database
#' @param name Character, schema and table of the PostgreSQL table
#' @param df The data frame to write (for dbReadDataFrame, this should
#' be left NULL)
#' @param overwrite Logical; if TRUE, a new table (\code{name}) will
#'    overwrite the existing table (\code{name}) in the database.
#' @param only_defs Logical; if TRUE, only the table definitions will be
#'    written.
#' @author David Bucklin \email{dbucklin@@ufl.edu}
#' @export
#' @return TRUE for dbWriteDataFrame, data.frame for dbReadDataFrame
#' @examples
#' \dontrun{
#' library(sp)
#' data(meuse)
#' 
#' dbWriteDataFrame(conn, name = "meuse_data", df = meuse)
#' 
#' me2 <- dbReadDataFrame(conn, name = "meuse_data")
#' 
#' all.equal(meuse, me2)
#' # Should return TRUE
#' }


dbWriteDataFrame<- function (conn, name, df, overwrite = FALSE, only_defs = FALSE) {
    
    nameque <- dbTableNameFix(conn,name)
    name <- dbTableNameFix(conn,name, as.identifier = FALSE)
    
    if (!only_defs) {
      d<-data.frame(df, .R_rownames=attr(df, "row.names"), stringsAsFactors = FALSE)
    } else {
      d<-df
    }
    
    if (!only_defs) {
      if (dbExistsTable(conn, name)) {
        if (!overwrite) {
          stop("Table ",paste(nameque,collapse = ".")," already exists. Use overwrite = TRUE to replace it.")
        } else {
          dbDrop(conn, name, type = "table")
        }
      }
    }
    
    # create defs table  
    if (!dbExistsTable(conn,  c(name[1],".R_df_defs"))) {
      sql_query<-paste0("CREATE TABLE ",nameque[1],
                        ".\".R_df_defs\" (table_nm character varying, df_def text[]);")
      dbExecute(conn, sql_query)
      suppressMessages({
      dbAddKey(conn, c(name[1],".R_df_defs"), colname = "table_nm", type = "primary")
      dbComment(conn, c(name[1],".R_df_defs"),
                comment = "Table holding R data frame column definitions 
                (for import/export using rpostgis::db(Read/Write)DataFrame).")
      })
      message("New R data frame definitions table created (",nameque[1],".\".R_df_defs\").")
    }
    
    # get data types
    types <- unlist(lapply(d, function(x) {
        class(x)[1]
    }))
    
    # handle attribute (time zones)
    attr2 <- lapply(d[1, ], function(x) {
        attr(x, "tzone")[1]
    })
    badtz <- unlist(lapply(attr2, function(x) {
        any(is.null(x), !x %in% OlsonNames())
    }))
    attr2[badtz] <- "NULL"
    attr2 <- unlist(attr2)
    
    # handle attribute (factor levels)
    fact <- unlist(lapply(d[1, ], function(x) {
        paste0("/*/", paste(attr(x, "levels"), collapse = "/*/"), 
            "/*/")
    }))
    attr2[!fact == "/*//*/"] <- fact[!fact == "/*//*/"]
    
    # make array of columns, types, and time zones
    defs <- paste0("{{", paste(names(d), collapse = ","), 
        "},{", paste(as.character(types), collapse = ","), "},{", 
        paste(as.character(attr2), collapse = ","), "}}")
    
    defs2 <- data.frame(table_nm = name[2], df_def = defs)
    
    suppressMessages({
    # send column defs to .R_df_defs
    pgInsert(conn, c(name[1],".R_df_defs"), defs2,
             upsert.using = "table_nm", row.names = FALSE)
    
    # send data to main table (only_defs = TRUE is for when used in pgInsert)
    if (!only_defs) {
      d<-data.frame(.db_pkid = 1:length(d[,1]),d) 
      dbWriteTable(conn, name, d, row.names = FALSE)
      dbAddKey(conn, name, colname = ".db_pkid", type = "primary")
      message("Data frame written to table ",paste(nameque,collapse = "."),".")
    }
    })
    
    return(TRUE)
}

# dbReadDataFrame

#' @rdname dbWriteDataFrame
#' @export

dbReadDataFrame<- function(conn, name, df = NULL) {
  
    nameque <- dbTableNameFix(conn,name)
    name <- dbTableNameFix(conn,name, as.identifier = FALSE)
    
    if (!dbExistsTable(conn, name)) {
      stop("Table ",paste(name,collapse=".")," not found.")
    }
    
    if (!dbExistsTable(conn,c(name[1],".R_df_defs"))) {
      message("R data frame definitions table not found. Using standard import...")
      if (is.null(df)) {
        return(dbReadTable(conn, name))
      } else {
        return(df)
      }
    } else {
       sql_query <- paste0("SELECT unnest(df_def[1:1]) as nms, 
                            unnest(df_def[2:2]) as defs,
                            unnest(df_def[3:3]) as atts 
                            FROM ",nameque[1],".\".R_df_defs\" WHERE table_nm = ",
                            dbQuoteString(conn, name[2]),";")
       defs<-dbGetQuery(conn,sql_query)
       
       if (length(defs) == 0) {
         message("R data frame definitions not found. Using standard import...")
         if (is.null(df)) {
            return(dbReadTable(conn, name))
          } else {
            return(df)
          }
       }
       
      if (is.null(df)) {
        sql_query<- paste0("SELECT * FROM ",paste(nameque, collapse = "."),
                           " ORDER BY \".db_pkid\";")
        d <- dbGetQuery(conn, sql_query)
      } else {
        d <- df
      }
      
      # assign types
      for (i in names(d)) {
          att <- defs[defs$nms == i, ]
          if (length(att[, 1]) == 0) {
              next
          }
          if (!is.na(att$atts)) {
              # handle factors
              if (att$defs %in% c("factor", "ordered")) {
                levs <- unlist(strsplit(att$atts, "/*/", fixed = TRUE))
                ordered <- ifelse(att$defs == "ordered", TRUE, 
                  FALSE)
                d[, i] <- factor(as.character(d[, 
                  i]), levels = levs[levs != ""], ordered = ordered)
              }
              if (att$defs %in% c("POSIXct", "POSIXlt", "POSIXt")) {
                d[, i] <- list(eval(parse(text = paste0("as.", 
                  att$defs, "(as.character(d[,i]),
                                      tz='", 
                  att$atts, "')"))))
              }
          } else {
              d[, i] <- do.call(paste0("as.", att$defs), 
                list(d[, i]))
          }
      }
      
      row.names(d)<-d$.R_rownames
      d$.R_rownames<-NULL
      d$.db_pkid<-NULL
      
      return(d)
    }
}