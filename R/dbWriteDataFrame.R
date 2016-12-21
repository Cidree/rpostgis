#dbWriteDataFrame
#' @keywords internal

dbWriteDataFrame<- function (conn, name, df, overwrite = FALSE, only_defs = FALSE) {
    
    nameque <- rpostgis:::dbTableNameFix(conn,name)
    name <- rpostgis:::dbTableNameFix(conn,name, as.identifier = FALSE)
    
    if (!only_defs) {
      d<-data.frame(.R_rownames=attr(df, "row.names"), df)
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
    pgInsert(conn, c(name[1],".R_df_defs"), defs2, upsert.using = "table_nm", row.names = FALSE)
    
    # send data to main table (only_defs = TRUE is for when used in pgInsert)
    if (!only_defs) {
      dbWriteTable(conn, name, d, row.names = FALSE)
      message("Data frame written to table ",paste(nameque,collapse = "."),".")
    }
    })
    
    return(TRUE)
}

####
# dbReadDataFrame
#' @keywords internal


dbReadDataFrame<- function(conn, name) {
  
    nameque <- rpostgis:::dbTableNameFix(conn,name)
    name <- rpostgis:::dbTableNameFix(conn,name, as.identifier = FALSE)
    
    if (!dbExistsTable(conn, name)) {
      stop("Table ",paste(name,collapse=".")," not found.")
    }
    
    if (!dbExistsTable(conn,c(name[1],".R_df_defs"))) {
      message("R data frame definitions table not found. Using standard import...")
      return(dbReadTable(conn, name))
    } else {
       sql_query <- paste0("SELECT unnest(df_def[1:1]) as nms, 
                            unnest(df_def[2:2]) as defs,
                            unnest(df_def[3:3]) as atts 
                            FROM ",nameque[1],".\".R_df_defs\" WHERE table_nm = ",
                            dbQuoteString(conn, name[2]),";")
       defs<-dbGetQuery(conn,sql_query)
       
       if (length(defs) == 0) {
         message("R data frame definitions not found. Using standard import...")
         return(dbReadTable(conn, name))
       }
       
      d <- dbReadTable(conn, name)
       
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
      
      return(d)
    }
}