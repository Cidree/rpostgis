library(rpostgisLT)
data("db_gps_data")
d<-db_gps_data$GSM01438[,1:14]

d$acquisition_time <- as.POSIXct(as.POSIXct(paste0(d$utc_date," ",d$utc_time),
                                                 format = "%d/%m/%y %H:%M:%S", tz = "UTC")
                                   , tz = "America/New_York")

d$posixlt<- as.POSIXlt(d$acquisition_time, tz = "America/New_York")


##### infolocs definition section #####
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
      info_nm <- paste0("'{{", paste(names(d), collapse = ","), 
          "},{", paste(as.character(types), collapse = ","), "},{", 
          paste(as.character(attr2), collapse = ","), "}}'")
      
      # write info_nm to animal_burst.info_cols
      sql_query <- paste0("UPDATE ", schemaq, ".animal_burst 
                          SET (info_cols) = (",info_nm, ")
                          FROM ", schemaq, ".pgtraj
                          WHERE pgtraj.id = animal_burst.pgtraj_id
                          AND pgtraj.pgtraj_name = ",dbQuoteString(conn, pgtraj),
                          " AND animal_burst.burst_name = ",dbQuoteString(conn,b_nm),";")
      dbExecute(conn, sql_query)
      
      # add to final data frame
      iloc_df<-rbind(iloc_df,d)
    }
    ##### end infolocs definition section #####