## pgGetRast

##' Load raster from PostGIS database.
##'
##' Retrieve rasters from a PostGIS table into a \code{terra SpatRaster} object
##' 
##' 
##' This function does not return anymore \code{raster}-class objects, in favour
##' or the \code{SpatRaster} object of the \code{terra} package. If you write
##' \code{raster}-class objects using \link[rpostgis]{pgWriteRast()}, this function will
##' return a \code{terra} object instead. 
##' 
##' The argument \code{bands} can take as argument:
##' 
##' * The index of the desirable band (e.g. bands = 2 will fetch the second band
##' ot the raster).
##' 
##' * More than one index for several bands (e.g. bands = c(2,4) will return a
##' \code{SpatRaster} with two bands).
##' 
##' * All bands in the raster (bands = TRUE).
##'
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., \code{name =
##'     c("schema","table")})
##' @param rast Name of the column in \code{name} holding the raster object. 
##' Defaults to "rast".
##' @param clauses character, optional SQL to append to modify select
##'     query from table. Must begin with 'WHERE'.
##' @param bands Index number(s) for the band(s) to retrieve (defaults to 1).
##' The special case (\code{bands = TRUE}) returns all bands in the raster. See
##' also 'Details'
##' @param boundary \code{sf} object, \code{SpatVector} object, or numeric. If a spatial object
##' is provided, its bounding box will be used to select the part of the raster
##' to import. Alternatively, a numeric vector (\code{c([xmin], [ymin], [xmax], [ymax])}) 
##' indicating the projection-specific limits with which to clip the raster. If
##' not value is provided, the default \code{boundary = NULL} will return the 
##' full raster.
##'      
##' @author David Bucklin \email{david.bucklin@@gmail.com} and Adrián Cidre
##' González \email{adrian.cidre@@gmail.com}
##' @importFrom terra rast ext crs crop
##' @importFrom sf st_crs st_bbox
##' @export
##' @return A \code{terra SpatRaster} object
##' @examples
##' \dontrun{
##' pgGetRast(conn, c("schema", "tablename"))
##' pgGetRast(conn, c("schema", "DEM"), boundary = c(12,
##'     50, 17, 55))
##' }

pgGetRast <- function(conn, name, rast = "rast", bands = 1,
    boundary = NULL, clauses = NULL) {
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
      stop("PostGIS is not enabled on this database.")
    }
    ## Check and prepare the schema.name
    name1 <- dbTableNameFix(conn, name)
    nameque <- paste(name1, collapse = ".")
    namechar <- gsub("'", "''", paste(gsub('^"|"$', '', name1), collapse = "."))
    
    ## rast query name
    rastque <- dbQuoteIdentifier(conn, rast)
    
    clauses2 <- sub("^where", "AND", clauses, ignore.case = TRUE)
    
    ## Check table exists and return error if it does not exist
    tmp.query <- paste0("SELECT r_raster_column AS geo FROM raster_columns\n  WHERE (r_table_schema||'.'||r_table_name) = '",
                        namechar, "';")
    tab.list <- dbGetQuery(conn, tmp.query)$geo
    if (is.null(tab.list)) {
        stop(paste0("Table '", namechar, "' is not listed in raster_columns."))
    } else if (!rast %in% tab.list) {
        stop(paste0("Table '", namechar, "' raster column '", rast,
            "' not found. Available raster columns: ", paste(tab.list,
                collapse = ", ")))
    }
    
    ## Check bands
    tmp.query <- paste0("SELECT st_numbands(", rastque, ") FROM ",
        nameque, " WHERE ", rastque, " IS NOT NULL LIMIT 1;")
    nbs <- 1:dbGetQuery(conn, tmp.query)[1,1]
    if (is.logical(bands) && bands) {
      bands <- nbs
    } else if (!all(bands %in% nbs)) {
      stop(paste0("Selected band(s) do not exist in PostGIS raster: choose bands numbers between ",
                  min(nbs) , " and " , max(nbs), "."))
    }
    
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", rastque, ")) FROM ",
        nameque, " WHERE ", rastque, " IS NOT NULL;")
    srid <- dbGetQuery(conn, tmp.query)
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) > 1) {
        stop("Multiple SRIDs in the raster")
    } else if (nrow(srid) < 1) {
        stop("Database table is empty.")
    }
    p4s <- NA
    tmp.query <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
                        srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query)$p4s
    if (!is.null(db.proj4)) {
      try(p4s <- sf::st_crs(db.proj4)$proj4string, silent = TRUE)
    }
    if (is.na(p4s)) {
      warning("Table SRID not found. Projection will be undefined (NA)")
    }
    
    # check alignment of raster
    tmp.query <- paste0("select st_samealignment(",rastque,") from ",nameque,";")
    # needs postgis version 2.1+, so just try
    al <- FALSE
    try(al <- dbGetQuery(conn, tmp.query)[1,1])
    if (!al) {
      # get alignment from upper left pixel of all raster tiles
      tmp.query <- paste0("select min(st_upperleftx(",rastque,")) ux, max(st_upperlefty(",rastque,")) uy FROM ",
            nameque,";")
      aligner <- dbGetQuery(conn, tmp.query)
      aq <- c("ST_SnapToGrid(", paste0(aligner[1,1],","), paste0(aligner[1,2],"),"))
    } else {
      aq <- NULL
    }
    
    # get rast
    if (is.null(boundary)) {
        
        for (b in bands) {
          info <- dbGetQuery(conn, paste0("select 
              st_xmax(st_envelope(rast)) as xmax,
              st_xmin(st_envelope(rast)) as xmin,
              st_ymax(st_envelope(rast)) as ymax,
              st_ymin(st_envelope(rast)) as ymin,
              st_width(rast) as cols,
              st_height(rast) as rows
              from
              (select st_union(",aq[1],rastque,",",aq[2],aq[3],b,") rast from ",nameque," ", clauses,") as a;"))
  
          vals <- dbGetQuery(conn, paste0("select
            unnest(st_dumpvalues(rast, 1)) as vals 
            from
            (select st_union(",aq[1],rastque,",",aq[2],aq[3],b,") rast from ",nameque," ", clauses,") as a;"))$vals
          
          rout <- terra::rast(nrows = info$rows, ncols = info$cols, 
                              xmin = info$xmin, xmax = info$xmax, ymin = info$ymin, ymax = info$ymax,
                              crs = p4s, vals = vals)
          
          if (length(bands) > 1) {
            if (b == bands[1]) {
              rb <- rout
            } else {
              rb <- c(rb, rout)
            }
          }
        }
        
    } else {
      
      ## Bbox of terra and sf objects
      if (inherits(boundary, "sf")) {
        boundary <- sf::st_bbox(boundary)
      } else if (inherits(boundary, "SpatVector")) {
        boundary <- c(terra::ext(boundary)[1], terra::ext(boundary)[3],
                      terra::ext(boundary)[2], terra::ext(boundary)[4])
      }
      
      ## Extent to clip the Rast
      extclip <- terra::ext(boundary[1], boundary[3], boundary[2], boundary[4])
     
      for (b in bands) {
        info <- dbGetQuery(conn, paste0("select 
              st_xmax(st_envelope(rast)) as xmx,
              st_xmin(st_envelope(rast)) as xmn,
              st_ymax(st_envelope(rast)) as ymx,
              st_ymin(st_envelope(rast)) as ymn,
              st_width(rast) as cols,
              st_height(rast) as rows
              from
              (select st_union(",aq[1],rastque,",",aq[2],aq[3],b,") rast from ",nameque, "\n
              WHERE ST_Intersects(",
              rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[1],
              " ", boundary[4], ",", boundary[1], " ", boundary[3],
              ",\n  ", boundary[2], " ", boundary[3], ",", boundary[2],
              " ", boundary[4], ",", boundary[1], " ", boundary[4],
              "))'),", srid, "))", clauses2,") as a;"))
        if (is.na(info$cols) & is.na(info$rows)) {
          stop("No data found within geographic subset defined by 'boundary'.")
        }
        
        vals <- dbGetQuery(conn,paste0("select
            unnest(st_dumpvalues(rast, 1)) as vals 
            from
            (select st_union(",aq[1],rastque,",",aq[2],aq[3],b,") rast from ",nameque, "\n
              WHERE ST_Intersects(",
            rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[1],
            " ", boundary[4], ",", boundary[1], " ", boundary[3],
            ",\n  ", boundary[2], " ", boundary[3], ",", boundary[2],
            " ", boundary[4], ",", boundary[1], " ", boundary[4],
            "))'),", srid, "))", clauses2,") as a;"))$vals  
        
        rout <- terra::rast(nrows = info$rows, ncols = info$cols, 
                            xmin = info$xmin, xmax = info$xmax, ymin = info$ymin, ymax = info$ymax,
                            crs = p4s, vals = vals)
        
        if (length(bands) > 1) {
          if (b == bands[1]) {
            rb <- terra::rast(rout)
          } else {
            rb <- c(rb, rout)
          }
        }
      }
       
    }
    
    if (length(bands) > 1) rout <- rb
    
    # set layer names
    if ("band_names" %in% dbTableInfo(conn,name)$column_name) {
      try({
        ct <- 1
        for (b in bands) {
          lnm <- dbGetQuery(conn, paste0("SELECT DISTINCT band_names[",b,
                                       "][1] as nm FROM ",nameque," ", clauses,";"))
          names(rout)[ct] <- lnm$nm
          ct <- ct + 1
        }
      })
    }
    
    # precise cropping
    if (!is.null(boundary)) {
      rout <- terra::crop(rout, extclip)
    }
    
    # get/set proj4
    r_crs <- NULL
    if ("r_proj4" %in% dbTableInfo(conn, name)$column_name) {
      try({
        r_crs <- dbGetQuery(conn, paste0("SELECT DISTINCT r_proj4 FROM ",
                                       nameque," WHERE ", rastque ," IS NOT NULL ", clauses2,";"))[,1]
        if (length(r_crs) == 1 & !is.null(r_crs) & !is.na(r_crs)) {
          r_crs <- sf::st_crs(r_crs)
          suppressMessages(suppressWarnings(
            if (any(pgSRID(conn, sf::st_crs(terra::crs(rout))) %in% pgSRID(conn, r_crs))) terra::crs(rout) <- r_crs
          ))
        }
      }, silent = TRUE)
    }
    
    # get/set class
    # r_class <- NULL
    # if ("r_class" %in% dbTableInfo(conn, name)$column_name) {
    #   try({
    #     r_class <- dbGetQuery(conn, paste0("SELECT DISTINCT r_class FROM ",
    #                                    nameque," WHERE ", rastque ," IS NOT NULL ", clauses2,";"))[,1]
    #     if (length(r_class) == 1 & !is.null(r_class) & 
    #         r_class %in% c("SpatialPixelsDataFrame","SpatialGridDataFrame","SpatialGrid","SpatialPixels")) rout <- as(rout, r_class)
    #   }, silent = TRUE)
    # }
    return(rout)
}
