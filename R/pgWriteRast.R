## pgWriteRast

##' Write raster to PostGIS database table.
##'
##' Sends R raster to a new PostGIS database table.
##' 
##' RasterLayer names will be stored in an array in the column
##' "band_names", which will be restored in R when imported with the function
##' \code{\link[rpostgis]{pgGetRast}}.
##' 
##' Rasters from the \code{sp} package are converted to \code{raster}
##' package objects prior to insert.
##' 
##' If \code{blocks = NULL} the attempted block size will be around
##' 10,000 pixels in size (100 x 100 cells), so number of blocks will
##' vary by raster size. If a specified number of blocks is desired,
##' set blocks to a one or two-length integer vector. Note that fewer, larger
##' blocks generally results in faster write times.
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema (if
##'     necessary) and table name to hold the
##'     raster (e.g., \code{name = c("schema","table")})
##' @param raster An R \code{RasterLayer}, \code{RasterBrick}, or \code{RasterStack} from 
##'     raster package; a \code{SpatialGrid*} or \code{SpatialPixels*} from sp package
##' @param bit.depth The bit depth of the raster. Will be set to 32-bit
##'     (unsigned int, signed int, or float, depending on the data)
##'     if left null, but can be specified (as character) as one of the
##'     PostGIS pixel types (see \url{http://postgis.net/docs/RT_ST_BandPixelType.html})
##' @param blocks Optional desired number of blocks (tiles) to split the raster
##'     into in the resulting PostGIS table. This should be specified as a
##'     one or two-length (columns, rows) integer vector.
##' @param constraints Whether to create constraints from raster data. Recommended
##'     to leave \code{TRUE} unless applying constraints manually (see
##'     \url{http://postgis.net/docs/RT_AddRasterConstraints.html}).
##'     Note that constraint notices may print to the console,
##'     depending on the PostgreSQL server settings.
##' @param overwrite Whether to overwrite the existing table (\code{name}).
##' @author David Bucklin \email{david.bucklin@@gmail.com}
##' @importFrom raster res blockSize extent t as.matrix values values<-
##' @importFrom methods as
##' @importFrom sp SpatialPixelsDataFrame
##' @export
##' @return TRUE for successful import.
##' 
##' @seealso Function follows process from 
##' \url{http://postgis.net/docs/using_raster_dataman.html#RT_Creating_Rasters}.
##' @examples
##' \dontrun{
##' pgWriteRast(conn, c("schema", "tablename"), raster_name)
##'
##' # basic test
##' r <- raster::raster(nrows=180, ncols=360, xmn=-180, xmx=180,
##'     ymn=-90, ymx=90, vals=1)
##' pgWriteRast(conn, c("schema", "test"), raster = r,
##'     bit.depth = "2BUI", overwrite = TRUE)
##' }

pgWriteRast <- function(conn, name, raster, bit.depth = NULL, 
    blocks = NULL, constraints = TRUE, overwrite = FALSE) {
    
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
  
    r_class <- dbQuoteString(conn, class(raster)[1])
  
    # sp-handling
    if (class(raster)[1] %in% c("SpatialPixelsDataFrame","SpatialGridDataFrame","SpatialGrid","SpatialPixels")) {
      if (class(raster) %in% c("SpatialGrid", "SpatialPixels") || length(raster@data) < 2) {
        # SpatialPixels needs a value
        if (class(raster) == "SpatialPixels") raster <- SpatialPixelsDataFrame(raster, data = data.frame(rep(0, length(raster))))
        raster <- as(raster, "RasterLayer")
      } else {
        raster <- as(raster, "RasterBrick")
      }
    }
    
    # crs
    r_crs <- dbQuoteString(conn, as.character(raster@crs))
  
    nameq <- dbTableNameFix(conn, name)
    namef <- dbTableNameFix(conn, name, as.identifier = FALSE)
    
    if (overwrite) {
        dbDrop(conn, name, ifexists = TRUE)
    }
    
    # 1. create raster table
    tmp.query <- paste0("CREATE TABLE ", paste(nameq, collapse = "."), 
        " (rid serial primary key, band_names text[], r_class character varying, r_proj4 character varying, rast raster);")
    dbExecute(conn, tmp.query)
    
    r1 <- raster
    res <- round(raster::res(r1), 10)

    # figure out block size
    if (!is.null(blocks)) {
      bs <- bs(r1, blocks)
      tr <- bs$tr
      cr <- bs$cr
    } else {
      tr <- raster::blockSize(r1[[1]], 10000, minblocks = 1, minrows = 100)
      cr <- raster::blockSize(raster::t(r1[[1]]), 10000, minblocks = 1, 
        minrows = 100)
    }
    
    message("Splitting ",length(names(r1))," band(s) into ", cr$n, " x ", tr$n, " blocks...")
    
    # figure out bit depth
    if (is.null(bit.depth)) {
        if (is.integer(raster::values(r1))) {
            if (min(raster::values(r1), na.rm = TRUE) >= 0) {
                bit.depth <- "32BUI"
            } else {
                bit.depth <- "32BSI"
            }
        } else {
            bit.depth <- "32BF"
        }
    }
    bit.depth <- dbQuoteString(conn, bit.depth)
    ndval<--99999
    
    # band names
    bnds<-dbQuoteString(conn, paste0("{{",paste(names(r1),collapse = "},{"),"}}"))
    
    srid <- 0
    try(srid <- suppressMessages(pgSRID(conn, r1@crs, create.srid = TRUE)))
    
    # loop over bands
    for (b in 1:length(names(r1))) {
      rb <- r1[[b]]
      # rid counter
      n<-0
      
      # handle empty data rasters by setting ndval to all values
      if (all(is.na(values(rb)))) values(rb) <- ndval
      
      # loop over blocks
      for (i in 1:tr$n) {
          suppressWarnings(rr <- rb[tr$row[i]:(tr$row[i] + tr$nrows[i] - 1), , drop = FALSE])
          
          for (l in 1:cr$n) {
              suppressWarnings(r <- rr[, cr$row[l]:(cr$row[l] + cr$nrows[l] - 1), 
                  drop = FALSE])
              ex <- raster::extent(r)
              d <- dim(r)

              # rid counter
              n <- n + 1
              
              # only ST_MakeEmptyRaster/ST_AddBand during first band loop
              if (b == 1) {
                
                # 2. make empty raster
                tmp.query <- paste0("INSERT INTO ", paste(nameq, 
                    collapse = "."), " (rid, band_names, r_class, r_proj4, rast) VALUES (",n, 
                    ",",bnds,",",r_class,",",r_crs,", ST_MakeEmptyRaster(", 
                    d[2], ",", d[1], ",", ex[1], ",", ex[4], ",", 
                    res[1], ",", -res[2], ", 0, 0,", srid[1], ") );")
                dbExecute(conn, tmp.query)
                
                # upper left x/y for alignment snapping
                if (l == 1 & i == 1) {
                  tmp.query <- paste0("SELECT st_upperleftx(rast) x FROM ", paste(nameq, collapse = ".") ," where rid = 1;")
                  upx <- dbGetQuery(conn, tmp.query)$x
                  tmp.query <- paste0("SELECT st_upperlefty(rast) y FROM ", paste(nameq, collapse = ".") ," where rid = 1;")
                  upy <- dbGetQuery(conn, tmp.query)$y
                }
                
                # 3. new band
                if (res[1] != res[2]) s2g <- paste0(", ", res[1], ", ", -res[2]) else s2g <- NULL
                bndargs<-paste0("ROW(",1:length(names(r1)),",",bit.depth,"::text,0,", ndval,")")
                tmp.query <- paste0("UPDATE ", paste(nameq, collapse = "."), 
                    " SET rast = ST_SnapToGrid(ST_AddBand(rast,ARRAY[",
                    paste(bndargs,collapse = ","),"]::addbandarg[]), ", upx, "," , upy , s2g, ") ", 
                    "where rid = ", 
                    n, ";")
                dbExecute(conn, tmp.query)
              }
              
              mr <- raster::as.matrix(r)
              mr[is.na(mr)] <- ndval
              r2 <- paste(apply(mr, 1, FUN = function(x) {
                  paste0("[", paste(x, collapse = ","), "]")
              }), collapse = ",")
              
              tmp.query <- paste0("UPDATE ", paste(nameq, collapse = "."), 
                  " SET rast = ST_SetValues(rast,",b,", 1, 1, ARRAY[", 
                  r2, "]::double precision[][])
                               where rid = ", 
                  n, ";")
              dbExecute(conn, tmp.query)
          }
      }
    }
    
    # 4. create index
    tmp.query <- paste0("CREATE INDEX ", gsub("\"", "", nameq[2]), 
        "_rast_st_conhull_idx ON ", paste(nameq, collapse = "."), 
        " USING gist( ST_ConvexHull(rast) );")
    dbExecute(conn, tmp.query)
    
    if (constraints) {
        # 5. add raster constraints
        tmp.query <- paste0("SELECT AddRasterConstraints(", dbQuoteString(conn, 
            namef[1]), "::name,", dbQuoteString(conn, namef[2]), 
            "::name, 'rast'::name);")
        dbExecute(conn, tmp.query)
    }
    
    return(TRUE)
}
