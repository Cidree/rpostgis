## pgCreateRast

##' Load raster into PostGIS database.
##'
##' Sends R raster to a new PostGIS database table.
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema (if
##'     necessary) and table name to hold the
##'     raster (e.g., name = c("schema","table"))
##' @param raster An R raster object
##' @param bit_depth The bit depth of the raster. Will be set to 32-bit
##'     (unsigned int, signed int, or float, depending on the data)
##'     if left null, but can be specified (as character) as one of the
##'     PostGIS pixel types (see \url{http://postgis.net/docs/RT_ST_BandPixelType.html})
##' @param constraints Whether to create constraints from raster data. Recommened
##'     to leave TRUE unless applying constraints manually (see
##'     \url{http://postgis.net/docs/RT_AddRasterConstraints.html}).
##'     Note that constraint notices may print to the console,
##'      depending on the PostgreSQL server settings.
##' @param overwrite Whether to overwrite the existing table (\code{name}).
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @importFrom raster res blockSize extent t as.matrix
##' @export
##' @return TRUE for successful import.
##' 
##' @seealso Function follows process from 
##' \url{http://postgis.net/docs/using_raster_dataman.html#RT_Creating_Rasters}.
##' @examples
##' \dontrun{
##' pgCreateRast(conn, c("schema", "tablename"), raster_name)
##'
##' # basic test
##' r<-raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, vals=1)
##' pgCreateRast(conn, c("schema", "test"), raster = r, bit_depth = "2BUI", overwrite = TRUE)
##' }

pgCreateRast <- function(conn, name, raster, bit_depth = NULL, 
    constraints = TRUE, overwrite = FALSE) {
    
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    
    nameq <- dbTableNameFix(conn, name)
    
    if (overwrite) {
        dbDrop(conn, name, ifexists = TRUE)
    }
    
    # 1. create raster table
    tmp.query <- paste0("CREATE TABLE ", paste(nameq, collapse = "."), 
        " (rid serial primary key, rast raster);")
    dbExecute(conn, tmp.query)
    
    r1 <- raster
    res <- round(raster::res(r1), 10)
    
    # figure out block size
    tr <- raster::blockSize(r1, 10000, minblocks = 1, minrows = 80)
    cr <- raster::blockSize(raster::t(r1), 10000, minblocks = 1, 
        minrows = 80)
    
    message("Splitting into ", cr$n, " x ", tr$n, " blocks...")
    
    # figure out bit depth
    if (is.null(bit_depth)) {
        if (is.integer(raster::values(r1))) {
            if (min(raster::values(r1), na.rm = TRUE) >= 0) {
                bit_depth <- "32BUI"
            } else {
                bit_depth <- "32BSI"
            }
        } else {
            bit_depth <- "32BF"
        }
    }
    bit_depth <- dbQuoteString(conn, bit_depth)
    ndval<--99999
    
    # rid counter
    n <- 0
    
    # loop over blocks
    for (i in 1:tr$n) {
        rr <- r1[tr$row[i]:(tr$row[i] + tr$nrows[i] - 1), , drop = FALSE]
        
        for (l in 1:cr$n) {
            r <- rr[, cr$row[l]:(cr$row[l] + cr$nrows[l] - 1), 
                drop = FALSE]
            ex <- raster::extent(r)
            d <- dim(r)
            
            # rid counter
            n <- n + 1
            
            srid <- suppressMessages(pgSRID(conn, r@crs))
            
            # 2. make empty raster
            tmp.query <- paste0("INSERT INTO ", paste(nameq, 
                collapse = "."), " (rid, rast) VALUES (", n, 
                ", ST_MakeEmptyRaster(", 
                d[2], ",", d[1], ",", ex[1], ",", ex[3], ",", 
                res[1], ",", res[2], ", 0, 0,", srid, ") );")
            dbExecute(conn, tmp.query)
            
            # 3. new band
            tmp.query <- paste0("UPDATE ", paste(nameq, collapse = "."), 
                " SET rast = ST_AddBand(rast,", bit_depth, "::text, 0,", ndval ,")
                where rid = ", 
                n, ";")
            dbExecute(conn, tmp.query)
            
            mr <- raster::as.matrix(r)
            mr[is.na(mr)] <- ndval
            r2 <- paste(rev(apply(mr, 1, FUN = function(x) {
                paste0("[", paste(x, collapse = ","), "]")
            })), collapse = ",")
            
            tmp.query <- paste0("UPDATE ", paste(nameq, collapse = "."), 
                " SET rast = ST_SetValues(rast,1, 1, 1, ARRAY[", 
                r2, "]::double precision[][])
                             where rid = ", 
                n, ";")
            dbExecute(conn, tmp.query)
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
            name[1]), "::name,", dbQuoteString(conn, name[2]), 
            "::name, 'rast'::name);")
        dbExecute(conn, tmp.query)
    }
    
    return(TRUE)
}