## pgGetRast

##' Load raster from PostGIS database.
##'
##' Retrieve rasters from a PostGIS table.
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., \code{name =
##'     c("schema","table")})
##' @param rast Name of the column in \code{name} holding the raster object
##' @param band Index number for the band to retrieve (defaults to 1)
##' @param digits numeric (default = 5), precision for detecting whether cells are
##'     on a regular grid (a low number of digits is a low precision).
##'     Unequal cell lengths (to this precision) in either X or Y dimensions will
##'     result in an error. From \code{\link[raster]{rasterFromXYZ}} 
##'     function (\code{raster} package).
##' @param boundary \code{sp} object or numeric. A Spatial* object,
##'     whose bounding box will be used to select the part of the
##'     raster to import. Alternatively, four numbers
##'     (e.g. \code{c([top], [bottom], [right], [left])}) indicating the
##'     projection-specific limits with which to clip the raster. \code{boundary = NULL}
##'     (default) will return the full raster.
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @importFrom raster rasterFromXYZ
##' @importFrom sp CRS
##' @export
##' @return RasterLayer
##' @examples
##' \dontrun{
##' pgGetRast(conn, c("schema", "tablename"))
##' pgGetRast(conn, c("schema", "DEM"), digits = 9, boundary = c(55,
##'     50, 17, 12))
##' }

pgGetRast <- function(conn, name, rast = "rast", bands = 1, digits = 5,
    boundary = NULL) {
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
      stop("PostGIS is not enabled on this database.")
    }
    ## Check and prepare the schema.name
    name1 <- dbTableNameFix(conn,name)
    nameque <- paste(name1, collapse = ".")
    namechar <- gsub("'","''",paste(gsub('^"|"$', '', name1),collapse="."))
    ## Check table exists
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
    tmp.query <- paste0("SELECT st_numbands(", rast, ") FROM ",
        nameque, " WHERE ", rast, " IS NOT NULL LIMIT 1;")
    nbs <- 1:dbGetQuery(conn, tmp.query)[1,1]
    if (!all(bands %in% nbs)) {
      stop(paste0("Selected band(s) do not exist in PostGIS raster: choose bands numbers between ",
                  min(nbs) , " and " , max(nbs), "."))
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", rast, ")) FROM ",
        nameque, " WHERE ", rast, " IS NOT NULL;")
    srid <- dbGetQuery(conn, tmp.query)
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) > 1) {
        stop("Multiple SRIDs in the raster")
    } else if (nrow(srid) < 1) {
        stop("Database table is empty.")
    }
    p4s <- sp::CRS(as.character(NA))@projargs
    tmp.query <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
                        srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query)$p4s
    if (!is.null(db.proj4)) {
      try(p4s <- sp::CRS(db.proj4)@projargs, silent = TRUE)
    }
    if (is.na(p4s)) {
      warning("Table SRID not found. Projection will be undefined (NA)")
    }
    
    # get alignment
    tmp.query <- paste0("select min(st_upperleftx(",rast,")) ux, max(st_upperlefty(",rast,")) uy FROM ",
          nameque,";")
    aligner <- dbGetQuery(conn, tmp.query)
    
    # get rast
    if (is.null(boundary)) {
        
        for (b in bands) {
          info <- dbGetQuery(conn, paste0("select 
              st_xmax(st_envelope(rast)) as xmx,
              st_xmin(st_envelope(rast)) as xmn,
              st_ymax(st_envelope(rast)) as ymx,
              st_ymin(st_envelope(rast)) as ymn,
              st_width(rast) as cols,
              st_height(rast) as rows
              from
              (select st_union(st_snaptogrid(", rast,",",aligner[1,1],",",aligner[1,2],"),",b,") rast from ",nameque,") as a;"))
  
          vals <- dbGetQuery(conn,paste0("select
            unnest(st_dumpvalues(rast, 1)) as vals 
            from
            (select st_union(st_snaptogrid(", rast,",",aligner[1,1],",",aligner[1,2],"),",b,") rast from ",nameque,") as a;"))$vals
          
          rout <- raster::raster(nrows = info$rows, ncols = info$cols, 
            xmn = info$xmn, xmx = info$xmx, ymn = info$ymn, ymx = info$ymx,
            crs = sp::CRS(p4s), val = vals)
          
          if(length(bands) > 1) {
            if (b == bands[1]) {
              rb <- raster::brick(rout)
            } else {
              rb[[raster::nlayers(rb)+1]] <- rout
            }
          }
        }
        
    } else {
        if (typeof(boundary) != "double") {
            boundary <- c(boundary@bbox[2, 2], boundary@bbox[2,
                1], boundary@bbox[1, 2], boundary@bbox[1, 1])
        }
      
        for (b in bands) {
          info <- dbGetQuery(conn, paste0("select 
              st_xmax(st_envelope(rast)) as xmx,
              st_xmin(st_envelope(rast)) as xmn,
              st_ymax(st_envelope(rast)) as ymx,
              st_ymin(st_envelope(rast)) as ymn,
              st_width(rast) as cols,
              st_height(rast) as rows
              from
              (select st_union(st_snaptogrid(", rast,",",aligner[1,1],",",aligner[1,2],"),",b,") rast from ",nameque, "\n
              WHERE ST_Intersects(",
              rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
              " ", boundary[1], ",", boundary[4], " ", boundary[2],
              ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
              " ", boundary[1], ",", boundary[4], " ", boundary[1],
              "))'),", srid, "))) as a;"))
  
          vals <- dbGetQuery(conn,paste0("select
            unnest(st_dumpvalues(rast, 1)) as vals 
            from
            (select st_union(st_snaptogrid(", rast,",",aligner[1,1],",",aligner[1,2],"),",b,") rast from ",nameque, "\n
              WHERE ST_Intersects(",
              rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
              " ", boundary[1], ",", boundary[4], " ", boundary[2],
              ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
              " ", boundary[1], ",", boundary[4], " ", boundary[1],
              "))'),", srid, "))) as a;"))$vals  
          
          rout <- raster::raster(nrows = info$rows, ncols = info$cols, 
            xmn = info$xmn, xmx = info$xmx, ymn = info$ymn, ymx = info$ymx,
            crs = sp::CRS(p4s), val = vals)
          
          if(length(bands) > 1) {
              if (b == bands[1]) {
                rb<-raster::brick(rout)
              } else {
                rb <- rb[[raster::nlayers(rb)+1]]
              }
          }
        }
    }
    
    if(length(bands) > 1) {rout <- rb}
    
    # set layer names
    if("band_names" %in% dbTableInfo(conn,name)$column_name) {
      try({
        ct<-1
        for (b in bands) {
          lnm<-dbGetQuery(conn, paste0("SELECT DISTINCT band_names[",b,
                                       "][1] as nm FROM ",nameque,";"))
          names(rout)[ct]<-lnm$nm
          ct<-ct+1
        }
      })
    }
    return(rout)
}
