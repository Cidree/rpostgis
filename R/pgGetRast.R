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
##' @param digits numeric, precision for detecting whether points are
##'     on a regular grid (a low number of digits is a low precision)
##'     - From \code{\link[raster]{rasterFromXYZ}} function (\code{raster} package)
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

pgGetRast <- function(conn, name, rast = "rast", band = 1, digits = 9,
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
    if (is.null(boundary)) {
        trast <- suppressWarnings(dbGetQuery(conn, paste0("SELECT ST_X(ST_Centroid((gv).geom)) AS x, ST_Y(ST_Centroid((gv).geom)) AS y,\n  (gv).val FROM (SELECT ST_PixelAsPolygons(",
            rast, ",",band,", FALSE) AS gv FROM ", nameque, ") a;")))
    } else {
        if (typeof(boundary) != "double") {
            boundary <- c(boundary@bbox[2, 2], boundary@bbox[2,
                1], boundary@bbox[1, 2], boundary@bbox[1, 1])
        }
        trast <- suppressWarnings(dbGetQuery(conn, paste0("SELECT ST_X(ST_Centroid((gv).geom)) AS x, ST_Y(ST_Centroid((gv).geom)) AS y,\n  (gv).val FROM (SELECT ST_PixelAsPolygons(ST_Clip(",
            rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
            " ", boundary[1], ",", boundary[4], " ", boundary[2],
            ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
            " ", boundary[1], ",", boundary[4], " ", boundary[1],
            "))'),", srid, ")),",band,", FALSE) AS gv FROM ", nameque, "\n  WHERE ST_Intersects(",
            rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
            " ", boundary[1], ",", boundary[4], " ", boundary[2],
            ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
            " ", boundary[1], ",", boundary[4], " ", boundary[1],
            "))'),", srid, "))) a;")))
    }
    rout<-raster::rasterFromXYZ(trast, crs = sp::CRS(p4s), digits = digits)
    
    # set NA value
    ndval<-dbGetQuery(conn, paste0("SELECT st_bandnodatavalue(",
                                   rast,",",band,") as nd from ",nameque,";"))$nd[1]
    rout[rout==ndval]<-NA
    
    # set layer name
    if("band_names" %in% dbListFields(conn, name)) {
      try({
      lnm<-dbGetQuery(conn, paste0("SELECT DISTINCT band_names[",band,
                                   "][1] as nm FROM ",nameque,";"))
      names(rout)<-lnm$nm
      })
    }
    return(rout)
}