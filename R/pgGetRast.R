## pgGetRast

##' Load raster from PostGIS database into R.
##'
##' Retrieve rasters from a PostGIS table into a `terra SpatRaster` object
##'
##'
##' Since version 1.5.0, this function retrieve SpatRaster objects from
##' `terra` package by default. The argument `returnclass` can be
##' used to return `raster` objects instead.
##'
##' The argument `bands` can take as argument:
##'
##' * The index of the desirable band (e.g. bands = 2 will fetch the second band
##' of the raster).
##'
##' * More than one index for several bands (e.g. bands = c(2,4) will return a
##' `SpatRaster` with two bands).
##'
##' * All bands in the raster (bands = TRUE).
##'
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., `name =
##'     c("schema","table")`)
##' @param rast Name of the column in `name` holding the raster object.
##' Defaults to "rast".
##' @param clauses character, optional SQL to append to modify select
##'     query from table. Must begin with 'WHERE'.
##' @param bands Index number(s) for the band(s) to retrieve (defaults to 1).
##' The special case (`bands = TRUE`) returns all bands in the raster. See
##' also 'Details'
##' @param boundary `sf` object, `SpatVector` object, or numeric. If a
##'     spatial object is provided, its bounding box will be used to select
##'     the part of the raster to import. Alternatively, a numeric vector
##'     (`c([top], [bottom], [right], [left])`) indicating the
##'     projection-specific limits with which to clip the raster. If not value
##'     is provided, the default `boundary = NULL` will return the
##'     full raster.
##' @param returnclass 'terra' by default; or 'raster' for `raster` objects.
##' @param progress whether to show a progress bar (TRUE by default). The progress
##'     bar mark the progress of reading bands from the database.
##'
##' @author David Bucklin \email{david.bucklin@@gmail.com} and Adrián Cidre
##' González \email{adrian.cidre@@gmail.com}
##' @importFrom terra rast ext crs crop
##' @importFrom sf st_crs st_bbox
##' @export
##' @return `SpatRaster`; `raster`; or `RasterStack` object
##' @examples
##' \dontrun{
##' pgGetRast(conn, c("schema", "tablename"))
##' pgGetRast(conn, c("schema", "DEM"), boundary = c(55,
##'     50, 17, 12))
##' }

pgGetRast <- function(conn, name, rast = "rast", bands = 1,
                      boundary = NULL, clauses = NULL,
                      returnclass = "terra", progress = TRUE) {

  ## warning on `sp` use
  warn_deprecated_rc(
    returnclass,
    "pgGetRast(returnclass = 'should be a `terra` object')"
  )

  ## Check connection and PostGIS extension
  dbConnCheck(conn)
  if (!suppressMessages(pgPostGIS(conn))) {
    cli::cli_abort("PostGIS is not enabled on this database.")
  }

  ## Check and prepare the schema.name
  name1    <- dbTableNameFix(conn, name)
  nameque  <- paste(name1, collapse = ".")
  namechar <- gsub("'", "''", paste(gsub('^"|"$', '', name1), collapse = "."))

  ## Raster query name
  rastque <- dbQuoteIdentifier(conn, rast)

  ## Fix user clauses
  clauses2 <- sub("^where", "AND", clauses, ignore.case = TRUE)

  ## Check table exists and return error if it does not exist
  tmp.query <- paste0("SELECT r_raster_column AS geo FROM raster_columns\n  WHERE (r_table_schema||'.'||r_table_name) = '",
                      namechar, "';")
  tab.list  <- dbGetQuery(conn, tmp.query)$geo
  if (is.null(tab.list)) {
    cli::cli_abort("Table '{namechar}' is not listed in raster_columns.")
  } else if (!rast %in% tab.list) {
    cli::cli_abort("Table '{namechar}' raster column '{rast}' not found. Available raster columns: {paste(tab.list, collapse = ', ')}")
  }

  ## Check bands
  tmp.query <- paste0("SELECT st_numbands(", rastque, ") FROM ", nameque, " WHERE ", rastque, " IS NOT NULL LIMIT 1;")

  nbs <- 1:dbGetQuery(conn, tmp.query)[1,1]
  if (isTRUE(bands)) {
    bands <- nbs
  } else if (!all(bands %in% nbs)) {
    cli::cli_abort("Selected band(s) do not exist in PostGIS raster: choose band numbers between {min(nbs)} and {max(nbs)}.")
  }

  ## Retrieve the SRID
  tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", rastque, ")) FROM ", nameque, " WHERE ", rastque, " IS NOT NULL;")

  srid      <- dbGetQuery(conn, tmp.query)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) > 1) {
    cli::cli_abort("Multiple SRIDs in the raster")
  } else if (nrow(srid) < 1) {
    cli::cli_abort("Database table is empty.")
  }
  ## Retrieve the SRID
  p4s <- NA
  tmp.query.sr <- paste0("SELECT r_proj4 AS p4s FROM ", nameque, ";")
  try(db.proj4 <- dbGetQuery(conn, tmp.query.sr)$p4s, silent = TRUE)

  # if db.proj4 doesnt exist (error because raster was not loaded using rpostgis)
  if (!exists("db.proj4")) {
    tmp.query.sr <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
                           srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query.sr)$p4s
  }
  if (!is.null(db.proj4)) {
    try(p4s <- terra::crs(db.proj4[1]), silent = TRUE)
  }
  if (is.na(p4s)) {
    cli::cli_alert_warning("Table SRID not found. Projection will be undefined (NA)")
  }

  ## Check alignment of raster
  tmp.query <- paste0("SELECT ST_SameAlignment(", rastque, ") FROM ", nameque, ";")
  # needs postgis version 2.1+, so just try
  al <- FALSE
  try(al <- dbGetQuery(conn, tmp.query)[1,1])
  if (!al) {
    # get alignment from upper left pixel of all raster tiles
    tmp.query <-  paste0("SELECT min(ST_UpperLeftX(", rastque, ")) ux, max(ST_UpperLeftY(", rastque, ")) uy FROM ", nameque, ";")

    aligner <- dbGetQuery(conn, tmp.query)
    aq <- c("ST_SnapToGrid(", paste0(aligner[1,1],","), paste0(aligner[1,2],"),"))
  } else {
    aq <- NULL
  }

  ## Get band function
  get_band <- function(band) {

    ## Get raster information (bbox, rows, cols)
    info <- dbGetQuery(conn, paste0("select
            st_xmax(st_envelope(rast)) as xmax,
            st_xmin(st_envelope(rast)) as xmin,
            st_ymax(st_envelope(rast)) as ymax,
            st_ymin(st_envelope(rast)) as ymin,
            st_width(rast) as cols,
            st_height(rast) as rows
            from
            (select st_union(",aq[1],rastque,",",aq[2],aq[3],band,") rast from ",nameque," ", clauses,") as a;"))
    ## Retrieve values of the cells
    vals <- dbGetQuery(conn, paste0("select
          unnest(st_dumpvalues(rast, 1)) as vals
          from
          (select st_union(",aq[1],rastque,",",aq[2],aq[3],band,") rast from ",nameque," ", clauses,") as a;"))$vals

    rout <- terra::rast(nrows = info$rows, ncols = info$cols, xmin = info$xmin,
                        xmax = info$xmax, ymin = info$ymin, ymax = info$ymax,
                        crs = p4s, vals = vals)

    return(rout)

  }

  ## Get band with boundary function
  get_band_boundary <- function(band) {

    ## Get info
    info <- dbGetQuery(conn, paste0("select
            st_xmax(st_envelope(rast)) as xmx,
            st_xmin(st_envelope(rast)) as xmn,
            st_ymax(st_envelope(rast)) as ymx,
            st_ymin(st_envelope(rast)) as ymn,
            st_width(rast) as cols,
            st_height(rast) as rows
            from
            (select st_union(",aq[1],rastque,",",aq[2],aq[3],band,") rast from ",nameque, "\n
            WHERE ST_Intersects(",
            rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
            " ", boundary[1], ",", boundary[4], " ", boundary[2],
            ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
            " ", boundary[1], ",", boundary[4], " ", boundary[1],
            "))'),", srid, "))", clauses2,") as a;"))
    if (is.na(info$cols) & is.na(info$rows)) {
      stop("No data found within geographic subset defined by 'boundary'.")
    }

    vals <- dbGetQuery(conn,paste0("select
          unnest(st_dumpvalues(rast, 1)) as vals
          from
          (select st_union(",aq[1],rastque,",",aq[2],aq[3],band,") rast from ",nameque, "\n
            WHERE ST_Intersects(",
          rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
          " ", boundary[1], ",", boundary[4], " ", boundary[2],
          ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
          " ", boundary[1], ",", boundary[4], " ", boundary[1],
          "))'),", srid, "))", clauses2,") as a;"))$vals

    rout <- terra::rast(nrows = info$rows, ncols = info$cols,
                        xmin = info$xmn, xmax = info$xmx, ymin = info$ymn, ymax = info$ymx,
                        crs = p4s, vals = vals)

    return(rout)
  }

  ## Get raster
  ## user feedback
  cli::cli_alert_info("Reading {length(bands)} band{?s}...")
  download_pb <- cli::cli_progress_bar(
    "Read bands",
    total       = length(bands),
    type        = "tasks",
    format_done = "{.alert-success Read completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  ## read
  rout <- list()
  if (is.null(boundary)) {
    ## Get bands
    for (i in 1:length(bands)) {
      rout[[i]] <- get_band(bands[i])
      cli::cli_progress_update(id = download_pb)
    }
    ## convert to 1 SpatRaster
    rb <- terra::rast(rout)

    ## Else: when boundary is provided
  } else {

    ## Bbox of terra and sf objects
    if (inherits(boundary, "sf")) {
      boundary <- sf::st_bbox(boundary)
      boundary <- c(boundary[4], boundary[2], boundary[3], boundary[1])
    } else if (inherits(boundary, "SpatVector")) {
      boundary <- c(terra::ext(boundary)[4], terra::ext(boundary)[3],
                    terra::ext(boundary)[2], terra::ext(boundary)[1])
    }

    ## Extent to clip the Rast
    extclip <- terra::ext(boundary[4], boundary[3], boundary[2], boundary[1])

    ## Get bands
    for (i in 1:length(bands)) {
      rout[[i]] <- get_band_boundary(bands[i])
      cli::cli_progress_update(id = download_pb)
    }
    rb   <- terra::rast(rout)
  }
  ## end user feedback
  cli::cli_process_done(id = download_pb)

  ## Set layer names
  if ("band_names" %in% dbTableInfo(conn,name)$column_name) {
    try({
      ct <- 1
      for (b in bands) {
        lnm <- dbGetQuery(conn, paste0("SELECT DISTINCT band_names[",b,
                                       "][1] as nm FROM ",nameque," ", clauses,";"))
        names(rb)[ct] <- lnm$nm
        ct <- ct + 1
      }
    })
  }

  # precise cropping
  if (!is.null(boundary)) {
    rb_final <- terra::crop(rb, extclip)
  } else {
    rb_final <- rb
  }

  # Output terra or raster
  if (returnclass == "terra") {
    return(rb_final)
  } else if (returnclass == "raster") {

    if (terra::nlyr(rb_final) == 1) {
      return(raster::raster(rb_final))
    } else {
      return(raster::stack(rb_final))
    }
  } else {
    cli::cli_abort("returnclass must be one of 'terra' or 'raster'")
  }



}
