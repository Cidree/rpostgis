# rpostgis tests
# These are most basic tests to ensure all functions are working
cwd <- getwd()

tryCatch({
  library(rpostgis)
    # setwd("./rpostgis")
  ae.all <- list()
  for (i in 1:2) {
    if (i == 1) {
      # RPostgreSQL is attached with rpostgis
      ae.all[[i]] <- "PostgreSQL Tests"
      drv <- dbDriver("PostgreSQL")
      message("testing RPostgreSQL driver...")
      Sys.sleep(3)
    }
    if (i == 2) {
      # RPostgres is suggests; not attached with rpostgis
      library(RPostgres)
      ae.all[[i]] <- "RPostgres Tests"
      drv <- Postgres()
      message("testing RPostgres driver...")
      Sys.sleep(3)
    }
    library(sp)
    library(raster)
    library(sf)
    library(tidyterra)
    library(terra)
    data("meuse")
    # cred <- scan("~/.pgpass_rpostgis", what = "character")
    conn <- dbConnect(
      RPostgres::Postgres(),
      host     = "localhost",
      dbname   = "Course_Udemy", 
      user     = "postgres", 
      password = keyring::key_get("postgres","postgres")
    )
  
    
    # general arguments
    new_table <- c("rpostgis", "db_test")
    ex_table <- c("public", "baea_nests")
    
    print(system.time({
        # general
        pgPostGIS(conn, topology = TRUE, tiger = TRUE, sfcgal = TRUE)
        pgListGeom(conn)
        pgListGeom(conn, geog = TRUE)
        
        # retrieval functions
        pts     <- pgGetGeom(conn, ex_table, geom = "geom", boundary = c(-105, 39, -104, 40))
        pts2    <- pgGetGeom(conn, ex_table, geom = "geom", cols = c("gid","status","nest_id"),
                          clauses = "where status = 'ACTIVE NEST' order by nest_id limit 30;")
        lin     <- pgGetGeom(conn, c("public", "linear_projects"))
        poly    <- pgGetGeom(conn, c("public", "buowl_habitat"), 
                             clauses = "order by habitat_id", boundary = pts2)
        bnd     <- pgGetBoundary(conn, ex_table)
        rast    <- pgGetRast(conn, c("public", "terra_p2"))
        rastclp <- pgGetRast(conn, c("public", "terra_p2"), 
                             boundary = c(635000, 4710000, 637000, 4715000))
        
        # view with pgGetGeom
        pts <- pgGetGeom(conn, c("public", "view_raptor_nests"), gid = "nest_id", cols = FALSE)
        # query version pgGetGeomQ
        pts <- pgGetGeom(conn, query = "SELECT r.gid as id, ST_Buffer(r.geom, 0.001) as geom 
                            FROM
                              raptor_nests r,
                              buowl_habitat b
                            WHERE 
                              ST_Intersects(r.geom, b.geom) AND r.recentstat = 'ACTIVE NEST';")
        pts <- pgGetGeom(conn, name = ex_table)
        # test ROLLBACK (fail)
        try(pts2 <- pgGetGeom(conn, query = "SELECT st_collect(geom) as geom FROM view_raptor_nests;",
                           cols = FALSE, geom = "geom"))
        message("THIS IS SUPPOSED BE AN ERROR!")
        
        pts2 <- pgGetGeom(conn, query = "SELECT recentstat, ST_Collect(geom) as geom 
                          FROM raptor_nests GROUP BY recentstat;",
                           cols = TRUE)
        
        dbDrop(conn, name = c("public","iris_data"))
        dbWriteDataFrame(conn, name = c("public", "iris_data"), iris)
        # geography columns
        pgListGeom(conn, geog = TRUE)
        rnest <- c("raptor_nests")
        pgeog  <- pgGetGeom(conn, rnest, clauses = "limit 100")
        pgeog2 <- pgGetGeom(conn, rnest, boundary = pts)
        pgeog2 <- pgGetBoundary(conn, rnest, clauses = "where recentstat = 'FLEDGED NEST'")
        rm(pgeog2, pgeom)
        
        # get SRIDs
        pgSRID(conn, crs = st_crs(bnd))
        pgSRID(conn, crs = st_crs(crs(rast)))
        
        crsf <- paste0("+:fakeCRS", sample(1:10000, 1))
        lin <- st_set_crs(lin, crsf)
        pgSRID(conn, crs = st_crs(lin), create.srid = TRUE)
        
        # send data to database
        dbSchema(conn, new_table[1])
        dbDrop(conn, new_table, type = "table", ifexists = TRUE)
        # matview with pgGetGeom
        dbSendQuery(conn, paste0("CREATE MATERIALIZED VIEW ",paste(new_table, collapse = "."),
                                  " AS (SELECT * FROM public.raptor_nests LIMIT 100);"))
        matview <- pgGetGeom(conn, new_table)
        dbDrop(conn, new_table, type = "materialized view")
        
        # basic insert
        pgInsert(conn, new_table, pts)
        # different CRS, insert geog
        pts3035 <- st_transform(pts, crs(rast))
        pgInsert(conn, c(new_table[1], "pts3035"), pts3035)
        pgInsert(conn, c(new_table[1], "ptsgeo"), pts3035, overwrite = TRUE, geog = TRUE)
        pgInsert(conn, c(new_table[1], "linegeog"), pgeog, geom = "geog", overwrite = T)
        pgeogline2 <- pgGetGeom(conn, c(new_table[1], "linegeog"), geom = "geog")
        ae.all[[i]] <- c(ae.all[[i]], paste(all.equal(pgeog, pgeogline2), collapse = ","))
        rm(pts3035, pgeog, pgeogline2)
        # pgi mode
        pgInsert(conn, c("rpostgis", "db_test2"), pts)
        pgi <- pgInsert(conn, new_table, pts, return.pgi = TRUE)
        print(pgi)
        pgi$in.table <- c("rpostgis", "db_test2")  # change insert table in object
        pgInsert(conn, data.obj = pgi)
        rm(pgi)
        
        pgListGeom(conn)
        
        # terra raster
        data(volcano2)
        y <- rast(volcano2)
        pgWriteRast(conn, c("rpostgis", "from_terra"), y, overwrite = TRUE)
        
        y2 <- pgGetRast(conn, c("rpostgis", "from_terra"))
        all.equal(class(y), class(y2))

        x <- raster(y)
        pgWriteRast(conn, c("rpostgis", "from_raster"), x, blocks = c(1,2), overwrite = TRUE)
        
        x2 <- pgGetRast(conn, c("rpostgis", "from_raster"))
        x2 <- raster(x2)
        all.equal(class(x), class(x2))
        
        rm(x,x2,y,y2, volcano2)
      
        # sp raster (retire in future)
        data(meuse.grid) # only the non-missing valued cells
        coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame
        proj4string(meuse.grid) <- CRS("+init=epsg:28992")
        gridded(meuse.grid) <- TRUE # promote to SpatialPixelsDataFrame
        
        # test with SpatialPixelsDataFrame
        y <- meuse.grid
        pgWriteRast(conn, c("rpostgis", "from_spxdf"), y, overwrite = TRUE)
        
        y2 <- pgGetRast(conn, c("rpostgis", "from_spxdf"))
        
        x <- as(meuse.grid, "SpatialGridDataFrame")
        pgWriteRast(conn, c("rpostgis", "from_sgdf"), x, overwrite = TRUE)
        
        x2 <- pgGetRast(conn, c("rpostgis", "from_sgdf"))
        
        rm(x,x2,y,y2, meuse.grid)
        # end SP-type rasters
        
        # raster (retire in future)
        r <- raster(nrows = 18, ncols = 36, xmn = -180, xmx = 180, 
            ymn = -90, ymx = 90, vals = 1)
        pgWriteRast(conn, c("rpostgis", "test_rast"), raster = r, blocks = c(2,3.5),
            bit.depth = "2BUI", overwrite = TRUE)
        rast2 <- pgGetRast(conn,  c("rpostgis", "test_rast"))
        print(all.equal(r, raster(rast2)))
        
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(r, raster(rast2)),collapse = ","))
        
        # try terra 
        load("tests/testthat/test_data/roe_raster.rda")
        roe_raster <- list(corine06 = rast(roe_raster$corine06), srtm_dem = rast(roe_raster$srtm_dem))
        rast <- roe_raster$corine06
        pgWriteRast(conn, c("rpostgis", "clc"), raster = rast, blocks = c(18,10), 
            overwrite = TRUE)
        
        rast2 <- pgGetRast(conn, c("rpostgis", "clc"), bands = c(1))
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(rast,rast2),collapse = ","))
        rast2 <- pgGetRast(conn, c("rpostgis", "clc"), clauses = "where rid < 10") # clauses test
        
        rast <- roe_raster$srtm_dem
        pgWriteRast(conn, c("rpostgis", "srtm"), raster = rast, blocks = c(14,2),
            overwrite = TRUE)
        
        rast2 <- pgGetRast(conn, c("rpostgis", "srtm"), bands = c(1))
        ae.all[[i]] <- c(ae.all[[i]], paste(all.equal(rast,rast2), collapse = ","))
        
        # write terra rast
        ls <- list.files("tests/testthat/test_data/rstack/", 
            full.names = TRUE)
        ls <- ls[!grepl(pattern = ".prj", ls)][1:5]
        
        rast <- rast(ls)
        pgWriteRast(conn, c("rpostgis", "uw"), rast, 
            overwrite = TRUE)
        
        # append *2 terra rast
        rast.app <- rast*2
        names(rast.app) <- paste(names(rast.app), "x2")
        pgWriteRast(conn, c("rpostgis", "uw"), rast.app, 
                    append = TRUE)
        bla <- pgGetRast(conn, c("rpostgis", "uw"), bands = c(2:4), boundary = c(-88, 32, -86, 34), 
                         clauses = "where band_names = '{{apr_prec x2},{apr_temp x2},{aug_prec x2},{aug_temp x2},{dec_prec x2}}'")
        
        # try raster brick (retire in future)
        rast <- brick(rast)
        pgWriteRast(conn, c("rpostgis", "uw"), rast, blocks = c(1,3),
            overwrite = TRUE)
        bla <- pgGetRast(conn, c("rpostgis", "uw"), bands = T)
        
        rast2 <- pgGetRast(conn, c("rpostgis", "uw"), bands = c(2:4), boundary = c(-88, 32, -86, 34))
        rast2 <- pgGetRast(conn, c("rpostgis", "uw"), bands = TRUE)
        
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(rast, brick(rast2)), collapse = ","))
        
        # List rasters
        pgListRast(conn)

        # drop table
        dbDrop(conn, new_table)
        
        # send data to database, no geom, with row.names only #############################
        dum.dat <- pts
        row.names(dum.dat) <- sample(row.names(dum.dat), size = length(row.names(dum.dat)), replace = FALSE)
        pgInsert(conn, new_table, dum.dat, row.names = TRUE, overwrite = TRUE)
        ae.all[[i]] <- c(ae.all[[i]], paste(all.equal(st_drop_geometry(dum.dat), dbReadDataFrame(conn, new_table)[,-8]), collapse = ","))
        rm(dum.dat)
        
        # df.geom test
        suppressWarnings(df <- dbGetQuery(conn, paste0("SELECT * FROM ", paste(ex_table, collapse = "."), ";")))
        pgInsert(conn, new_table, data.obj = df, df.geom = c("geom","(POINT,4326)"), overwrite = TRUE)
        pgInsert(conn, new_table, data.obj = df, df.geom = "geom", overwrite = TRUE)

        # send data to database with geom, overwrite, with new ID num
        pgInsert(conn, new_table, pts, overwrite = TRUE, new.id = "gid_r")
        
        # test general db functions
        dbComment(conn, new_table, comment = "test table for rpostgis.")
        dbAddKey(conn, new_table, colname = c("tgid", "nest_id"), type = "primary") # multi-column primary
        
        # send data to database with geom, overwrite, with new ID num
        pgInsert(conn, new_table, pts, overwrite = TRUE, new.id = "gid_r")
        dbAddKey(conn, new_table, colname = c("gid_r"))

        # dbColumn(conn, new_table, "date2", coltype = "character varying")
        # dbExecute(conn, "UPDATE rpostgis.db_test SET date2 = time;")
        # 
        # dbAsDate(conn, new_table, "date2", tz = "America/Los_Angeles")
        # dbTableInfo(conn, new_table, allinfo = TRUE)
        # 
        # dbGetQuery(conn, "SELECT time, date2 FROM rpostgis.db_test LIMIT 1;")
        # # date2 is 3 hrs later, since displaying in local time (EST)
        # 
        # dbIndex(conn, new_table, colname = "date2")
        dbIndex(conn, new_table, colname = "geom", method = "gist")
        # multi-colum index
        dbIndex(conn, new_table, colname = c("gid_r", "nest_id"), method = "btree")
        
        dbVacuum(conn, new_table, full = TRUE)
        
        # insert only geom
        dbExecute(conn, "ALTER TABLE rpostgis.db_test DROP COLUMN gid_r;")
        pts.sponly <- st_geometry(pts)
        pgInsert(conn, new_table, pts.sponly)
        
        # df mode only geom
        pgInsert(conn, new_table, pts.sponly, df.mode = TRUE, 
            overwrite = TRUE)
        
        pts.sponly2 <- pgGetGeom(conn, new_table)
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(pts.sponly, st_geometry(pts.sponly2)), collapse = ","))
        
        # pgMakePts
        pgInsert(conn, c("rpostgis", "meuse"), meuse, overwrite = T)
        pgMakePts(conn, c("rpostgis", "meuse"), colname = "geom_make", 
            srid = 26917, index = TRUE)
        
        # pgMakeStp
        alb <- DBI::dbGetQuery(conn, "SELECT * FROM public.linear_projects;")
        pgInsert(conn, c("rpostgis", "alba"), alb, new.id = "gid_R")
        pgMakeStp(conn, c("rpostgis", "alba"), colname = "geom_stp", 
                  srid = 26917, index = TRUE)
        
        # data.frame mode write/read
        load("tests/testthat/test_data/roe_vector_geom.rda")
        
        p1 <- roe_vector_geom$meteo_stations
        p1 <- as(p1, "sf")
        row.names(p1) <- 7:2 # mess with row.names
        pgInsert(conn, c("rpostgis", "pts"), data.obj = p1, df.mode = TRUE, 
                 overwrite = TRUE)
        p2 <- pgGetGeom(conn, c("rpostgis", "pts"), gid = "station_id") # works but ignores df.mode
        p2 <- pgGetGeom(conn, c("rpostgis", "pts"))
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(p1, p2), collapse = ","))
        
        p1 <- roe_vector_geom$roads
        p1$bla <- as.numeric(row.names(p1)) + 100
        p1 <- as(p1, "sf")
        pgInsert(conn, c("rpostgis", "lin"), data.obj = p1, df.mode = TRUE, 
            overwrite = TRUE)
        p2 <- pgGetGeom(conn, c("rpostgis", "lin"), gid = "bla") # works but ignores df.mode
        p2 <- pgGetGeom(conn, c("rpostgis", "lin"))
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(p1, p2), collapse = ","))
        
        p1 <- roe_vector_geom$adm_boundaries
        p1$bla <- as.numeric(row.names(p1)) + 100
        p1 <- as(p1, "sf")
        pgInsert(conn, c("rpostgis", "poly"), data.obj = p1, 
            df.mode = TRUE, overwrite = TRUE)
        p2 <- pgGetGeom(conn, c("rpostgis", "poly"), gid = "bla") # works but ignores df.mode
        p2 <- pgGetGeom(conn, c("rpostgis", "poly"))  # ordering...
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(p1, p2), collapse = ","))
        
        load("tests/testthat/test_data/roe_gps_data.rda")
        d <- rbind(roe_gps_data$GSM01511[, 1:14], roe_gps_data$GSM01508[, 
            1:14])
        d$acquisition_time <- as.POSIXct(paste0(d$utc_date, " ", 
            d$utc_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
        d$posixlt <- as.POSIXlt(d$acquisition_time, tz = "America/New_York")
        d$nav <- as.ordered(d$nav)
        
        dbWriteDataFrame(conn, c("rpostgis", "d"), d, overwrite = TRUE)
        d2 <- dbReadDataFrame(conn, c("rpostgis", "d"))
        ae.all[[i]] <- c(ae.all[[i]],paste(all.equal(d, d2), collapse = ","))
        # end data frame mode section
        
        # drop schema
        dbDrop(conn, new_table[1], type = "schema", cascade = TRUE)
        
        dbDisconnect(conn)
        
        rm(list = ls()[!ls() %in% c("ae.all","i","cwd")])
    }))
  }
    print("ALL GOOD!!!")
}, error = function(x) {
    print("errors...")
    print(x)
})

ae.all
setwd(cwd)
rm(cwd)
