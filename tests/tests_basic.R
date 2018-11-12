# rpostgis tests
# These are most basic tests to ensure all functions are working
cwd <- getwd()

tryCatch({
    # setwd("./rpostgis")
    library(rpostgis)
    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    library(sp)
    library(raster)
    data("meuse")
    cred <- scan("~/.pgpass_rpostgis", what = "character")
    conn <- dbConnect(drv, host = cred[1], dbname = cred[2], 
        user = cred[3], password = cred[4])
    conn2 <- dbConnect(drv, host = cred[1], dbname = cred[5], 
        user = cred[3], password = cred[4])
    
    # general arguments
    new_table <- c("rpostgis", "db_test")
    ex_table <- c("example_data", "relocations_plus")
    ae.all <- NULL
    
    print(system.time({
        # general
        pgPostGIS(conn, topology = TRUE, tiger = TRUE, sfcgal = TRUE)
        pgListGeom(conn)
        pgListGeom(conn,geog=TRUE)
        
        # retrieval functions
        pts <- pgGetGeom(conn, ex_table, geom = "geom", boundary = c(30,25,-80,-100))
        pts2 <- pgGetGeom(conn, ex_table, geom = "geom", other.cols = c("gid","dummy","burst"),
                          clauses = "where id = 'continental' order by time limit 100;")
        lin <- pgGetGeom(conn2, c("env_data", "roads"))
        poly <- pgGetGeom(conn2, c("env_data", "adm_boundaries"), 
            clauses = "order by nome_com", boundary = lin)
        bnd <- pgGetBoundary(conn, ex_table)
        rast <- pgGetRast(conn2, c("env_data", "corine_land_cover"))
        rastclp <- pgGetRast(conn2, c("env_data", "srtm_dem"), 
            boundary = lin)
        # view with pgGetGeom
        poly <- pgGetGeom(conn2, c("analysis", "view_convex_hulls"), gid = "animals_id", other.cols = FALSE)
        # query version pgGetGeomQ
        poly <- pgGetGeom(conn2, query = "SELECT r.gid as id, ST_Buffer(r.geom, 0.001) as geom 
                            FROM
                              env_data.roads r,
                              env_data.adm_boundaries b
                            WHERE 
                              ST_Intersects(r.geom, b.geom) AND nome_com = 'Trento';")
        poly <- pgGetGeom(conn2, name = c("env_data","test"), 
                          query = "SELECT r.gid as id, ST_Buffer(r.geom, 0.001) as geom 
                            FROM
                              env_data.roads r,
                              env_data.adm_boundaries b
                            WHERE 
                              ST_Intersects(r.geom, b.geom) AND nome_com = 'Trento'", boundary = lin[1,])
        poly <- pgGetGeom(conn2, name = c("env_data","test"))
        # test ROLLBACK (fail)
        try(pts2 <- pgGetGeom(conn2, query = "SELECT st_collect(geom) as geom FROM env_data.meteo_stations;",
                           other.cols = FALSE, geom = "geo"))
        message("THIS IS SUPPOSED BE AN ERROR!")
        
        pts2 <- pgGetGeom(conn2, query = "SELECT 1 as id, st_collect(geom) as geom FROM env_data.meteo_stations;",
                           other.cols = TRUE)
        dbDrop(conn2, name = c("env_data","test"), type = "view")
        # geography columns
        pgListGeom(conn, geog = TRUE)
        pgeog <- pgGetGeom(conn, c("example_data","steps"), geom = "step_geog", clauses = "limit 500")
        pgeog2 <- pgGetGeom(conn, c("example_data", "continental"), geom = "geog", boundary = pts)
        pgeog2 <- pgGetBoundary(conn, c("example_data", "continental"), geom = "geog", clauses = "where id = 'continental'")
        pgeog2 <- pgGetBoundary(conn, c("example_data", "continental"), geom = "geog")
        pgeom <- pgGetBoundary(conn, c("example_data", "gps_data_animals"), geom = "ge'om", clauses = "where roads_dist < 100")
        rm(pgeog2, pgeom)
        
        # get SRIDs
        pgSRID(conn, crs = bnd@proj4string)
        pgSRID(conn2, crs = rast@crs)
        
        crsf <- paste0("+:fakeCRS", sample(1:10000, 1))
        lin@proj4string <- CRS(crsf, doCheckCRSArgs = FALSE)
        pgSRID(conn2, crs = lin@proj4string, create.srid = TRUE)
        
        # send data to database
        dbSchema(conn, new_table[1])
        dbDrop(conn, new_table, type = "table", ifexists = TRUE)
        # matview with pgGetGeom
        dbSendQuery(conn, paste0("CREATE MATERIALIZED VIEW ",paste(new_table, collapse="."),
                                  " AS (SELECT * FROM example_data.relocations_plus LIMIT 100);"))
        matview<- pgGetGeom(conn, new_table)
        dbDrop(conn, new_table, type = "materialized view")
        
        # basic insert
        pgInsert(conn, new_table, pts)
        # different CRS, insert geog
        pts3035<-spTransform(pts, rast@crs)
        pgInsert(conn, c(new_table[1], "pts3035"), pts3035)
        pgInsert(conn, c(new_table[1], "ptsgeo"), pts3035, overwrite = TRUE, geog = TRUE)
        pgInsert(conn, c(new_table[1], "linegeog"), pgeog, geom = "geog")
        pgeogline2<-pgGetGeom(conn, c(new_table[1], "linegeog"), geom = "geog")
        ae.all <- c(ae.all,paste(all.equal(pgeog,pgeogline2), collapse = ","))
        rm(pts3035, pgeog, pgeogline2)
        # pgi mode
        pgInsert(conn, c("rpostgis", "db_test2"), pts)
        pgi <- pgInsert(conn, new_table, pts, return.pgi = TRUE)
        print(pgi)
        pgi$in.table <- c("rpostgis", "db_test2")  # change insert table in object
        pgInsert(conn, data.obj = pgi)
        rm(pgi)
        
        pgListGeom(conn)
        
        # SP-type rasters
        data(meuse.grid)
        pg = meuse.grid[c("x", "y")]
        y <- SpatialPixels(SpatialPoints(pg, proj4string = CRS("+init=epsg:28992")))
        pgWriteRast(conn, c("rpostgis", "from_spx"), y, overwrite = TRUE)
        
        y2 <- pgGetRast(conn, c("rpostgis", "from_spx"))
        all.equal(class(y), class(y2))

        x = GridTopology(c(0,0), c(1,1), c(5,5))
        x = SpatialGrid(grid = x, proj4string = CRS("+init=epsg:28992"))
        pgWriteRast(conn, c("rpostgis", "from_sg"), x, blocks = c(1,2), overwrite = TRUE)
        
        x2 <- pgGetRast(conn, c("rpostgis", "from_sg"))
        all.equal(class(x), class(x2))
        
        rm(x,x2,y,y2,pg, meuse.grid)
      
        data(meuse.grid) # only the non-missing valued cells
        coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame
        proj4string(meuse.grid) <- CRS("+init=epsg:28992")
        gridded(meuse.grid) <- TRUE # promote to SpatialPixelsDataFrame
        
        # test with SpatialPixelsDataFrame
        y <- meuse.grid
        pgWriteRast(conn, c("rpostgis", "from_spxdf"), y, overwrite = TRUE)
        
        y2 <- pgGetRast(conn, c("rpostgis", "from_spxdf"))
        ae.all <- c(ae.all,paste(all.equal(class(y), class(y2)), collapse = ","))
        
        x <- as(meuse.grid, "SpatialGridDataFrame")
        pgWriteRast(conn, c("rpostgis", "from_sgdf"), x, overwrite = TRUE)
        
        x2 <- pgGetRast(conn, c("rpostgis", "from_sgdf"))
        ae.all <- c(ae.all,paste(all.equal(class(x), class(x2)), collapse = ","))
        
        rm(x,x2,y,y2, meuse.grid)
        # end SP-type rasters
        
        r <- raster(nrows = 18, ncols = 36, xmn = -180, xmx = 180, 
            ymn = -90, ymx = 90, vals = 1)
        pgWriteRast(conn, c("rpostgis", "test_rast"), raster = r, blocks = c(2,3.5),
            bit.depth = "2BUI", overwrite = TRUE)
        rast2 <- pgGetRast(conn,  c("rpostgis", "test_rast"))
        print(all.equal(r, rast2))
        
        ae.all <- c(ae.all,paste(all.equal(r, rast2),collapse = ","))
        
        load("tests/test_data/roe_raster.rda")
        rast <- roe_raster$corine06
        pgWriteRast(conn, c("rpostgis", "clc"), raster = rast, blocks = c(18,10), 
            overwrite = TRUE)
        
        rast2 <- pgGetRast(conn, c("rpostgis", "clc"), bands = c(1))
        ae.all <- c(ae.all,paste(all.equal(rast,rast2),collapse = ","))
        
        rast <- roe_raster$srtm_dem
        pgWriteRast(conn, c("rpostgis", "srtm"), raster = rast, blocks = c(14,2),
            overwrite = TRUE)
        
        rast2 <- pgGetRast(conn, c("rpostgis", "srtm"), bands = c(1))
        ae.all <- c(ae.all,paste(all.equal(rast,rast2), collapse = ","))
        
        # write rast stack/brick
        ls <- list.files("tests/test_data/rstack/", 
            full.names = TRUE)
        ls <- ls[!grepl(pattern = ".prj", ls)][1:5]
        
        rast <- stack(ls)
        system.time(pgWriteRast(conn, c("rpostgis", "uw"), rast, 
            overwrite = TRUE))
        rast <- brick(rast)
        system.time(pgWriteRast(conn, c("rpostgis", "uw"), rast, blocks = c(1,3),
            overwrite = TRUE))
        
        rast2 <- pgGetRast(conn, c("rpostgis", "uw"), bands = c(2:4), boundary = c(30, -5, -80, -95))
        rast2 <- pgGetRast(conn, c("rpostgis", "uw"), bands = TRUE)
        
        ae.all <- c(ae.all,paste(all.equal(rast,rast2), collapse = ","))
        
        # List rasters
        pgListRast(conn)

        # drop table
        dbDrop(conn, new_table)
        
        # send data to database, no geom, with row.names only
        dum.dat <- pts@data
        row.names(dum.dat) <- sample(row.names(dum.dat), size = length(row.names(dum.dat)), replace = FALSE)
        pgInsert(conn, new_table, dum.dat, row.names = TRUE, overwrite = TRUE)
        ae.all <- c(ae.all,paste(all.equal(dum.dat, dbReadDataFrame(conn, new_table)), collapse = ","))
        rm(dum.dat)
        
        # df.geom test
        suppressWarnings(df<-dbReadTable(conn, ex_table))
        pgInsert(conn, new_table, data.obj = df, df.geom = c("geom","(POINT,4326)"), overwrite = TRUE)
        pgInsert(conn, new_table, data.obj = df, df.geom = "geom", overwrite = TRUE)

        # send data to database with geom, overwrite, with new ID num
        pgInsert(conn, new_table, pts, overwrite = TRUE, new.id = "gid_r")
        
        # test general db functions
        dbComment(conn, new_table, comment = "test table for rpostgis.")
        dbAddKey(conn, new_table, colname = c("gid_r", "time")) # multi-column primary
        
        # send data to database with geom, overwrite, with new ID num
        pgInsert(conn, new_table, pts, overwrite = TRUE, new.id = "gid_r")
        dbAddKey(conn, new_table, colname = c("gid_r"))

        dbColumn(conn, new_table, "date2", coltype = "character varying")
        dbExecute(conn, "UPDATE rpostgis.db_test SET date2 = time;")
        
        dbAsDate(conn, new_table, "date2", tz = "America/Los_Angeles")
        dbTableInfo(conn, new_table, allinfo = TRUE)
        
        dbGetQuery(conn, "SELECT time, date2 FROM rpostgis.db_test LIMIT 1;")
        # date2 is 3 hrs later, since displaying in local time (EST)
        
        dbIndex(conn, new_table, colname = "date2")
        dbIndex(conn, new_table, colname = "geom", method = "gist")
        # multi-colum index
        dbIndex(conn, new_table, colname = c("gid_r", "time"), method = "btree")
        
        dbVacuum(conn, new_table, full = TRUE)
        
        # upsert
        pts <- pgGetGeom(conn, new_table)
        head(pts@data$dummy)
        pts@data$dummy <- 12345
        
        pgInsert(conn, new_table, pts, upsert.using = "gid_r")
        pts2 <- pgGetGeom(conn, new_table)
        
        ae.all <- c(ae.all,paste(all.equal(pts, pts2), collapse = ","))
        # only difference is date2 (tz difference)
        pts <- pts2
        rm(pts2)
        
        # insert only geom
        dbExecute(conn, "ALTER TABLE rpostgis.db_test DROP COLUMN gid_r;")
        pts.sponly <- SpatialPoints(pts, proj4string = pts@proj4string)
        pgInsert(conn, new_table, pts.sponly)
        
        # df mode only geom
        pgInsert(conn, new_table, pts.sponly, df.mode = TRUE, 
            overwrite = TRUE)
        
        pts.sponly2 <- pgGetGeom(conn, new_table)
        ae.all <- c(ae.all,paste(all.equal(pts.sponly,pts.sponly2), collapse = ","))
        
        # pgMakePts
        pgInsert(conn, c("rpostgis", "meuse"), meuse)
        pgMakePts(conn, c("rpostgis", "meuse"), colname = "geom_make", 
            srid = 26917, index = TRUE)
        
        # pgMakeStp
        alb <- DBI::dbGetQuery(conn, "SELECT * FROM example_data.albatross;")
        pgInsert(conn, c("rpostgis", "alba"), alb, new.id = "gid_R")
        pgMakeStp(conn, c("rpostgis", "alba"), colname = "geom_stp", 
            srid = 26917, index = TRUE)
        
        # data.frame mode write/read
        load("tests/test_data/roe_vector_geom.rda")
        
        p1 <- roe_vector_geom$meteo_stations
        row.names(p1) <- 7:2 # mess with row.names
        pgInsert(conn, c("rpostgis", "pts"), data.obj = p1, df.mode = TRUE, 
            overwrite = TRUE)
        p2 <- pgGetGeom(conn, c("rpostgis", "pts"), gid = "station_id") # works but ignores df.mode
        p2 <- pgGetGeom(conn, c("rpostgis", "pts"))
        ae.all <- c(ae.all,paste(all.equal(p1, p2), collapse = ","))
        
        p1 <- roe_vector_geom$roads
        p1$bla <- as.numeric(row.names(p1)) + 100
        pgInsert(conn, c("rpostgis", "lin"), data.obj = p1, df.mode = TRUE, 
            overwrite = TRUE)
        p2 <- pgGetGeom(conn, c("rpostgis", "lin"), gid = "bla") # works but ignores df.mode
        p2 <- pgGetGeom(conn, c("rpostgis", "lin"))
        ae.all <- c(ae.all,paste(all.equal(p1@data, p2@data), collapse = ","))
        
        p1 <- roe_vector_geom$adm_boundaries
        p1$bla <- as.numeric(row.names(p1)) + 100
        pgInsert(conn, c("rpostgis", "poly"), data.obj = p1, 
            df.mode = TRUE, overwrite = TRUE)
        p2 <- pgGetGeom(conn, c("rpostgis", "poly"), gid = "bla") # works but ignores df.mode
        p2 <- pgGetGeom(conn, c("rpostgis", "poly"))  # ordering...
        ae.all <- c(ae.all,paste(all.equal(p1@data, p2@data), collapse = ","))
        
        load("tests/test_data/roe_gps_data.rda")
        d <- rbind(roe_gps_data$GSM01511[, 1:14], roe_gps_data$GSM01508[, 
            1:14])
        d$acquisition_time <- as.POSIXct(paste0(d$utc_date, " ", 
            d$utc_time), format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
        d$posixlt <- as.POSIXlt(d$acquisition_time, tz = "America/New_York")
        d$nav <- as.ordered(d$nav)
        
        dbWriteDataFrame(conn, c("rpostgis", "d"), d)
        d2 <- dbReadDataFrame(conn, c("rpostgis", "d"))
        ae.all <- c(ae.all,paste(all.equal(d, d2), collapse = ","))
        # end data frame mode section
        
        # drop schema
        dbDrop(conn, new_table[1], type = "schema", cascade = TRUE)
        
        dbDisconnect(conn)
        dbDisconnect(conn2)
        
        rm(alb, d, d2, meuse, bnd, conn, conn2, cred, crsf, roe_gps_data, 
            roe_vector_geom, roe_raster, drv, ex_table, lin, ls, 
            new_table, p1, p2, poly, pts, pts.sponly, pts.sponly2, r, rast, 
            rastclp, matview, df, rast2)
    }))
    print("ALL GOOD!!!")
}, error = function(x) {
    print("errors...")
    print(x)
})
setwd(cwd)
rm(cwd)
