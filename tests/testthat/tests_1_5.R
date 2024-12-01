
# 1. Connect to db --------------------------------------------------------

conn <- dbConnect(
  RPostgres::Postgres(),
  host     = "localhost",
  dbname   = "rpostgis",
  user     = "postgres",
  password = keyring::key_get("rpostgis","postgres")
)

# 2. Create schema --------------------------------------------------------

## Create schema
try(dbDrop(conn, "pg", "schema", cascade = TRUE), silent = TRUE)
dbSchema(conn, name = "pg")

## Create postgis extension
pgPostGIS(conn, topology = TRUE, tiger = TRUE, sfcgal = TRUE, raster = TRUE)
pgPostGIS(conn, sf = TRUE, exec = FALSE)
pgPostGIS(conn, display = TRUE)

## List elements
pgListGeom(conn)

# 3. Data frames ----------------------------------------------------------

# 3.1. Normal export ------------------------------------------------------

## Write data frame
my_data <- mtcars
my_data$cyl <- as.factor(my_data$cyl)
dbWriteDataFrame(conn, c("pg", "first_mtcars"), my_data)

## Read it back into R
my_data_2 <- dbReadDataFrame(conn, c("pg", "first_mtcars"))

## Test equal
test_that("Export/Import DF works", {
  expect_equal(my_data, my_data_2)
})

# 3.2. Overwrite ----------------------------------------------------------

## Write data frame
dbWriteDataFrame(conn, c("pg", "first_mtcars"), my_data, overwrite = TRUE)

## Read it back into R
my_data_2 <- dbReadDataFrame(conn, c("pg", "first_mtcars"))

## Test equal
test_that("Export/Import DF works", {
  expect_equal(my_data, my_data_2)
})

# 3.3. Using df mode ------------------------------------------------------

## Write data frame
pgWriteGeom(conn, c("pg", "second_mtcars"), my_data, df.mode = TRUE)

## Read it back into R
my_data_3 <- dbReadDataFrame(conn, c("pg", "second_mtcars"))

## Test equal
test_that("Export DF with pgWriteGeom works", {
  expect_equal(my_data, my_data_3)
})

# 3.4. Only defs ----------------------------------------------------------

## Write data frame
my_data_4 <- dbWriteDataFrame(conn, c("pg", "third_mtcars"), my_data, only.defs = TRUE)

## Test equal
test_that("Only defs works", {
  expect_equal(my_data, my_data_4)
})

# 4. Vectorial data -------------------------------------------------------

# 4.1. Normal export ------------------------------------------------------

## Read vectorial data (point)
baea_nests <- readRDS("test_data_1_5/baea_nests.rds")

## Write vectorial data
pgWriteGeom(conn, c("pg", "first_baea_nests"), baea_nests)

## Read it back into R
baea_nests_2 <- pgGetGeom(conn, c("pg", "first_baea_nests"))

## Test equal
test_that("Export/Import point data works", {
  expect_equal(baea_nests, baea_nests_2, tolerance = 0.0001)
})

# ## Without CRS
# baea_nests_nocrs <- baea_nests
# sf::st_crs(baea_nests_nocrs) <- NA
# pgWriteGeom(conn, c("pg", "second_baea_nests"), baea_nests_nocrs)
# baea_nests_nocrs_2 <- pgGetGeom(conn, c("pg", "second_baea_nests"))
#
# test_that("Export/Import point data works", {
#   expect_equal(baea_nests_nocrs, baea_nests_nocrs_2, tolerance = 0.0001)
# })

## Write as geography
pgWriteGeom(conn, c("pg", "third_baea_nests"), baea_nests, geog = TRUE, overwrite = TRUE)

# 4.2. Terra format -------------------------------------------------------

## Read SpatVector
baea_nests_terra <- pgGetGeom(conn, c("pg", "first_baea_nests"), returnclass = "terra")

## Test class
test_that("It is terra object", {
  expect_s4_class(baea_nests_terra, "SpatVector")
})

# 4.3. sp format ----------------------------------------------------------

## Read sp
baea_nests_sp <- pgGetGeom(conn, c("pg", "first_baea_nests"), returnclass = "sp")

## Test class
test_that("It is sp object", {
  expect_s4_class(baea_nests_sp, "SpatialPointsDataFrame")
})

# 4.4. Other geometries ---------------------------------------------------

## setwd("tests/testthat")
## Read vectorial data (maybe need to use st_make_valid)
linear_projects      <- readRDS("test_data_1_5/linear_projects.rds")
roads                <- readRDS("test_data_1_5/roads.rds")
linear_dissolve_type <- readRDS("test_data_1_5/linear_dissolve_type.rds")
buowl_habitat        <- readRDS("test_data_1_5/buowl_habitat.rds")

## Write vectorial data
pgWriteGeom(conn, c("pg", "first_linear_projects"), linear_projects, row.names = TRUE)
pgWriteGeom(conn, c("pg", "first_roads"), roads, row.names = TRUE)
pgWriteGeom(conn, c("pg", "first_linear_dissolve_type"), linear_dissolve_type)
pgWriteGeom(conn, c("pg", "first_buowl_habitat"), buowl_habitat, new.id = "test_id", row.names = TRUE)

## Read vectorial data
## Issue -> pgWriteGeom does not export some objects in the same order (multipolygon, multilinestring)
## as the R object (however, they are the same object but not sorted)

linear_projects_2      <- pgGetGeom(conn, c("pg", "first_linear_projects")) ## passed (multilinestring) (not sorted)
roads_2                <- pgGetGeom(conn, c("pg", "first_roads"))  ## passed (multilinestring) (not sorted)
linear_dissolve_type_2 <- pgGetGeom(conn, c("pg", "first_linear_dissolve_type")) ## passed (geometry)
buowl_habitat_2        <- pgGetGeom(conn, c("pg", "first_buowl_habitat"),
                                    gid = ".R_rownames")   ## passed (multipolygon) (not sorted)

## Test new column ID exists
test_that("test_id exists", {
  expect_true("test_id" %in% names(buowl_habitat_2))
})

## Test that different geometries are exported/imported correctly
buowl_habitat_2 <- buowl_habitat_2[,-which(names(buowl_habitat_2) == "test_id")]
test_that("Geometries are imported/exported correctly", {
  expect_equal(dplyr::arrange(linear_projects, postgis_fi),
               dplyr::arrange(linear_projects_2, postgis_fi))
  expect_equal(linear_dissolve_type,
               linear_dissolve_type_2)
  expect_equal(dplyr::arrange(buowl_habitat, postgis_fi),
               dplyr::arrange(buowl_habitat_2, postgis_fi))
  expect_equal(dplyr::arrange(roads, denominaci),
               dplyr::arrange(roads_2, denominaci))
})

# 4.5. Data frame mode ----------------------------------------------------

## Write vectorial data
pgWriteGeom(conn, c("pg", "second_linear_projects"), linear_projects, df.mode = TRUE)
## Read vectorial data
linear_projects_3 <- pgGetGeom(conn, c("pg", "second_linear_projects"))

## Test equal
test_that("Export/Import DF with pgWriteGeom works", {
  expect_equal(dplyr::arrange(linear_projects, postgis_fi),
               dplyr::arrange(linear_projects_3, postgis_fi))
})


# 4.6. Alter names --------------------------------------------------------

## Edit data
names(roads)[1] <- "deNomInazi.rOAd"

## Write vectorial data
pgWriteGeom(conn, c("pg", "first_roads"), roads, alter.names = TRUE, overwrite = TRUE)

## Read vectorial data
roads_3 <- pgGetGeom(conn, c("pg", "first_roads"))

## Check column name
test_that("Column name is altered", {
  expect_equal(names(roads_3)[1], "denominazi_road")
})

# 4.7. get geom boundary --------------------------------------------------

## Boundary for roads
bndry <- c(4230000, 4150000, 200000, 150000)

## Read vectorial data
roads_4 <- pgGetGeom(conn, c("pg", "first_roads"), boundary = bndry)

# 4.8 Query ---------------------------------------------------------------

## Query
buowl_query <- pgGetGeom(conn,
                         query = "SELECT *
                                     FROM pg.first_buowl_habitat
                                     WHERE recentstat = 'REMOVED'")

## Test query worked
test_that("Query works", {
  expect_equal(unique(buowl_query$recentstat), "REMOVED")
})

# 4.8. Export only df -----------------------------------------------------

## Write vectorial data
dbWriteDataFrame(conn, c("pg", "second_roads"), roads)
dbWriteDataFrame(conn, c("pg", "third_roads"), terra::vect(roads))

# 4.9. Insert function ----------------------------------------------------

## Convert to sp
roads_sp <- as(roads, "Spatial")

## Insert function
pgInsert(conn, c("pg", "fourth_roads"), roads_sp)
pgInsert(conn, c("pg", "fourth_roads"), roads_sp, overwrite = TRUE)
pgInsert(conn, c("pg", "fourth_roads"), roads_sp, df.mode = TRUE, overwrite = TRUE)

# 4.10. Insert into existing table ----------------------------------------

roads <- readRDS("test_data_1_5/roads.rds")

## Insert data twice
pgWriteGeom(conn, c("pg", "fifth_roads"), roads)
pgitest <- pgWriteGeom(conn, c("pg", "fifth_roads"), roads, return.pgi = TRUE)

## Get data
roads_5 <- pgGetGeom(conn,  c("pg", "fifth_roads"))

## Test equal
test_that("Geometry was imported twice", {
  expect_equal(nrow(roads_5), 2 * nrow(roads))
  expect_s3_class(pgitest, "pgi")
})

## Insert with pgi
pgWriteGeom(conn, c("pg", "sixth_roads"), pgitest)

# 4.11. Insert df geom ----------------------------------------------------

## Create data
roads_df <- data.frame(roads)
roads_df$geom <- sf::st_as_text(roads_df$geom)
pgWriteGeom(conn, c("pg", "seventh_roads"), roads_df, df.geom = "geom")
roads_6 <- pgGetGeom(conn,  c("pg", "seventh_roads"))

# 5. Raster ---------------------------------------------------------------

# 5.1. Normal import/export -----------------------------------------------

## Read raster
dem <- rast("test_data_1_5/dem_esp.tif")

## Write raster
pgWriteRast(conn, c("pg", "first_dem"), dem, progress = FALSE)
pgWriteRast(conn, c("pg", "first_dem"), dem, overwrite = TRUE, bit.depth = "64BF")

## Read raster back into R
dem_2 <- pgGetRast(conn, c("pg", "first_dem"), rast = "rast")

## Test equal
test_that("Extent and CRS are the same after export/import", {
  expect_equal(terra::crs(dem), terra::crs(dem_2))
  expect_equal(as.vector(terra::ext(dem)),
               as.vector(terra::ext(dem_2)),
               tolerance = 0.001)
})

# 5.2. Raster class -------------------------------------------------------

## Read raster
dem_raster <- pgGetRast(conn, c("pg", "first_dem"), returnclass = "raster")

test_that("Return raster class works", {
  expect_true(class(dem_raster) == "RasterLayer")
})

# 5.3. Multi-band raster --------------------------------------------------

## Read stack
terrain_sr <- rast("test_data_1_5/terrain_stack.tif")

## Write stack
pgWriteRast(conn, c("pg", "first_terrain"), terrain_sr, blocks = c(5, 5))

## Read stack back into R
terrain_sr_2 <- pgGetRast(conn, c("pg", "first_terrain"), bands = TRUE)

## Test equal
test_that("Extent and CRS are the same after export/import", {
  expect_equal(terra::crs(terrain_sr), terra::crs(terrain_sr_2))
  expect_equal(as.vector(terra::ext(terrain_sr)),
               as.vector(terra::ext(terrain_sr_2)),
               tolerance = 0.001)
  expect_equal(terra::nlyr(terrain_sr), terra::nlyr(terrain_sr_2))
})

# 5.4. Get only some bands ------------------------------------------------

## Read stack
terrain_sr_3 <- pgGetRast(conn,
                          c("pg", "first_terrain"),
                          bands = c(1, 3))

## Test equal
test_that("Extent and CRS are the same after export/import", {
  expect_equal(terra::crs(terrain_sr[[c(1,3)]]),
               terra::crs(terrain_sr_3))

  expect_equal(as.vector(terra::ext(terrain_sr[[c(1,3)]])),
               as.vector(terra::ext(terrain_sr_3)),

               tolerance = 0.001)
  expect_equal(terra::nlyr(terrain_sr[[c(1,3)]]),
               terra::nlyr(terrain_sr_3))
})

# 5.5. Use boundary -------------------------------------------------------

## Boundary
myboundary <- c(42, 40, 3, -2)

## Read raster
dem_boundary <- pgGetRast(conn, c("pg", "first_dem"), boundary = myboundary)

## Test equal
test_that("Extent is correct", {
  expect_equal(as.numeric(as.vector(terra::ext(dem_boundary))),
               c(myboundary[4],myboundary[3],myboundary[2],myboundary[1]))
})

# 6. Get Boundary ---------------------------------------------------------

# 6.1. Vectorial data -----------------------------------------------------

## Get boundary
roads_boundary <- pgGetBoundary(conn, c("pg", "first_roads"))

## Test equal
## Direction of polygon may be different
test_that("Boundary is the same", {
  expect_equal(sf::st_bbox(roads_boundary), sf::st_bbox(roads))
})

# 6.2. Raster data --------------------------------------------------------

## Get boundary
dem_boundary <- pgGetBoundary(conn, c("pg", "first_dem"), geom = "rast")
## Test equal
test_that("Boundary is the same", {
  expect_equal(as.vector(sf::st_bbox(dem_boundary)),
               as.vector(sf::st_bbox(terra::ext(dem))))
})

# 6.3. Other classes ------------------------------------------------------

## sp
roads_boundary_sp    <- pgGetBoundary(conn, c("pg", "first_roads"), returnclass = "sp")
roads_boundary_terra <- pgGetBoundary(conn, c("pg", "first_roads"), returnclass = "terra")

test_that("Classes are correct", {
  expect_s4_class(roads_boundary_sp, "SpatialPolygons")
  expect_s4_class(roads_boundary_terra, "SpatVector")
})

# 6.4. Clauses ------------------------------------------------------------

roads_boundary_cl <- pgGetBoundary(conn, c("pg", "first_roads"),
                                   clauses = "WHERE tipo_via = 'Vereda'")

# 7. List geometries ------------------------------------------------------

pgListGeom(conn)
pgListRast(conn)

# 8. pgSRID ---------------------------------------------------------------

## Existing CRS
crs <- sf::st_crs("+proj=longlat")
srid_1 <- pgSRID(conn, crs)

test_that("Retrieving existing CRS works", {
  expect_equal(srid_1[1], 4326)
})

## Non-existing CRS
crs2 <- sf::st_crs(paste("+proj=stere", "+lat_0=52.15616055555555 +lon_0=5.38763888888889",
                          "+k=0.999908 +x_0=155000 +y_0=463000", "+ellps=bessel",
                          "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812",
                          "+units=m"))
srid_2 <- pgSRID(conn, crs2, create.srid = TRUE)

test_that("Creating non-existing CRS works", {
  expect_equal(srid_2, 880001)
})

## NA and non-existing CRS
test_that("Non-existing CRS throws an error", {
  expect_error(pgSRID(conn, sf::st_crs(NA)))
})

test_that("New SRID is created", {
  expect_equal(
    pgSRID(conn, sf::st_crs("ESRI:37220"), create.srid = TRUE, new.srid = 37220),
    37220
  )
})

## Existing ESRI CRS
test_that("ESRI CRS works", {
  expect_equal(pgSRID(conn, sf::st_crs("ESRI:102825")), 102825)
})

# 9. Wrappers -------------------------------------------------------------


# 9.1. Drop a table -------------------------------------------------------

dbDrop(conn, c("pg", "first_mtcars"), "table")

test_that("Drop table works", {
  expect_error(dbReadDataFrame(conn, c("pg", "first_mtcars")))
})

# 9.2. Create a schema ----------------------------------------------------

dbSchema(conn, "pgtest")
dbWriteDataFrame(conn, c("pgtest", "mtcars"), mtcars)

test_that("Creating schema works", {
  expect_true(dbExistsTable(conn, c("pgtest", "mtcars")))
})

# 9.3. Drop a schema ------------------------------------------------------

dbDrop(conn, "pgtest", "schema", cascade = TRUE)

test_that("Deleting schema works", {
  expect_false(dbExistsTable(conn, c("pgtest", "mtcars")))
})

# 9.4. Others -------------------------------------------------------------

dbTableInfo(conn, c("pg", "first_baea_nests"))

test_that("Function works", {
  expect_true(dbVacuum(conn, c("pg", "first_baea_nests"), full = TRUE))
})

# 9.5 Add/rm column -------------------------------------------------------

# Remove column
dbColumn(conn, c("pg", "first_baea_nests"), "nest_id", "drop")

# Add column
dbColumn(conn, c("pg", "first_baea_nests"), "nest_id", "add")

# 9.6. Index --------------------------------------------------------------

dbIndex(conn, c("pg", "first_baea_nests"),
        colname = "geom",
        method = "gist")

dbIndex(conn, c("pg", "first_baea_nests"), colname = "status")

# 9.7 Points --------------------------------------------------------------

# Baea nests coordinates as columns
baea_nests_tbl <- sf::st_drop_geometry(baea_nests)

# Insert without geometry
dbWriteDataFrame(conn, c("pg", "fourth_baea_nests"), baea_nests_tbl, overwrite = TRUE)

# Create points column
test_that("Make pts works", {
  v <- pgMakePts(conn, name = c("pg", "fourth_baea_nests"), colname = "pts_geom",
            x = "long_x_dd", y = "lat_y_dd", srid = 4326, exec = TRUE)
  expect_true(v)
})

# Read third baea nest
baea_nests_3 <- pgGetGeom(conn, c("pg", "fourth_baea_nests"), geom = "pts_geom")

# Test equal
test_that("Creating point column works", {
  expect_equal(baea_nests, baea_nests_3, tolerance = .01)
})









