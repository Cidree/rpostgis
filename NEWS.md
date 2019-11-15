rpostgis 1.4.3
==============

OVERALL CHANGES

* The `RPostgres::Postgres()` driver can now be used as a connection. 
While `rpostgis` is functional with the driver, it should be considered
experimental and may not function as expected in all cases. The default 
`RPostgreSQL::PostgreSQL()` driver is recommended for most use cases.

NEW FEATURES

* `pgWriteRast`: Now allows insert with `append = TRUE` to an existing 
PostGIS raster table. Note that any existing constraints are dropped for
the insert, but re-created after insert (if `constraints = TRUE`). This 
allows more than one raster to be entered into a table. It is recommended
to alter the raster names (`names(raster) <-`) prior to import to ensure 
each raster in a table has a unique entry in the `band_names` column. 
See `pgGetRast`'s new `clauses` argument for importing subsets of a raster table.

* `pgGetRast`: A `clauses` argument allows to specify additional SQL 
(e.g. `clauses = "WHERE band_names = '{{landcover_2010}}'"`)
to select a specific subset of a raster table to import.

rpostgis 1.4.2
==============

BUG FIXES

* fixed bug affecting `pgWriteRast`, affecting rasters with irregular x/y resolutions.

* fixed bug affecting `pgInsert`, where single-part polygons with holes were 
counted as MultiPolygons, causing the geometry column to be defined as MultiPolygon. 
This did not affect import into the database, but caused an error when single-part
polygons (without holes) were attempted to be inserted into the existing table.

rpostgis 1.4.1
==============

DOCUMENTATION

* `rpostgis` now has a new home: https://mablab.org/rpostgis/

* Documentation prepared using `pkgdown`.

BUG FIXES

* Fixed bug affecting `dbReadDataFrame`, where datetimes were assigned the PostgreSQL time zone
instead of the system timezone. This resulted in incorrect time assignments when the PostgreSQL
time zone setting did not match the system's time zone setting. Tables in PostgreSQL written
by `dbWriteDataFrame` were not affected and have the correct time assigned.

rpostgis 1.4.0
==============

NEW FEATURES

* `dbAddKey` and `dbIndex`: support added for keys and indexes
  referencing multiple columns.

* `pgWriteRast`: new `blocks` argument to specify exact number of
  desired blocks for the new raster in PostGIS table. Default block
  size (`blocks = NULL`) will retain previous functionality, though
  the minimum block size (rows or columns) was increased to 100
  (previously 80).

BUG FIXES

* `pgWriteRast`: fixed alignment issue affecting high-precision
  (generally decimal-degree unit) based raster writing, where PostGIS
  was not able to add a same alignment constraint to all tiles in the
  newly-created raster.


rpostgis 1.3.0
==============

OVERALL CHANGES

* Example data removed, now part of extension package `rpostgisLT`

NEW FUNCTIONS

* `pgListRast` to list all raster columns in a PostGIS database

NEW FEATURES

* `pgGetRast`: re-written for faster reading of rasters from the
  database. Multi-band raster import is now supported (`band` argument
  changed to `bands`, and can be given a vector of integers, or TRUE
  to return all bands). Default is still to import the first band (1)
  of the raster.  Multi-band rasters imported as `RasterBrick`-class
  objects. When raster blocks have imperfect alignment, PostGIS
  function `ST_SnapToGrid` is applied to register the raster according
  to the upper left pixel of the full raster dataset.

* `pgGetRast`/`pgWriteRast`: now support sp-class `SpatialGrid*` and
  `SpatialPixels*` type objects. The class of the raster written in
  pgWriteRast is saved in a database column, and re-applied to the
  raster during import with `pgGetRast`.

* "Data frame mode" for Spatial* objects: now saves proj4string of
  Spatial* objects in data frame mode writing with `pgInsert`. It is
  then re-imported using `pgGetGeom`, if the saved proj4string and the
  database proj4string (defined by the column's SRID) are
  equivalent. Otherwise, the database proj4string is used.

* `pgGetRast`/`pgWriteRast`: original R proj4string is saved in the
  raster database table, and re-imported, if the saved proj4string and
  the database proj4string (defined by the column's SRID) are
  equivalent. Otherwise, the database proj4string is used.

* `pgGetGeom`: now includes `boundary` parameter, to spatially subset
  GEOMETRY/GEOGRPAHY objects to return (same usage as in `pgGetRast`).

* `pgGetBoundary`: now includes `clauses` parameter, with same usage
  as in `pgGetGeom`

UPDATES

* `pgWriteRast`: now attempts to write a new SRID to
  `spatial_ref_sys`, if one cannot be resolved from the raster's
  proj4string.

* All query-constructor functions (e.g. `dbDrop`) now return nothing
  (previously returned TRUE) if `exec = FALSE`.

* `pgGetGeom`: `other.cols = FALSE` now not necessary when reading
  only a geometry column (e.g., in query mode or from a one-column
  table).

BUG FIXES

* `pgWriteRast`: fixed bug affecting imported raster's metadata
  regarding the upper left pixel location (returned by PostGIS
  function `ST_UpperLeftX`/`ST_UpperLeftY(rast)`).  Previously,
  `pgWriteRast` was erroneously applying the lower left pixel location
  for this value. This did not affect the actual data of the raster,
  or their usage within PostGIS, however it could have affected export
  of this raster to external files, or viewing in a GIS.

* `pgWriteRast`: fixed bug where constraints were not written for
  tables given without a schema name in `name`.

* `pgGetGeom`: fixed bug for one-column `other.cols` specifications.

* `dbReadDataFrame`: infolocs columns of type `POSIXlt` now re-import
  time zone attribute correctly (previously just used database time
  zone).


rpostgis 1.2.1
==============

NEW FEATURES

* Support for PostGIS `GEOGRAPHY` types added, in `pgGetGeom`,
  `pgListGeom`, `pgGetBoundary`, `pgInsert`. With `pgInsert`, a
  `Spatial*` object can be uploaded to PostGIS as a geography using
  `geog = TRUE`.

BUG FIXES

* `pgInsert` and `pgWriteRast`: fixed bug causing failed uploads when
  multiple PostGIS SRIDs found with `pgSRID` for a spatial object, now
  uses the first one returned.

* `pgGetGeom`: fixed bug when loading non-spatial columns with names
  'x' or 'y' for point geometries.

UPDATES

* `pgGetRast`: `digits` argument default lowered to 5 (same as
  `raster::rasterFromXYZ`).  Resolution of the raster is now
  determined using PostGIS functions and directly applied to the R
  raster. This allows (in some cases) for faster recognition of uneven
  cell sizes, which result in an error.


rpostgis 1.2.0
==============

NEW FEATURES

* `pgGetGeom`: new `query` argument allows `pgGetGeom` to specify a
  full SQL query which returns a GEOMETRY instead of an existing
  table/view.  If desired, the query can be saved in the database as a
  new view using the `name` argument.

* `pgInsert`: now can insert geometries stored as character in
  `data.frame`s using `df.geom` argument.

BUG FIXES

* Add support for materialized views (read geometries with
  `pgGetGeom`, drop with `dbDrop`).


rpostgis 1.1.1
==============

BUG FIXES

* `pgGetGeom`: fixed bug causing failed imports for views due to usage
  of `RPostgreSQL::dbListFields` (changed to `rpostgis::dbTableInfo`).

* fixed bug for data frame mode writing (e.g., `pgInsert(...,df.mode =
  TRUE)` and `dbWriteDataFrame`) where factor names included a comma.


rpostgis 1.1.0
==============

OVERALL CHANGES

* Added four example datasets (all have prefix `roe_`).

NEW FUNCTIONS

* `pgWriteRast` to upload R Raster* datasets to PostGIS database
  tables.
  
* `dbWriteDataFrame`/`dbReadDataFrame`: Write/read in data frame mode
  to/from database table (see below).

NEW FEATURES

* `pgInsert` can write in data frame mode when `df.mode = TRUE`
  (default is `FALSE`).

* `pgInsert` now defaults to 'alter.names = FALSE'.

* `pgGetGeom` will try to read Spatial*DataFrames in data frame mode
  (will not affect import for non-data frame mode tables).
  
* `pgGetRast` now has a `band` argument to select which band in the
  database raster to import.
  
BUG FIXES

* `pgGetGeom`: Fix bug affecting one-column selections with
  `other.cols` argument for line and polygon imports.

ABOUT DATA FRAME MODE

Writing in data frame mode is only for new database tables (or for
overwriting an existing one). It will save all column names as they
appear in R, along with column data types and attributes.  This is
done by adding metadata to a lookup table in the table's schema named
'.R_df_defs' (will be created if not present).  It also adds two field
with fixed names to the database table: '.R_rownames' (storing the
`row.names` of the data frame), and '.db_pkid', which is a new integer
primary key. Existing columns in the `data.frame` matching these names
will be automatically changed.

All `Spatial*DataFrame`s writing should continue to use `pgInsert`,
which can write in data frame mode when `df.mode = TRUE`.  For more
flexible writing of `Spatial*DataFrame`s and `data.frame`s to the
database (including all inserts into existing database tables), use
`pgInsert` with `df.mode = FALSE` (default).


rpostgis 1.0.4
==============

OVERALL CHANGE

* Add package tests to repository.

BUG FIXES

* `pgGetGeom`: Fix bug which was causing errors when using 'ORDER BY
  ...'  statements in the `clauses` argument.
  

rpostgis 1.0.3
==============

OVERALL CHANGE

* Only allow <PostgreSQLConnection> for executed queries.

NEW FUNCTIONS

* `dbBuildTableQuery` (internal): Builds CREATE TABLE query for a data
  frame object.

* `dbExistsTable` (internal): Check if a PostgreSQL table exists.

* `dbConnCheck` (internal): Check if a supported PostgreSQL connection.


rpostgis 1.0.2
==============

OVERALL CHANGES

* Updated to use current search path schema as default in all
  functions (in previous versions default schema was fixed to
  'public').

NEW FUNCTIONS

* `dbVersion` (internal): PostgreSQL version checking.

NEW FEATURES

* `pgInsert` now provides 'upsert' functionality (INSERT ON CONFLICT
  UPDATE) with the 'upsert.using' argument. Requires PostgreSQL
  version 9.5+.


rpostgis 1.0.1
==============

NEW FEATURES

* `pgPostGIS` now returns TRUE for non-standard installs.


rpostgis 1.0.0
==============

OVERALL CHANGES

* Initial release to CRAN.

NEW FEATURES

* `pgGetGeom` now handles all regular PostGIS Geometry data table
  imports.


rpostgis 0.10
=============

OVERALL CHANGES

* Big package cleaning.

* Changed all non-PostGIS related functions to `db-` instead of `pg-`.

* All functions not returning an actual object now returns `TRUE` if
  successful.


rpostgis 0.8.3
==============

OVERALL CHANGE

* Change 'str' to 'query'

* Now uses `messages` instead of `cat` to print SQL code.


rpostgis 0.8.2
==============

OVERALL CHANGES

* Changed file extension to .R.


rpostgis 0.8.1
==============

OVERALL CHANGES

* Fixed dependencies (DESCRIPTION, namespace, ::).


rpostgis 0.8
============

NEW FUNCTIONS

* `pgPostGIS`: Check and create PostGIS extension.

NEW FEATURES

* `pgSchema`: Now returns TRUE if the schema exists (whether it was
  already available or was just created).


rpostgis 0.7
============

OVERALL CHANGES

* Initial merge with `pgis2r` package, incorporating support for
  loading geometry/raster objects stored in PostgreSQL databases in R.

NEW FUNCTIONS

* `pgGetRast`: Load raster from PostGIS database.

* `pgGetLines`: Load a PostGIS linestring geometry from a PostgreSQL
  table/view into R.

* `pgGetPolys`: Load a PostGIS polygon geometry from a PostgreSQL
  table/view into R.


rpostgis 0.6.1
==============

OVERALL CHANGES

* Update for roxygen2 4.0.


rpostgis 0.6
============

NEW FUNCTIONS

* `pgGetPts`: Retrieve point geometries.


rpostgis 0.5
============

OVERALL CHANGES

* Code completely commented and cleaned.

NEW FUNCTIONS

* `pgColumn`: Add or remove a column (replaces `pgDropColumn`).


rpostgis 0.4
============

NEW FUNCTIONS

* `pgMakePts` and `pgMakeStp`: Add a POINT or LINESTRING geometry
  field.


rpostgis 0.3.1
==============

OVERALL CHANGES

* Global change for `display = TRUE` to print the executed or
  non-executed query.


rpostgis 0.3
============

OVERALL CHANGES

* Non executed examples added for each function.

* `display = TRUE` by defaults for all functions.

NEW FUNCTIONS

* `pgAsDate`: Converts to timestamp.

* `pgDrop`: Drop table/view/schema.

* `pgSchema`: Create schema.

NEW FEATURES

* `pgComment`: Allows comments on schemas.


rpostgis 0.2
============

OVERALL CHANGES

* Change the package name to `rpostgis`.

* Change every `db`- function to `pg`- function, in order to avoid
  confusion with standard `DBI` or `RPostgreSQL` functions.


RPostgreSQLmod 0.1
==================

NEW FUNCTIONS

* `dbAddKey`: Add key.

* `dbComment`: Comment table/view.

* `dbVacuum`: VACUUM.

* `dbDropColumn`: Removing a Column.

* `dbIndex`: CREATE INDEX.
