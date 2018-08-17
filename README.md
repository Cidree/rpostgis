rpostgis
========

[CRAN](https://CRAN.R-project.org/package=rpostgis)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

`rpostgis` provides an interface between R and `PostGIS`-enabled
`PostgreSQL` databases to transparently transfer spatial data. Both
vector (points, lines, polygons) and raster data are supported in read
and write modes. Also provides convenience functions to execute common
procedures in `PostgreSQL`/`PostGIS`.


## Installation of the released versions

You can install the latest released version from CRAN:

    install.packages("rpostgis")

You can then use `update.packages()` to update to the latest CRAN version.


## Installation of the development versions

A stable version of the package is always available on the project's
[GitHub page](https://github.com/mablab/rpostgis), and may be ahead of
the CRAN version. To install it, use the
[`devtools`](https://CRAN.R-project.org/package=devtools):

    library(devtools)
    install_github("mablab/rpostgis")
    
For the latest (possibly unstable) development version, use:

    install_github("mablab/rpostgis", ref = "dev")


## Getting started

`rpostgis` relies on a working connection provided by the
`RPostgreSQL` package to a PostgreSQL database, e.g.:

    conn <- RPostgreSQL::dbConnect("PostgreSQL", host = "localhost", dbname = "<DB_NAME>", 
        user = "<USER>", password = "<PASSWORD>")

Once the connection is established, the first step is to check if the
database has `PostGIS` already installed (and install it if it's not
the case):

    pgPostGIS(conn)

If the function returns `TRUE`, the database is ready and functional.
You can check the geometries and rasters present in the database with:

    pgListGeom(conn, geog = TRUE)
    pgListRast(conn)

To terminate the session, close and clear the connection with:

    RPostgreSQL::dbDisconnect(conn)


## Documentation

Full documentation with the complete list of functions of the package
can be found on `rpostgis` [homepage](https://mablab.org/rpostgis/).
