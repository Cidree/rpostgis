rpostgis
========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rpostgis)](https://CRAN.R-project.org/package=rpostgis)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
![](http://cranlogs.r-pkg.org/badges/rpostgis)


This is the development area for the package `rpostgis`, which provides additional functions to the
`RPostgreSQL` package to interface R with a PostGIS-enabled database, as well as convenient wrappers to common PostgreSQL queries.

All functions require a database connection object (from the `RPostgreSQL` package) to a PostgreSQL database, e.g.:

    conn<-dbConnect("PostgreSQL",dbname='db_name',host='localhost',port='5432',user='postgres',password='PASSWORD')

You can import a PostgreSQL data table with a PostGIS `GEOMETRY` data type as a `sp`-type `Spatial*` or `Spatial*DataFrame` (points, lines or polygons) object into R:

    my_spdf<-pgGetGeom(conn, name=c("schema","my_geom_table"), geom = "my_geom_column")


The function `pgInsert` allows `sp`-type `Spatial*` and `Spatial*DataFrames` (in addition to regular R `data.frame`) objects to be written (inserted) into new or existing PostgreSQL tables:

    pgInsert(conn, name=c("schema","my_new_geom_table"), data.obj=my_spdf, geom = "my_new_geom_column", new.id = "gid")


General-purpose database functions such as `dbAddKey` and `dbIndex` provide PostgreSQL table management:

    ## Add primary key
    dbAddKey(conn, name = c("schema", "my_new_geom_table"), colname = "gid", type = "primary")
    ## Add index to GEOMETRY column
    dbIndex(conn, name = c("schema", "my_new_geom_table"), colname = "my_new_geom_column", method = "gist")
    
Package functions which are primarily for general database procedures have the prefix (`db`), while PostGIS-enabled database specific fuctions have the prefix (`pg`).

## Installation of the released versions

You can install the latest released version from CRAN:

    install.packages("rpostgis")

You can use `update.packages()` to update to the latest CRAN version.

## Installation of the development version

A stable development version of the package will be available on the project's [Github page](https://github.com/mablab/rpostgis), which may be ahead the CRAN version (check versions to verify). To install it, use the [`devtools`](https://CRAN.R-project.org/package=devtools) package from Hadley Wickham:

    library(devtools)
    install_github("mablab/rpostgis")
    
For the latest (possibly unstable) development version, use:

    install_github("mablab/rpostgis",ref="dev")

## Getting started

For a list of documented functions, use `library(help = "rpostgis")` or see
the Reference manual.
