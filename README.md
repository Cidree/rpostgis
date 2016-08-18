rpostgis
========

This is the development area for the package `rpostgis`, which provides additional functions to the
RPostgreSQL package to interface R with a PostGIS-enabled database, as well as convenient wrappers to common PostgreSQL queries.

All functions require a database connection object (from the `DBI` package) to a PostgreSQL database, e.g.:

    drv<-dbDriver("PostgreSQL")
    conn<-dbConnect(drv,dbname='db_name',host='localhost',port='5432',user='postgres',password='PASSWORD')

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

See [the home page for the released versions](http://ase-research.org/basille/rpostgis/).

## Installation of the development version

You need to use the package
[`devtools`](http://cran.r-project.org/package=devtools)
from Hadley Wickham:

    library(devtools)
    install_github("mablab/rpostgis")


## Installation of the released versions

Binaries for Windows are (sometimes) maintained for the
latest R release, and can be installed with the command:

    install.packages("rpostgis", repos = "http://ase-research.org/R/")

If it's not the case, and with other operating systems (such as GNU/Linux or OS X), you can compile the source package with the command:

    install.packages("rpostgis", repos = "http://ase-research.org/R/", type = "source")

The upgrade process (e.g. using `update.packages()`) is not operative on
packages from this repository. Simply reinstall the package with one of the
commands above to upgrade it when necessary.


## Getting started

For a list of documented functions, use `library(help = "rpostgis")` or see
the Reference manual.
