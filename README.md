# rpostgis <img src="man/figures/rpostgis-1024-white.png" align="right" width="200px"/>

<!-- badges: start -->
[![CRAN Status](http://www.r-pkg.org/badges/version/rpostgis)](https://CRAN.R-project.org/package=rpostgis)
[![Project Status: Inactive - The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
![](https://cranlogs.r-pkg.org/badges/rpostgis)
<!-- badges: end -->

The `rpostgis` package provides an interface between R and
[`PostGIS`](https://postgis.net/)-enabled
[`PostgreSQL`](https://www.postgresql.org/) databases to transparently
transfer spatial data. Both vector (points, lines, polygons) and
raster data are supported in read and write modes. Also provides
convenience functions to execute common procedures in
`PostgreSQL`/`PostGIS`.

> **Warning** 
> Due to retirement of the package `rgeos`, `rpostgis` will retire in September 2023.
>  * For vector operations, please check package `sf`, which provides a mechanism
>    to connect to PostGIS databases.
>  * For raster operations, no alternative solution is identified yet.
>  * For general database operations, use 'RPostgreSQL' directly.
> 
> If you are interested in the development and maintenance of `rpostgis`, please
> check [this issue on the GitHub
> repository](https://github.com/mablab/rpostgis/issues/28).
> For more information on R-spatial evolution, visit: [https://r-spatial.org/](https://r-spatial.org/).

## Installation of the released versions

You can install the latest released version from CRAN:

```r
install.packages("rpostgis")
```

You can then use `update.packages()` to update to the latest CRAN
version.


## Installation of the development versions

A stable version of the package is always available on the project's
[GitHub page](https://github.com/mablab/rpostgis), and may be ahead of
the CRAN version. To install it, use the
[`remotes`](https://CRAN.R-project.org/package=remotes):

```r
remotes::install_github("mablab/rpostgis")
```
    
For the latest (possibly unstable) development version, use:

```r
remotes::install_github("mablab/rpostgis", ref = "dev")
```


## Getting started

`rpostgis` relies on a working connection provided by the
`RPostgreSQL` package to a PostgreSQL database, e.g.:

```r
conn <- RPostgreSQL::dbConnect("PostgreSQL", host = "localhost",
    dbname = "<DB_NAME>", user = "<USER>", password = "<PASSWORD>")
```
        
> Note: as of `rpostgis 1.4.3` the `RPostgres::Postgres()` driver is also
> allowed for connection objects; however, this should be considered
> experimental and is not recommended for most use cases.

Once the connection is established, the first step is to check if the
database has `PostGIS` already installed (and install it if it's not
the case):

```r
pgPostGIS(conn)
```

If the function returns `TRUE`, the database is ready and functional.
You can check the geometries and rasters present in the database with:

```r
pgListGeom(conn, geog = TRUE)
pgListRast(conn)
```

To terminate the session, close and clear the connection with:

```r
RPostgreSQL::dbDisconnect(conn)
```


## Documentation

Full documentation with the complete list of functions of the package
can be found on `rpostgis` [homepage](https://mablab.org/rpostgis/).
