##' rpostgis: R Interface to a PostGIS Database
##'
##' The **`rpostgis`** package provides an interface between **R** and
##' **PostGIS-enabled PostgreSQL databases**, allowing seamless transfer of spatial data.
##' It supports both **vector** (points, lines, polygons) and **raster** data for reading and writing.
##' Additionally, it offers convenience functions to perform common operations within **PostgreSQL/PostGIS**.
##'
##' @section Features:
##' - **Vector Data Support**: Points, lines, polygons
##' - **Raster Data Support**: Read and write raster data
##' - **PostGIS Functions**: Convenient wrappers for common PostGIS tasks
##' - **PostgreSQL Connectivity**: Interface for database interaction
##'
##' @section Getting Started:
##'
##' **1. Loading the package and connecting to a database**
##' To begin, load the `rpostgis` package and establish a connection to a PostgreSQL database:
##'
##' ```r
##' library(rpostgis)
##' con <- dbConnect("PostgreSQL", dbname = "<dbname>", host = "<host>",
##'                  user = "<user>", password = "<password>")
##' ```
##'
##' Example:
##'
##' ```r
##' con <- dbConnect("PostgreSQL", dbname = "rpostgis", host = "localhost",
##'                  user = "postgres", password = "postgres")
##' ```
##'
##' **2. Checking and Installing PostGIS**
##' Verify if **PostGIS** is installed on the database. If not, it will attempt to install it:
##'
##' ```r
##' pgPostGIS(con)
##' ```
##' This function should return `TRUE` invisibly when PostGIS is installed and ready for use.
##'
##' **3. Closing the Database Connection**
##' After finishing your work, make sure to close the connection:
##'
##' ```r
##' dbDisconnect(con)
##' ```
##'
##' @section List of Functions:
##' To view a list of all available functions in `rpostgis`, use:
##'
##' ```r
##' library(help = "rpostgis")
##' ```
##'
##' @section Additional Notes:
##' - Ensure your **PostgreSQL** instance is **PostGIS-enabled** before using `rpostgis` functions.
##' - The functions provided simplify working with spatial data and interacting with a PostGIS-enabled database.
##'
##'
##' @import RPostgreSQL
##' @import DBI
##' @importFrom methods slot "slot<-"
##' @keywords internal
"_PACKAGE"
