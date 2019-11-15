## Test environments
* local Ubuntu 18.04 install, R 3.6.1
* R Under development (unstable) (2018-11-08 r75566) with devtools::build_win()
* Debian 8, R 3.5.3

## R CMD check results
There were no ERRORs or WARNINGS. 
There is one NOTE, regarding usage of `:::` for calling an `rpostgis` internal function.
This is due to the usage of an internal function in rpostgis, that shares the name with a
function in one of rpostgis Depends packages (RPostgreSQL).

## Downstream dependencies
Ran `devtools::revdep_check()` with no ERRORs, WARNINGs, or NOTEs.

## Notes
Note that due to the requirement of a PostgreSQL/PostGIS enabled database for functions in this package, all package tests are run locally and excluded from the R build.