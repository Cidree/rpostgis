## Test environments
* local Ubuntu 16.04 install, R 3.4.3
* R Under development (unstable) (2017-09-12 r73242) with devtools::build_win()
* Debian 8, R 3.4.0

## R CMD check results
There were no ERRORs, WARNINGS, or NOTEs.

## Downstream dependencies
Ran `devtools::revdep_check()` with no ERRORs or WARNINGs. 

The only NOTE is about the use of ":::" to access rpostgis internal functions within a package we also maintain (rpostgisLT).

## Notes

Note that due to the requirement of a PostgreSQL/PostGIS enabled database for functions in this package, all package tests are run locally and excluded from the R build.