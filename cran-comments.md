## IMPORTANT
`rpostgis` is under new maintenance, and development will be moving to a new location soon! For more information, see: [https://github.com/mablab/rpostgis/issues/28](https://github.com/mablab/rpostgis/issues/28)

## Test environments
* local Windows 11 Pro 22H2, 64 bit, R 4.3.1

## R CMD check results
There were no ERRORS, WARNINGS, or NOTES

## Downstream dependencies
Ran `revdepcheck::revdep_check()`. There is one package (`lucas`) with reverse dependency, who we
are in contact with regarding the retirement of `rpostgis`.

## Notes
Note that due to the requirement of a PostgreSQL/PostGIS enabled database for functions in this package, 
all package tests are run locally and excluded from the R build.
