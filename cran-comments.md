## IMPORTANT
`rpostgis` is under new maintenance, and development will be moving to a new location soon! For more information, see: [https://github.com/mablab/rpostgis/issues/28](https://github.com/mablab/rpostgis/issues/28)

## Test environments
* local Ubuntu 18.04.6 LTS, R 4.3.0
* R Under development (unstable) (2023-04-27 r84335 ucrt)
* R-devel on Windows Server 2022, R-devel, 64 bit, with rhub::check()

## R CMD check results
There were no ERRORS, WARNINGS, or NOTES

## Downstream dependencies
Ran `revdepcheck::revdep_check()`. There is one package (`lucas`) with reverse dependency, who we
are in contact with regarding the retirement of `rpostgis`.

## Notes
Note that due to the requirement of a PostgreSQL/PostGIS enabled database for functions in this package, 
all package tests are run locally and excluded from the R build.
