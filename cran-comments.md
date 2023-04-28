## Test environments
* local x86_64-pc-linux-gnu, R 4.2.2
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