## IMPORTANT
`rpostgis` is no longer actively maintained and is planning to retire in September 2023, due to 
retirement of `rgeos` later in 2023. This release adds a startup message to inform users 
of this. Unless new maintainers are found, we expect `rpostgis 1.4.4` to be the final 
CRAN release, and will request that CRAN archive the package before the end of September 2023.

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
