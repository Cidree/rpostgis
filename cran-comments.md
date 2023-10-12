## IMPORTANT
`rpostgis` is under new maintenance, and development will be moving to a new location soon! For more information, see: [https://github.com/mablab/rpostgis/issues/28](https://github.com/mablab/rpostgis/issues/28)

## Test environments
* local Windows 11 Pro 22H2, 64 bit, R 4.3.1
* R Under development (unstable) (2023-10-10 r85312 ucrt)
* R-devel on Windows Server 2022, R-devel, 64 bit, with rhub::check()

## R CMD check results
There were no ERRORS or WARNINGS.
There is one NOTE about the maintainer change, with devtools::check_win_devel()

## Downstream dependencies
Ran `revdepcheck::revdep_check()` locally, and found no ERRORs, WARNINGS, or NOTES.

## Notes
Note that due to the requirement of a PostgreSQL/PostGIS enabled database for functions in this package, 
all package tests are run locally and excluded from the R build.
