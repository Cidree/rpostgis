## Test environments
* local Windows 7 install, R 3.3.3
* Windows Server 2008 R2 SP1, 32/64 bit (R-devel)
* Debian 8.6, R 3.3.3

## R CMD check results
There were no ERRORs, WARNINGS, or NOTEs.

Note that due to the requirement of a PostgreSQL/PostGIS enabled database for functions in this package, all package tests are run locally and excluded from the R build.

## Downstream dependencies
Ran `devtools::revdep_check()` with no ERRORs, WARNINGS, or NOTEs.