
## Test environments
* local Windows 11 Pro 22H2, 64 bit, R 4.4.2
* R Under development (unstable) (2023-10-11 r85316 ucrt)
* R-devel on Windows Server 2022, R-devel, 64 bit, with rhub::check()

## R CMD check results
There were no ERRORS or WARNINGS.

There is one note on `devtools::check()`:
```
checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
```

As stated in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be a bug/crash in MiKTeX and can likely be ignored.

There is one note in `devtools::check_win_devel()`:

```
checking CRAN incoming feasibility ... [10s] NOTE
Maintainer: 'Adrian Cidre Gonzalez <adrian.cidre@gmail.com>'
```

## Notes
Note that due to the requirement of a PostgreSQL/PostGIS enabled database for functions in this package, 
all package tests are run locally and excluded from the R build.
