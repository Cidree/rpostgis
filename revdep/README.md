# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.0 (2017-04-21) |
|system   |x86_64, mingw32              |
|ui       |RStudio (1.0.143)            |
|language |(EN)                         |
|collate  |English_United States.1252   |
|tz       |America/New_York             |
|date     |2017-06-30                   |

## Packages

|package     |*  |version |date       |source                     |
|:-----------|:--|:-------|:----------|:--------------------------|
|DBI         |*  |0.7     |2017-06-18 |CRAN (R 3.4.0)             |
|rgdal       |   |1.2-7   |2017-04-25 |CRAN (R 3.4.0)             |
|rgeos       |   |0.3-23  |2017-04-06 |CRAN (R 3.4.0)             |
|rpostgis    |*  |1.3.0   |2017-06-30 |local (mablab/rpostgis@NA) |
|RPostgreSQL |*  |0.6-2   |2017-06-24 |CRAN (R 3.4.0)             |
|sp          |*  |1.2-4   |2016-12-22 |CRAN (R 3.4.0)             |
|wkb         |   |0.3-0   |2016-03-24 |CRAN (R 3.4.0)             |

# Check results

1 packages

|package    |version | errors| warnings| notes|
|:----------|:-------|------:|--------:|-----:|
|rpostgisLT |0.5.0   |      0|        0|     1|

## rpostgisLT (0.5.0)
Maintainer: David Bucklin <dbucklin@ufl.edu>  
Bug reports: https://github.com/mablab/rpostgisLT/issues

0 errors | 0 warnings | 1 note 

```
checking dependencies in R code ... NOTE
Unexported objects imported by ':::' calls:
  'rpostgis:::dbBuildTableQuery' 'rpostgis:::dbConnCheck'
  'rpostgis:::dbExistsTable' 'rpostgis:::dbTableNameFix'
  See the note in ?`:::` about the use of this operator.
```

