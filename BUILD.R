setwd('/home/david/git/rpostgis/')
library(devtools)
# install_github("basille/basr")
library(basr)
install()

# build ignores
# use_build_ignore("rpostgis.pdf")
# use_build_ignore("tests")
# use_build_ignore("revdep")
# use_build_ignore("cran-comments.md")
# use_build_ignore("tests/test_data/")
# use_build_ignore("BUILD.R")
# use_build_ignore("docs")
# use_build_ignore("pkgdown")

# create/update documentation
document()

# run examples and checks (not using these)
# run_examples()
# spell_check()

# check locally
check(cran=TRUE)

# test on r-devel
check_win_devel()

# rhub version, windows with development R
library(rhub) #library for testing on a variety of system configs
check(platform = "windows-x86_64-devel")

# write manual
manual(overwrite=TRUE)

# install()
# test code coverage
source("tests/tests_basic.R")
print(ae.all) # all.equal test results (returns all TRUE except for one)

# check code coverage
library(covr)
library(DT)
x<-package_coverage(quiet = FALSE)
report(x)

# tidy code
# library(formatR)
# tidy_source(arrow=TRUE,width.cutoff = 60)

# check reverse dependencies (make sure to run on clean workspace)
rm(list=ls())
devtools::revdep()

library(revdepcheck)
revdep_check()
revdep_reset()

# release to CRAN
release()

## Build documentation:
pkgdown::build_site()

# badges currently broken for readme
# [![CRAN Status](http://www.r-pkg.org/badges/version/rpostgis)](https://CRAN.R-project.org/package=rpostgis)
# [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
# ![](https://cranlogs.r-pkg.org/badges/rpostgis)