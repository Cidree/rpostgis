setwd('/home/david/git/rpostgis/')
library(devtools)
# install_github("basille/basr")
library(basr)
install()

# build ignores
use_build_ignore("rpostgis.pdf")
use_build_ignore("tests")
use_build_ignore("revdep")
use_build_ignore("cran-comments.md")
use_build_ignore("tests/test_data/")
use_build_ignore("BUILD.R")
use_build_ignore("docs")
use_build_ignore("pkgdown")

# create/update documentation
document()

# write manual
manual(overwrite=TRUE, path = "")

# run examples and spell check
# run_examples() # not using
spell_check()

# check locally
check(cran=TRUE)

# test on r-devel
check_win_devel()

# rhub version, windows with development R
# rhub::validate_email() # may need to validate e-mail with rhub prior to running check
rhub::check(platform = "windows-x86_64-devel")

# install()
# test code coverage
source("tests/testthat/tests_1_5.R") # no errors

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

## Build documentation:
pkgdown::build_site()

# release to CRAN
release()

# badges currently broken for readme
# [![CRAN Status](http://www.r-pkg.org/badges/version/rpostgis)](https://CRAN.R-project.org/package=rpostgis)
# [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
# ![](https://cranlogs.r-pkg.org/badges/rpostgis)


# 0. CRAN release ---------------------------------------------------------

## Check current CRAN check results
## - https://cran.rstudio.org/web/checks/check_results_forestdata.html

# --- R Package Checklist ---

# Preparation
urlchecker::url_check()            # Check for broken URLs
devtools::document()               # Update documentation
devtools::build_readme()           # Rebuild README
# Update cran-comments.md manually

# Run Checks
devtools::check(cran = TRUE)       # Local CRAN-like check
devtools::check_win_devel()        # Check on Windows development version
revdepcheck::revdep_check(num_workers = 4)  # Reverse dependency check

# Submission to CRAN
usethis::use_version(which = "minor")  # Increment version (patch/minor/major)
devtools::submit_cran()            # Submit to CRAN
system("git push")                 # Push changes to Git

# Post-Submission
pkgdown::build_site()              # Build package website
usethis::use_github_release()      # Tag GitHub release
usethis::use_dev_version(push = TRUE)  # Start a new development version
