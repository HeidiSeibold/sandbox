library("covr")
knitr::purl("OpenML_test_ctree.Rmd")
pkg_r_dir <- "~/Documents/svn/partykit/pkg/devel/partykit/R/"
source_files <- paste0(pkg_r_dir, c("ctree.R", "cforest.R", "urp.R"))
fc <- file_coverage(source_files = source_files,
              test_files = "OpenML_test_ctree.R")
fc
