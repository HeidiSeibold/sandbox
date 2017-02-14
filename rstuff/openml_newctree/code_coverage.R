library("covr")
getwd()
pkg_dir <- "~/partykit/pkg/devel/partykit"
tst_dir <- "~/sandbox/rstuff/openml_newctree/"
knitr::purl("OpenML_test_ctree.Rmd")
pc <- package_coverage(path = pkg_dir, type = "none", 
                       code = "OpenML_test_ctree.R")
pc <- package_coverage(path = pkg_dir, type = "none", 
                       code = file.path(tst_dir, "OpenML_test_ctree.R"))
pc <- package_coverage(path = pkg_dir, type = "none", 
                       code = paste0(tst_dir, "OpenML_test_ctree.R"))
pc <- package_coverage(path = pkg_dir, type = "none", 
                       code = "source('OpenML_test_ctree.R', echo = TRUE)")
pc
