library("covr")
getwd()
pkg_dir <- "~/partykit/pkg/devel/partykit"
tst_dir <- "~/sandbox/rstuff/openml_newctree/"
knitr::purl("OpenML_test_ctree.Rmd")
excl <- list("R/as.party.R",
              "R/glmtree.R",
              "R/lmtree.R",
              "R/meanvartree.R",
              "R/mob-plot.R",
              "R/modelparty2.R",
              "R/modelparty.R",
              "R/mtree.R",
              "R/palmtree.R",
              "R/plot.R",
              "R/pmmlTreeModel.R",
              "R/print.R",
              "R/simpleparty.R",
              "R/utils.R",
              "R/varimp.R")
pc <- package_coverage(path = pkg_dir, type = "none", 
                       code = "source('OpenML_test_ctree.R', echo = TRUE)",
                       exclusions = excl)
pc
save(pc, file = "code_coverage.rda")

zero_coverage(pc)
