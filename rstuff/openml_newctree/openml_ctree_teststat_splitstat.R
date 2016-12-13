source("openml_ctree.R")
set.seed(123)

## create leraners
lrn.list <- list(
  makeLearner("classif.develpartykit.ctree", teststat = "quadratic", splitstat = "quadratic"),
  makeLearner("classif.develpartykit.ctree", teststat = "quadratic", splitstat = "maximum"),
  makeLearner("classif.develpartykit.ctree", teststat = "maximum", splitstat = "quadratic"),
  makeLearner("classif.develpartykit.ctree", teststat = "maximum", splitstat = "maximum")
)

## creat OMLFlows and upload
flow.list <- lapply(lrn.list, function(x) {
  nx <- convertMlrLearnerToOMLFlow(x)
  nx$description <- "Please use the mlr add-on code 
  https://github.com/HeidiSeibold/sandbox/blob/95fb76a6c6c645dc5915754b739f2c00b12df542/rstuff/openml_ctree.R
  and devel partykit package revision 1034: https://r-forge.r-project.org/scm/viewvc.php/pkg/devel/?root=partyki"
  return(nx)
})
lapply(flow.list, uploadOMLFlow)


## set up grid to run each learner on each task
grid <- expand.grid(task.id = taskinfo$task.id, 
                    lrn.ind = seq_along(lrn.list))


## run learners on tasks
run_lt <- function(i) {
  message(i)
  runTaskMlr(task = tasks[[as.character(grid$task.id[i])]],
             learner = lrn.list[[grid$lrn.ind[i]]])
}

runs <- mclapply(seq_row(grid), run_lt,
                 mc.cores = ncores)


## upload runs
upload_runs <- function(run){
  if(class(run) == "OMLMlrRun") {
    uploadOMLRun(run, tags = "study_38", confirm.upload = FALSE)
  } else {
    return(NA)
  }
}
run.id <- lapply(runs, upload_runs)


## add parameter info
grid1 <- cbind(grid, 
               t(sapply(lrn.list, function(x) unlist(x$par.vals)))[grid$lrn.ind,])

## info on run.id
grid1$run.id <- unlist(run.id)

## add data name
grid2 <- merge(grid1, taskinfo, all = TRUE)

## add info on whether run is without error
gridinfo <- cbind(grid2, class = sapply(runs, class))
gridinfo$errormessage <- sapply(runs, function(x) ifelse(class(x) == "try-error", x, NA))




## get infos form OpenML and merge
# OMLRuns
runinfo <- listOMLRuns(tag = "study_38")
runinfo <- runinfo[runinfo$run.id %in% gridinfo$run.id, ]
runinfo1 <- merge(gridinfo, runinfo, all = TRUE, by = c("task.id", "run.id"))
stopifnot(NROW(runinfo1) == 200)

# OMLRunEvaluations
runeval <- listOMLRunEvaluations(tag = "study_38")
runeval$task.id <- as.factor(runeval$task.id)
runeval$setup.id <- as.factor(runeval$setup.id)
runeval <- runeval[runeval$run.id %in% gridinfo$run.id, ]
oml_newctree <- merge(runinfo1, runeval, all = TRUE, 
                      by = c("task.id", "run.id", "setup.id", "flow.id")) # can be deleted once issue is solved https://github.com/openml/openml-r/issues/299
stopifnot(NROW(oml_newctree) == 200)


save(oml_newctree, file = "oml_newctree.rda")

