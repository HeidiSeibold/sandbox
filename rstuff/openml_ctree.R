library("mlr")
library("OpenML")
options( java.parameters = "-Xmx4g" )
setOMLConfig(arff.reader = "RWeka") 

library("partyNG")
source("new_ctree_mlr.R")

## parallelization -> mclapply
library("parallel")
ncores <- detectCores() - 1


## get task infos and keep only relevant ones
taskinfo_all <- listOMLTasks(task.type = "Supervised Classification")

# less than 30 percent missing
fewmissings <- taskinfo_all$number.of.instances.with.missing.values / 
  taskinfo_all$number.of.instances < 0.3

# nobs in (80, 1Mio)
goodnobs <- taskinfo_all$number.of.instances > 80 &
  taskinfo_all$number.of.instances < 10^6

# nfeatures in (3, 50)
goodnfeat <- taskinfo_all$number.of.features > 3 &
  taskinfo_all$number.of.features < 50

# no leave-one-out CV
noloo <- taskinfo_all$estimation.procedure != "Leave one out"

taskinfo_relevant <- taskinfo_all[fewmissings & goodnobs & goodnfeat & noloo, ]

# each data set only once
dup <- duplicated(taskinfo_relevant$data.id)
taskinfo <- taskinfo_relevant[!dup, ]

## take a sample of tasks
# set.seed(123)
set.seed(1234)
taskinfo <- taskinfo[sample(1:nrow(taskinfo), size = 50), ]

## obtain list of relevant tasks
tid <- taskinfo$task.id
tasks <- lapply(tid, getOMLTask)
names(tasks) <- tid

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



