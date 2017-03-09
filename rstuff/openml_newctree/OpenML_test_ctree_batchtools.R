library("partykit")
library("OpenML")
library("mlr")
library("batchtools")


## mlr implementation of ctree and cforest
source("https://raw.githubusercontent.com/HeidiSeibold/sandbox/master/rstuff/openml_newctree/new_ctree_mlr.R")

## number of data sets to look at
n_sets <- 10

## description of the flow
flow_descr <- "Please use the mlr add-on code https://github.com/HeidiSeibold/sandbox/blob/master/rstuff/openml_newctree/new_ctree_mlr.R
and devel partykit package revision 1134: https://r-forge.r-project.org/scm/viewvc.php/pkg/devel/?root=partykit"



########################################
###  Preparation for parallel computing 
########################################

## before creating the registry, check if it already exists
## if so, delete it
unlink("check_new_ctree_classif", recursive = TRUE)
unlink("check_new_ctree_regr", recursive = TRUE)

## create the experiment registry
reg_classif <- makeExperimentRegistry(file.dir = "check_new_ctree_classif",
                                      packages= c("mlr", "OpenML", "partykit"),
                                      seed = 123)
reg_regr <- makeExperimentRegistry(file.dir = "check_new_ctree_regr",
                                   packages= c("mlr", "OpenML", "partykit"),
                                   seed = 123)

## allow for parallel computing, for other options see ?makeClusterFunctions
reg_classif$cluster.functions <- makeClusterFunctionsMulticore()
reg_regr$cluster.functions <- makeClusterFunctionsMulticore()



########################################
###  Preparation of tasks / problems
########################################

## get classification tasks
set.seed(123)
taskinfo_classif0 <- listOMLTasks(task.type = "Supervised Classification",
                                  evaluation.measures = "predictive_accuracy",
                                  number.of.instances = c(80, 10^6),
                                  number.of.classes = c(2, 10), 
                                  number.of.features = c(3, 50),
                                  number.of.missing.values = c(1, 10^6),
                                  estimation.procedure = "10-fold Crossvalidation")
taskinfo_classif1 <- taskinfo_classif0[taskinfo_classif0$max.nominal.att.distinct.values < 10 &
                                         !is.na(taskinfo_classif0$max.nominal.att.distinct.values), ]

taskinfo_classif <- taskinfo_classif1[sample(seq_len(NROW(taskinfo_classif1)), size = n_sets), ]

tid_classif <- taskinfo_classif$task.id
tasks_classif <- lapply(tid_classif, getOMLTask)
names(tasks_classif) <- tid_classif


## get regression tasks
set.seed(123)
taskinfo_regr0 <- listOMLTasks(task.type = "Supervised Regression",
                               evaluation.measures = "mean_absolute_error",
                               number.of.instances = c(80, 10^6),
                               number.of.features = c(3, 50),
                               number.of.missing.values = c(1, 10^6),
                               estimation.procedure = "10-fold Crossvalidation")
taskinfo_regr1 <- taskinfo_regr0[taskinfo_regr0$max.nominal.att.distinct.values < 10 &
                                   !is.na(taskinfo_regr0$max.nominal.att.distinct.values), ]

taskinfo_regr <- taskinfo_regr1[sample(seq_len(NROW(taskinfo_regr1)), size = n_sets), ]

tid_regr <- taskinfo_regr$task.id
tasks_regr <- lapply(tid_regr, getOMLTask)
names(tasks_regr) <- tid_regr



## add the problem, in our case the tasks from OpenML
for(task in tasks_classif) {
  addProblem(name = as.character(task$task.id), data = task,
             reg = reg_classif)
}

for(task in tasks_regr) {
  addProblem(name = as.character(task$task.id), data = task,
             reg = reg_regr)
}



########################################
###  Preparation of algorithms 
########################################

##' Function that takes the task (data) and the learner, runs the learner on
##' the task, uploads the run and returns the run ID.
##' 
##' @param job required argument for addAlgorithm
##' @param instance required argument for addAlgorithm
##' @param data the task
##' @param learner the string that defines the learner, see listLearners()
runTask_uploadRun = function(job, instance, data, learner, ..., upload = FALSE) {
  
  learner = makeLearner(learner, par.vals = list(...))
  run = runTaskMlr(data, learner)
  
  if(upload) {
    run_id = uploadOMLRun(run, tag = "study_38", confirm.upload = FALSE)
    return(run_id)
  } else {
    return(run)
  }
}

## add the algorithm
addAlgorithm(name = "mlr", fun = runTask_uploadRun, reg = reg_classif)
addAlgorithm(name = "mlr", fun = runTask_uploadRun, reg = reg_regr)



########################################
###  Preparation of experiment
########################################

## Grid of things to compute / experiment 
## (for now only change one parameter from the default)
grid <- expand.grid(learner = c("develpartykit.ctree", "develpartykit.cforest"),
                    splittest = c(FALSE, TRUE),
                    lookahead = c(FALSE, TRUE),
                    intersplit = c(FALSE, TRUE),
                    nmax = c(Inf, 20), 
                    nresample = 500, testtype = "Bonferroni", stringsAsFactors = FALSE)
grid$testtype[grid$splittest == TRUE] <- "MonteCarlo"
grid <- grid[c(1:2, 3:4, 5:6, 9:10, 17:18), ]
# grid <- grid[1:2, ]

# correct names for classification and regression
grid_classif <- grid_regr <- grid
grid_classif$learner <- paste("classif", grid_classif$learner, sep = ".")
grid_regr$learner <- paste("regr", grid_regr$learner, sep = ".")


addExperiments(algo.designs = list(mlr = grid_classif), repls = 1, reg = reg_classif)
addExperiments(algo.designs = list(mlr = grid_regr), repls = 1, reg = reg_regr)




########################################
###  Running
########################################
# testJob(id = 1, reg = reg_classif)
# testJob(id = 1, reg = reg_regr)

submitJobs(reg = reg_classif)
submitJobs(reg = reg_regr)

getStatus(reg = reg_classif)
getStatus(reg = reg_regr)

## collect runs and upload if no errors

# classification
waitForJobs(reg = reg_classif)
(err_classif  <- getErrorMessages(reg = reg_classif))
runs_classif <- reduceResultsList(reg = reg_classif)

if(NROW(err_classif) == 0) {
  runids_classif <- sapply(runs_classif, uploadOMLRun, 
                           tag = "study_38", confirm.upload = FALSE) 
  
  res_classif <- cbind(run.id = runids_classif, getJobPars(reg = reg_classif))
  
  save(res_classif, file = "results_classif_batchtools.rda")
}
  

# regression
waitForJobs(reg = reg_regr)
(err_regr <- getErrorMessages(reg = reg_regr))
runs_regr <- reduceResultsList(reg = reg_regr)

if(NROW(err_regr) == 0) {
  runids_regr <- sapply(runs_regr, uploadOMLRun, 
                        tag = "study_38", confirm.upload = FALSE) 
  
  res_regr <- cbind(run.id = runids_regr, getJobPars(reg = reg_regr))
  
  save(res_regr, file = "results_regr_batchtools.rda")
}

