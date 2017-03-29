library("OpenML")
library("mlr")
library("batchtools")


## load task ids used for ctree checking
load(file = "results_classif_batchtools.rda")
eval_classif <- listOMLRunEvaluations(run.id = res_classif$run.id)
runeval_classif <- cbind(res_classif, eval_classif)

task_ids <- unique(runeval_classif$task.id)





########################################
###  Preparation for parallel computing 
########################################

## before creating the registry, check if it already exists
## if so, delete it
unlink("check_forests_classif", recursive = TRUE)

## create the experiment registry
reg_classif <- makeExperimentRegistry(file.dir = "check_forests_classif",
                                      packages= c("mlr", "OpenML"),
                                      seed = 123)

## allow for parallel computing, for other options see ?makeClusterFunctions
reg_classif$cluster.functions <- makeClusterFunctionsMulticore()



########################################
###  Preparation of tasks / problems
########################################

## get classification tasks
tasks_classif <- lapply(task_ids, getOMLTask)
names(tasks_classif) <- tid_classif


## add the problem, in our case the tasks from OpenML
for(task in tasks_classif) {
  addProblem(name = as.character(task$task.id), data = task,
             reg = reg_classif)
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
    run_id = uploadOMLRun(run, tag = "trees", confirm.upload = FALSE)
    return(run_id)
  } else {
    return(run)
  }
}

## add the algorithm
addAlgorithm(name = "mlr", fun = runTask_uploadRun, reg = reg_classif)



########################################
###  Preparation of experiment
########################################

## Grid of things to compute / experiment 
## (for now only change one parameter from the default)
grid_classif <- data.frame(learner = c("classif.rpart", 
                                       "classif.ctree", 
                                       "classif.cforest", 
                                       # "classif.extraTrees", # no NA allowed
                                       # "classif.h2o.randomForest", # no NA allowed
                                       # "classif.randomForest", # no NA allowed
                                       "classif.randomForestSRC"
                                       # "classif.ranger" # no NA allowed
                                       ), 
                           stringsAsFactors = FALSE)



addExperiments(algo.designs = list(mlr = grid_classif), repls = 1, reg = reg_classif)




########################################
###  Running
########################################
# testJob(id = 7, reg = reg_classif)

submitJobs(reg = reg_classif)

getStatus(reg = reg_classif)

## collect runs and upload if no errors

# classification
waitForJobs(reg = reg_classif)
(err_classif  <- getErrorMessages(reg = reg_classif))
runs_classif <- reduceResultsList(reg = reg_classif)

if(NROW(err_classif) == 0) {
  runids_classif <- sapply(runs_classif, uploadOMLRun, 
                           tag = "study_38", confirm.upload = FALSE) 
  
  res_classif <- cbind(run.id = runids_classif, getJobPars(reg = reg_classif))
  
  save(res_classif, file = "results_classif_trees_batchtools.rda")
}




