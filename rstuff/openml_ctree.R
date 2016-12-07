library("mlr")
library("OpenML")
library("partyNG")
source("new_ctree_mlr.R")

## parallelization
# library("parallelMap")
# ncores <- parallel::detectCores() - 1
# parallelStartSocket(ncores)
# parallelSource("new_ctree_mlr.R")

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

## obtain list of relevant tasks
set.seed(123)
taskinfo <- taskinfo[sample(1:nrow(taskinfo), size = 2), ]
tid <- taskinfo$task.id
tasks <- lapply(tid, getOMLTask)
names(tasks) <- tid

## create leraners
lrn.list <- list(
  makeLearner("classif.newctree", teststat = "quadratic", splitstat = "quadratic"),
  # makeLearner("classif.newctree", teststat = "quadratic", splitstat = "maximum"),
  # makeLearner("classif.newctree", teststat = "maximum", splitstat = "quadratic"),
  makeLearner("classif.newctree", teststat = "maximum", splitstat = "maximum")
)

## set up grid to run each learner on each task
grid <- expand.grid(task.id = taskinfo$task.id, 
                    lrn.ind = seq_along(lrn.list))


## run learners on tasks
run_lt <- function(i) {
  message(i)
  runTaskMlr(task = tasks[[as.character(grid$task.id[i])]],
             learner = lrn.list[[grid$lrn.ind[i]]])
}

# runs <- lapply(seq_row(grid), run_lt)
runs <- mclapply(seq_row(grid), run_lt,
                 mc.cores = ncores)


