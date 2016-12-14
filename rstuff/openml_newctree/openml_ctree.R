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

# number of classes < 11
fewclasses <- taskinfo_all$number.of.classes < 11

# no leave-one-out CV
noloo <- taskinfo_all$estimation.procedure != "Leave one out"

taskinfo_relevant <- taskinfo_all[fewmissings & goodnobs & goodnfeat & fewclasses & noloo, ]

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


