library("mlr")
library("OpenML")
library("partyNG")
source("new_ctree_mlr.R")


## get task infos
taskinfo <- listOMLTasks(task.type = "Supervised Classification")[1:2, ]
tasks <- lapply(taskinfo$task.id, getOMLTask)
names(tasks) <- taskinfo$task.id

## create leraners
lrn.list <- list(
  makeLearner("classif.newctree", teststat = "quadratic", splitstat = "quadratic"),
  makeLearner("classif.newctree", teststat = "quadratic", splitstat = "maximum"),
  makeLearner("classif.newctree", teststat = "maximum", splitstat = "quadratic"),
  makeLearner("classif.newctree", teststat = "maximum", splitstat = "maximum")
)

## set up grid to run each learner on each task
grid <- expand.grid(task.id = taskinfo$task.id, 
                    lrn.ind = seq_along(lrn.list))


## run learners on tasks
# runs <- lapply(seq_row(grid), function(i) {
#   message(i)
#   runTaskMlr(task = tasks[[grid$task.id[i]]], 
#              learner = lrn.list[[grid$lrn.ind[i]]])
# })


problem <- runTaskMlr(task = tasks[[grid$task.id[8]]], 
                      learner = lrn.list[[grid$lrn.ind[8]]])

dat <- tasks[[grid$task.id[8]]]$input$data.set$data
save(dat, file = "problemdata.RData")

summary(dat)
tr1 <- partyNG::ctree(class ~ ., dat, control = ctree_control(teststat = "quadratic", splitstat = "maximum"))
tr4 <- partyNG::ctree(class ~ ., dat, control = ctree_control(teststat = "maximum", splitstat = "quadratic"))
