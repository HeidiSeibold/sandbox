library("ggplot2")
library("OpenML")

load(file = "results_regr_batchtools.rda")


eval_regr <- listOMLRunEvaluations(run.id = res_regr$run.id)
runeval_regr <- cbind(res_regr, eval_regr)

ggplot(runeval_regr, aes(x = interaction(splittest, lookahead, intersplit, nmax), 
                         color = data.name, group = interaction(data.name, learner), linetype = learner)) +
  geom_point(aes(y = mean.absolute.error)) + geom_line(aes(y = mean.absolute.error))



eval_classif <- listOMLRunEvaluations(run.id = res_classif$run.id)
runeval_classif <- cbind(res_classif, eval_classif)

ggplot(runeval_classif, aes(x = parameter, color = data.name, group = interaction(data.name, type), linetype = type)) +
  geom_point(aes(y = predictive.accuracy)) + geom_line(aes(y = predictive.accuracy))
