library("ggplot2")
library("OpenML")

load(file = "results_regr_batchtools.rda")


eval_regr <- listOMLRunEvaluations(run.id = res_regr$run.id)
runeval_regr <- cbind(res_regr, eval_regr)

p_regr <- ggplot(runeval_regr, 
                 aes(x = interaction(splittest, lookahead, intersplit, nmax), 
                     color = data.name, group = interaction(data.name, learner), 
                     linetype = learner)) +
  geom_point(aes(y = mean.absolute.error)) + 
  geom_line(aes(y = mean.absolute.error))


load(file = "results_classif_batchtools.rda")

eval_classif <- listOMLRunEvaluations(run.id = res_classif$run.id)
runeval_classif <- cbind(res_classif, eval_classif)

p_classif <- ggplot(runeval_classif, 
                    aes(x = interaction(splittest, lookahead, intersplit, nmax), 
                        color = data.name, group = interaction(data.name, learner), 
                        linetype = learner)) +
  geom_point(aes(y = predictive.accuracy)) + 
  geom_line(aes(y = predictive.accuracy))


ggsave(p_regr, filename = "OpenML_performance_ctree_regr.pdf", 
       width = 11, height = 6)
ggsave(p_classif, filename = "OpenML_performance_ctree_classif.pdf", 
       width = 11, height = 6)
