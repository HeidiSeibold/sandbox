library("ggplot2")
library("OpenML")
library("plyr")

load(file = "results_regr_batchtools.rda")


eval_regr <- listOMLRunEvaluations(run.id = res_regr$run.id)
runeval_regr <- cbind(res_regr, eval_regr)

p_regr <- ggplot(runeval_regr, 
                 aes(x = interaction(splittest, lookahead, intersplit, nmax), 
                     color = data.name, group = interaction(data.name, learner, task.id), 
                     linetype = learner)) +
  geom_point(aes(y = mean.absolute.error)) + 
  geom_line(aes(y = mean.absolute.error)) + 
  facet_grid(task.id ~ ., scales = "free_y")
p_regr


load(file = "results_classif_batchtools.rda")

eval_classif <- listOMLRunEvaluations(run.id = res_classif$run.id)
runeval_classif <- cbind(res_classif, eval_classif)

p_classif <- ggplot(runeval_classif, 
                    aes(x = interaction(splittest, lookahead, intersplit, nmax), 
                        group = interaction(data.name, learner), 
                        linetype = learner)) +
  geom_point(aes(y = predictive.accuracy)) + 
  geom_line(aes(y = predictive.accuracy)) + 
  facet_grid(task.id ~ ., scales = "free_y")
p_classif


ggsave(p_regr, filename = "OpenML_performance_ctree_regr.pdf", 
       width = 11, height = 7)
ggsave(p_classif, filename = "OpenML_performance_ctree_classif.pdf", 
       width = 11, height = 7)



## check classif performance in comparison with other tree/forest algorithms

get_comparisons <- function(task.id) {
  reval0 <- listOMLRunEvaluations(task.id = task.id, tag = "study_38")
  is_interesting <- !grepl("develpartykit", reval0$flow.name)
  
  reval <- reval0[is_interesting, ]
  
  return(reval)
}
reval <- ldply(unique(runeval_classif$task.id), get_comparisons)

p_classif_c <- p_classif + 
  geom_hline(data = reval, aes(yintercept = predictive.accuracy,
                               color = flow.name), alpha = 0.7) +
  theme_classic()
p_classif_c



ggsave(p_classif_c, filename = "OpenML_performance_ctree_classif_compare.pdf", 
       width = 11, height = 11)
