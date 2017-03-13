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
  reval0 <- listOMLRunEvaluations(task.id = task.id)
  is_interesting0 <- grepl("forest|rpart|ranger|tree", reval0$flow.name, ignore.case = TRUE)
  is_interesting1 <- !grepl("develpartykit", reval0$flow.name)
  is_interesting <- is_interesting0 & is_interesting1
  
  reval1 <- reval0[is_interesting, ]
  reval <- ddply(reval1, .(flow.name), .fun = function(x) {
    maxpa <- which.max(x$predictive.accuracy)
    x[maxpa, ]
  })
  
  return(reval)
}
reval <- ldply(unique(runeval_classif$task.id), get_comparisons)

p_classif + 
  geom_hline(data = reval, aes(yintercept = predictive.accuracy), alpha = 0.4) +
  theme_bw()

reval$program <- NA

reval$program[grepl("weka.", reval$flow.name)] <- "weka"
reval$program[grepl("classif.", reval$flow.name)] <- "R"


p_classif_c <- ggplot(runeval_classif, 
       aes(x = interaction(splittest, lookahead, intersplit, nmax), 
           group = interaction(data.name, learner), 
           color = learner)) +
  geom_hline(data = reval, alpha = 0.2, 
             aes(yintercept = predictive.accuracy, linetype = program)) +
  geom_point(aes(y = predictive.accuracy), size = 1.8) + 
  geom_line(aes(y = predictive.accuracy), size = .8) + 
  facet_grid(task.id ~ ., scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p_classif_c

ggsave(p_classif_c, filename = "OpenML_performance_ctree_classif_compare.pdf", 
       width = 11, height = 11)
