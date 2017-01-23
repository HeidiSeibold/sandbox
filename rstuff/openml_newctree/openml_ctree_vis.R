library("ggplot2")
library("zoo")
library("plyr")


###### teststat, splitstat
load("oml_newctree.rda")

mean_pa <- tapply(oml_newctree$predictive.accuracy, oml_newctree$task.id, 
                  mean, na.rm = TRUE)
rank_pa <- rank(mean_pa)

oml_newctree$task.id <- factor(oml_newctree$task.id, levels = names(sort(rank_pa)))



res <- ddply(oml_newctree, .(task.id),  function(dd) {
  x <- dd$predictive.accuracy
  dd$pa_forward <- if(all(is.na(x))) {
    x
  } else {
    na.locf(x)
  }
  return(dd)
})
lastlevel <- res$teststat == "quadratic" & res$splitstat == "quadratic"

plot_pa <- ggplot(res, 
       aes(#x = setup.id, 
         x = interaction(teststat, splitstat),
         y = predictive.accuracy, 
         color = number.of.classes,
         group = task.id)) + 
  geom_point(alpha = 0.4) + 
  geom_line(alpha = 0.4) + 
  geom_text(data = res[lastlevel, ], 
            aes(label = task.id, y = pa_forward),
            hjust = 0, nudge_x = 0.05) +
  theme_classic() #+
  # theme(legend.position = "none")
plot_pa 
ggsave(filename = "ctree_predictive_accuracy_plot.pdf",
       plot = plot_pa)


table(res$errormessage)




###### splittest
load("oml_newctree_splittest.rda")
runeval <- listOMLRunEvaluations(tag = "study_38")
runeval$task.id <- as.factor(runeval$task.id)
runeval$setup.id <- as.factor(runeval$setup.id)
runeval <- runeval[runeval$run.id %in% oml_newctree$run.id, ]

select <- c("run.id", "splittest","number.of.classes", "number.of.features", 
            "number.of.numeric.features", "number.of.instances.with.missing.values",
            "number.of.instances.x")
oml_newctree1 <- merge(oml_newctree[ , select], 
                       runeval, all = TRUE)

mean_pa <- tapply(oml_newctree1$predictive.accuracy, oml_newctree1$task.id, 
                  mean, na.rm = TRUE)
rank_pa <- rank(mean_pa)

oml_newctree1$task.id <- factor(oml_newctree1$task.id, levels = names(sort(rank_pa)))



res <- ddply(oml_newctree1, .(task.id),  function(dd) {
  x <- dd$predictive.accuracy
  dd$pa_forward <- if(all(is.na(x))) {
    x
  } else {
    na.locf(x)
  }
  return(dd)
})
lastlevel <- res$splittest == TRUE

plot_pa <- ggplot(res, 
                  aes(
                    x = splittest,
                    y = predictive.accuracy, 
                    color = number.of.features,
                    group = task.id)) + 
  geom_point(alpha = 0.4, aes(size = number.of.classes)) + 
  geom_line(alpha = 0.6) + 
  geom_text(data = res[lastlevel, ], 
            aes(label = task.id, y = pa_forward),
            hjust = 0, nudge_x = 0.05) +
  theme_classic() 
plot_pa 
ggsave(filename = "ctree_predictive_accuracy_splittest_plot.pdf",
       plot = plot_pa)


table(res$errormessage)