library("ggplot2")

load("oml_newctree.rda")

mean_pa <- tapply(oml_newctree$predictive.accuracy, oml_newctree$task.id, 
                  mean, na.rm = TRUE)
rank_pa <- rank(mean_pa)

oml_newctree$task.id <- factor(oml_newctree$task.id, levels = names(sort(rank_pa)))



library("zoo")
library("plyr")
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
