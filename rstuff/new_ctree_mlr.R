library("mlr")


#' @export
makeRLearner.classif.newctree = function() {
  makeRLearnerClassif(
    cl = "classif.newctree",
    package = "partykit",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "teststat", default = "quadratic", values = c("quadratic", "maximum")),
      makeDiscreteLearnerParam(id = "splitstat", default = "quadratic", values = c("quadratic", "maximum")),
      makeLogicalLearnerParam(id = "splittest", default = FALSE),
      makeDiscreteLearnerParam(id = "testtype", default = "Bonferroni", values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
      makeNumericLearnerParam(id = "nmax", default = Inf, lower = 0, allow.inf = TRUE),
      makeNumericLearnerParam(id = "alpha", default = 0.05, lower = 0, upper = 1),
      # makeNumericLearnerParam(id = "mincriterion", default = 1 - alpha, lower = 0, upper = 1),
      # makeNumericLearnerParam(id = "logmincriterion", default = log(mincriterion), lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", default = 7L, lower = 1L),
      makeNumericLearnerParam(id = "minprob", default = 0.01, lower = 0, upper = 1),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeLogicalLearnerParam(id = "lookahead", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", default = 9999L, lower = 1L, requires = quote(testtype=="MonteCarlo")),
      makeLogicalLearnerParam(id = "MIA", default = FALSE),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 0L, lower = 0L),
      makeIntegerLearnerParam(id = "mtry", default = Inf, lower = 1, special.vals = list(Inf)),
      makeIntegerLearnerParam(id = "maxdepth", default = Inf, lower = 0L, special.vals = list(Inf)),
      makeLogicalLearnerParam(id = "multiway", default = FALSE),
      makeIntegerLearnerParam(id = "splittry", default = 2L, lower = 1L),
      makeLogicalLearnerParam(id = "majority", default = FALSE),
      makeLogicalLearnerParam(id = "caseweights", default = TRUE),
      makeFunctionLearnerParam("applyfun", default = NULL, special.vals = list(NULL)),
      makeIntegerLearnerParam(id = "cores", default = NULL, lower = 1L, tunable = FALSE, special.vals = list(NULL))
    ),
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", "ordered", "prob", "weights"),
    name = "Conditional Inference Trees",
    short.name = "ctree",
    note = "Devel partykit package: https://r-forge.r-project.org/scm/viewvc.php/pkg/devel/?root=partykit"
  )
}

#' @export
trainLearner.classif.newctree = function(.learner, .task, .subset, .weights, 
                                      teststat, 
                                      splitstat, 
                                      splittest,
                                      testtype,
                                      nmax, 
                                      alpha, 
                                      mincriterion, 
                                      logmincriterion, 
                                      minsplit, 
                                      minbucket, 
                                      minprob, 
                                      stump, 
                                      lookahead,
                                      nresample, 
                                      MIA,
                                      maxsurrogate, 
                                      mtry, 
                                      maxdepth, 
                                      multiway, 
                                      splittry, 
                                      majority, 
                                      caseweights, 
                                      applyfun, 
                                      cores, ...) {
  
  ctrl = learnerArgsToControl(partykit::ctree_control, 
                              teststat, 
                              splitstat, 
                              splittest,
                              testtype,
                              nmax, 
                              alpha, 
                              mincriterion, 
                              logmincriterion, 
                              minsplit, 
                              minbucket, 
                              minprob, 
                              stump, 
                              lookahead,
                              nresample, 
                              MIA,
                              maxsurrogate, 
                              mtry, 
                              maxdepth, 
                              multiway, 
                              splittry, 
                              majority, 
                              caseweights, 
                              applyfun, 
                              cores)
  f = getTaskFormula(.task)
  partykit::ctree(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, ...)
}

#' @export
predictLearner.classif.newctree = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "response")
    p = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  else
    p = predict(.model$learner.model, newdata = .newdata, type = "prob", ...)
  
  return(p)
  
}



registerS3method("makeRLearner", "newctree", makeRLearner.classif.newctree)
registerS3method("trainLearner", "newctree", trainLearner.classif.newctree)
registerS3method("predictLearner", "newctree", predictLearner.classif.newctree)


# tr <- partykit::ctree(Species ~ ., data = iris)
# 
# lrn = makeLearner("classif.newctree", predict.type = "prob", 
#                            fix.factors.prediction = TRUE)
# task = makeClassifTask(id = "tutorial", data = iris, target = "Species")
# 
# # undebug(partykit:::.urp_tree)
# mod = train(lrn, task)
# 
# rdesc = makeResampleDesc(method = "CV", stratify = TRUE)
# r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE)
