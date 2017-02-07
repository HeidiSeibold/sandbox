library("mlr")

### parameters used in ctree_control
# makeNumericLearnerParam(id = "mincriterion", default = 1 - alpha, lower = 0, upper = 1),
# makeNumericLearnerParam(id = "logmincriterion", default = log(mincriterion), lower = 0, upper = 1),
params = makeParamSet(
  makeDiscreteLearnerParam(id = "teststat", default = "quadratic", values = c("quadratic", "maximum")),
  makeDiscreteLearnerParam(id = "splitstat", default = "quadratic", values = c("quadratic", "maximum")),
  makeLogicalLearnerParam(id = "splittest", default = FALSE),
  makeDiscreteLearnerParam(id = "testtype", default = "Bonferroni", values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
  makeNumericLearnerParam(id = "nmax", default = Inf, lower = 0, allow.inf = TRUE),
  makeNumericLearnerParam(id = "alpha", default = 0.05, lower = 0, upper = 1),
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
  makeLogicalLearnerParam(id = "intersplit", default = FALSE),
  makeLogicalLearnerParam(id = "majority", default = FALSE),
  makeLogicalLearnerParam(id = "caseweights", default = TRUE),
  makeFunctionLearnerParam("applyfun", default = NULL, special.vals = list(NULL)),
  makeIntegerLearnerParam(id = "cores", default = NULL, lower = 1L, tunable = FALSE, special.vals = list(NULL))
)


#' ctree for classification
#' @export
makeRLearner.classif.develpartykit.ctree = function() {
  makeRLearnerClassif(
    cl = "classif.develpartykit.ctree",
    package = "partykit",
    par.set = params,
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", 
                   "ordered", "prob", "weights"),
    name = "Conditional Inference Trees",
    short.name = "ctree",
    note = "Devel partykit package revision 1078: https://r-forge.r-project.org/scm/viewvc.php/pkg/devel/?root=partykit"
  )
}


#' cforest for classification
#' @export
makeRLearner.classif.develpartykit.cforest = function() {
  makeRLearnerClassif(
    cl = "classif.develpartykit.cforest",
    package = "partykit",
    par.set = params,
    properties = c("twoclass", "multiclass", "missings", "numerics", "factors", 
                   "ordered", "prob", "weights"),
    name = "Conditional Inference Forest",
    short.name = "cforest",
    note = "Devel partykit package revision 1078: https://r-forge.r-project.org/scm/viewvc.php/pkg/devel/?root=partykit"
  )
}

#' ctree for regression 
#' @export
makeRLearner.regr.develpartykit.ctree = function() {
  makeRLearnerRegr(
    cl = "regr.develpartykit.ctree",
    package = "partykit",
    par.set = params,
    properties = c("missings", "numerics", "factors", "ordered", "weights"),
    name = "Conditional Inference Trees",
    short.name = "ctree",
    note = "Devel partykit package revision 1078: https://r-forge.r-project.org/scm/viewvc.php/pkg/devel/?root=partykit"
  )
}

#' cforest for regression
#' @export
makeRLearner.regr.develpartykit.cforest = function() {
  makeRLearnerRegr(
    cl = "regr.develpartykit.cforest",
    package = "partykit",
    par.set = params,
    properties = c("missings", "numerics", "factors", "ordered", "weights"),
    name = "Conditional Inference Forest",
    short.name = "cforest",
    note = "Devel partykit package revision 1078: https://r-forge.r-project.org/scm/viewvc.php/pkg/devel/?root=partykit"
  )
}

#' ctree for classification
#' @export
trainLearner.classif.develpartykit.ctree = function(.learner, .task, .subset, .weights, 
                                                    teststat, splitstat, splittest, 
                                                    testtype, nmax, alpha, mincriterion, 
                                                    logmincriterion, minsplit, minbucket, 
                                                    minprob, stump, lookahead, nresample, 
                                                    MIA, maxsurrogate, mtry, maxdepth, 
                                                    multiway, splittry, intersplit, 
                                                    majority, caseweights, applyfun, 
                                                    cores, ...) {
  
  ctrl = learnerArgsToControl(partykit::ctree_control, teststat, splitstat, 
                              splittest, testtype, nmax, alpha, mincriterion, 
                              logmincriterion, minsplit, minbucket, minprob, 
                              stump, lookahead, nresample, MIA, maxsurrogate, 
                              mtry, maxdepth, multiway, splittry, intersplit, 
                              majority, caseweights, applyfun, cores)
  f = getTaskFormula(.task)
  partykit::ctree(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, 
                  ...)
}

#' cforest for classification
#' @export
trainLearner.classif.develpartykit.cforest = function(.learner, .task, .subset, .weights, 
                                                    teststat, splitstat, splittest, 
                                                    testtype, nmax, alpha, mincriterion, 
                                                    logmincriterion, minsplit, minbucket, 
                                                    minprob, stump, lookahead, nresample, 
                                                    MIA, maxsurrogate, mtry, maxdepth, 
                                                    multiway, splittry, intersplit, 
                                                    majority, caseweights, applyfun, 
                                                    cores, ...) {
  
  ctrl = learnerArgsToControl(partykit::ctree_control, teststat, splitstat, 
                              splittest, testtype, nmax, alpha, mincriterion, 
                              logmincriterion, minsplit, minbucket, minprob, 
                              stump, lookahead, nresample, MIA, maxsurrogate, 
                              mtry, maxdepth, multiway, splittry, intersplit, 
                              majority, caseweights, applyfun, cores)
  f = getTaskFormula(.task)
  partykit::cforest(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, 
                  ...)
}


#' ctree for regression
#' @export
trainLearner.regr.develpartykit.ctree = function(.learner, .task, .subset, .weights, 
                                                 teststat, splitstat, splittest, 
                                                 testtype, nmax, alpha, mincriterion, 
                                                 logmincriterion, minsplit, minbucket, 
                                                 minprob, stump, lookahead, nresample, 
                                                 MIA, maxsurrogate, mtry, maxdepth, 
                                                 multiway, splittry, intersplit, 
                                                 majority, caseweights, applyfun, 
                                                 cores, ...) {
  
  ctrl = learnerArgsToControl(partykit::ctree_control, teststat, splitstat, 
                              splittest, testtype, nmax, alpha, mincriterion, 
                              logmincriterion, minsplit, minbucket, minprob, 
                              stump, lookahead, nresample, MIA, maxsurrogate, 
                              mtry, maxdepth, multiway, splittry, intersplit, 
                              majority, caseweights, applyfun, cores)
  f = getTaskFormula(.task)
  partykit::ctree(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, 
                  ...)
}


#' cforest for regression
#' @export
trainLearner.regr.develpartykit.cforest = function(.learner, .task, .subset, .weights, 
                                                 teststat, splitstat, splittest, 
                                                 testtype, nmax, alpha, mincriterion, 
                                                 logmincriterion, minsplit, minbucket, 
                                                 minprob, stump, lookahead, nresample, 
                                                 MIA, maxsurrogate, mtry, maxdepth, 
                                                 multiway, splittry, intersplit, 
                                                 majority, caseweights, applyfun, 
                                                 cores, ...) {
  
  ctrl = learnerArgsToControl(partykit::ctree_control, teststat, splitstat, 
                              splittest, testtype, nmax, alpha, mincriterion, 
                              logmincriterion, minsplit, minbucket, minprob, 
                              stump, lookahead, nresample, MIA, maxsurrogate, 
                              mtry, maxdepth, multiway, splittry, intersplit, 
                              majority, caseweights, applyfun, cores)
  f = getTaskFormula(.task)
  partykit::cforest(f, data = getTaskData(.task, .subset), control = ctrl, weights = .weights, 
                  ...)
}



#' ctree prediction classification
#' @export
predictLearner.classif.develpartykit.ctree = function(.learner, .model, .newdata, ...) {
  
  if (.learner$predict.type == "response") {
    p = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, type = "prob", ...)
  }
  return(p)
  
}


#' cforest prediction classification
#' @export
predictLearner.classif.develpartykit.cforest = function(.learner, .model, .newdata, ...) {
  
  if (.learner$predict.type == "response") {
    p = predict(.model$learner.model, newdata = .newdata, type = "response", ...)
  } else {
    p = predict(.model$learner.model, newdata = .newdata, type = "prob", ...)
  }
  return(p)
  
}


#' ctree prediction classification
#' @export
predictLearner.regr.develpartykit.ctree = function(.learner, .model, .newdata, ...) {
  
  p = predict(.model$learner.model, newdata = .newdata,  ...)
  return(p)
  
}


#' cforest prediction classification
#' @export
predictLearner.regr.develpartykit.cforest = function(.learner, .model, .newdata, ...) {
  
  p = predict(.model$learner.model, newdata = .newdata,  ...)
  return(p)
  
}





registerS3method("makeRLearner", "develpartykit.ctree", makeRLearner.classif.develpartykit.ctree)
registerS3method("trainLearner", "develpartykit.ctree", trainLearner.classif.develpartykit.ctree)
registerS3method("predictLearner", "develpartykit.ctree", predictLearner.classif.develpartykit.ctree)

registerS3method("makeRLearner", "develpartykit.cforest", makeRLearner.classif.develpartykit.cforest)
registerS3method("trainLearner", "develpartykit.cforest", trainLearner.classif.develpartykit.cforest)
registerS3method("predictLearner", "develpartykit.cforest", predictLearner.classif.develpartykit.cforest)

registerS3method("makeRLearner", "develpartykit.ctree", makeRLearner.regr.develpartykit.ctree)
registerS3method("trainLearner", "develpartykit.ctree", trainLearner.regr.develpartykit.ctree)
registerS3method("predictLearner", "develpartykit.ctree", predictLearner.regr.develpartykit.ctree)

registerS3method("makeRLearner", "develpartykit.cforest", makeRLearner.regr.develpartykit.cforest)
registerS3method("trainLearner", "develpartykit.cforest", trainLearner.regr.develpartykit.cforest)
registerS3method("predictLearner", "develpartykit.cforest", predictLearner.regr.develpartykit.cforest)


# tr <- partykit::cforest(Species ~ ., data = iris)
# 
# lrn = makeLearner("classif.develpartykit.cforest")
# task = makeClassifTask(id = "tutorial", data = iris, target = "Species")
# 
# # undebug(partykit:::.urp_tree)
# mod = train(lrn, task)
# 
# rdesc = makeResampleDesc(method = "CV", stratify = TRUE)
# r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE)
# 
# 
# airq <- subset(airquality, !is.na(Ozone))
# airct <- partykit::cforest(Ozone ~ ., data = airq)
# airct
# 
# lrn = makeLearner("regr.develpartykit.cforest")
# task = makeRegrTask(id = "tutorial", data = airq, target = "Ozone")
# mod = train(lrn, task)

