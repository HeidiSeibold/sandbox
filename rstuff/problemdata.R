load("problemdata.RData")
library("partyNG")

## get rid of variables with too many NAs
dat_nas <- is.na(dat)
n_nas <- colSums(dat_nas)
(frac_nas <- n_nas/NROW(dat))
datn <- dat[ , frac_nas < 0.2]

## drop unused levels
datn <- droplevels(datn)

summary(datn)
datn$product.type <- NULL

## compute trees
(tr1 <- partyNG::ctree(class ~ ., datn, 
                       control = ctree_control(teststat = "quadratic", 
                                               splitstat = "maximum", 
                                               maxdepth = 2)))
(tr2 <- partyNG::ctree(class ~ ., datn, 
                       control = ctree_control(teststat = "maximum", 
                                               splitstat = "quadratic", 
                                               maxdepth = 2)))

(ctr1 <- partykit::ctree(class ~ ., datn, 
                         control = partykit::ctree_control(teststat = "quad", 
                                                           maxdepth = 2)))
(ctr2 <- partykit::ctree(class ~ ., datn, 
                         control = partykit::ctree_control(teststat = "max", 
                                                           maxdepth = 2)))

# Problem with tr2 in doTest() mit
# ret <- .Call("R_MaxtypeTest", object, as.integer(alt), 
#              as.integer(pvalue), as.integer(lower), as.integer(log), 
#              as.integer(pargs$maxpts), as.double(pargs$releps), 
#              as.double(pargs$abseps), PACKAGE = "libcoin")
