## Ben Kite

##' Conducts random permutation test for mean difference between two groups.
##'
##' @title mean_permuter
##' @param dat Data frame with group variable and dv.
##' @param group Name of the grouping variable.
##' @param dv Name of the dependent variable.
##' @param nperms Number of permutation replications. Defaults to 1000.
##' @param alpha Type I error rate. Defaults to .05.
##' @return Results of permutation test, including a statement of what the rejection decision should be.
##' @examples
##' n <- 100
##' x <- rbinom(n, 1, .5)
##' y <- 5*x + rnorm(n)
##' dat <- data.frame(x, y)
##' mean_permuter(dat, "x", "y")
##' @author Ben Kite
mean_permuter <- function(dat, group, dv, nperms = 1000, alpha = .05){
    groupmeans <- aggregate(dat[,dv], by = list(dat[,group]), mean)
    teststat <- groupmeans[2, 2] - groupmeans[1, 2]
    refdis <- rep(NA, nperms)
    for (i in 1:nperms){
        dat[,group] <- sample(dat[,group], nrow(dat), replace = FALSE)
        diffs <- aggregate(dat[,dv], by = list(dat[,group]), mean)
        refdis[i] <- diffs[2, 2] - diffs[1, 2]
    }
    lower <- quantile(refdis, probs = alpha/2, names = FALSE)
    upper <- quantile(refdis, probs = 1 - alpha/2, names = FALSE)
    decision <- ifelse(teststat > upper, "reject null",
                ifelse(teststat < lower, "reject null", "fail to reject null"))
    data.frame("test value" = teststat,
               "lower crit" = lower,
               "upper crit" = upper,
               "decision" = decision
               )
}
