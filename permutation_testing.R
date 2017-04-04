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
    dat = dat[,c(group, dv)]
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



##' Conducts random permutation test for correlation between two variables.
##'
##' @param dat Data frame containing x and y.
##' @param x Variable to be shuffle to build reference distribution.
##' @param y Second variable in correlation calculation.
##' @param nperms Number of permutation replications. Defaults to 1000.
##' @param alpha Type I error rate. Defaults to .05.
##' @return Results of permutation test, including a statement of what the rejection decision should be.
##' @examples
##' n <- 100
##' x <- rnorm(n)
##' y <- rnorm(n)
##' dat <- data.frame(x, y)
##' cor_permuter(dat, "x", "y")
##' @author Ben Kite
cor_permuter <- function(dat, x, y, nperms = 1000, alpha = .05){
    dat = dat[,c(x, y)]
    teststat <- cor(dat[,x], dat[,y])
    refdis <- rep(NA, nperms)
    for (i in 1:nperms){
        dat[,x] <- sample(dat[,x], nrow(dat), replace = FALSE)
        refdis[i] <- cor(dat[,x], dat[,y])
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




##' Checks to determine if categorical variables have equal response categories observed across groups
##'
##' @param dat Data frame with grouping variable and categorical variables
##' @param group Grouping variable
##' @param catvars Vector of categorical variable names
##' @examples
##' n <- 100
##' x <- c(rep(1, n/4), rep(2, n/4), rep(3, n/4), rep(4, n/4))
##' y1 <- rbinom(n, 1, .2)
##' y2 <- rbinom(n, 1, .5)
##' dat <- data.frame(x, y1, y2)
##' categoryCheck(dat, "x", c("y1", "y2"))
##' @return TRUE or FALSE to indicate if all response categories are equal across groups.
##' @author Ben Kite
categoryCheck <- function(dat, group, catvars){
    groups <- as.character(unique(dat[,group]))
    unilist <- list()
    for (u in groups){
        tmpdat <- dat[which(dat[,group] == u),]
        unilist[[as.character(u)]] <- apply(tmpdat[,catvars], 2, function(x) sort(unique(x)))
    }
    differences <- matrix(NA, length(groups), length(groups))
    colnames(differences) <- groups
    rownames(differences) <- groups
    for (i in groups){
        for (j in groups){
            differences[i, j] <- identical(unilist[[i]], unilist[[j]])
        }
    }
    if(prod(differences) == 1){
        return(TRUE)
    } else {
        mismatches <- which(!differences, arr.ind = TRUE)
        for (m in 1:nrow(mismatches)){
            print(paste0("The categories are different between groups ", mismatches[m,1], " and ", mismatches[m,2]))
        }
        return(FALSE)
    }
}
