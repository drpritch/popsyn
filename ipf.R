#
# Dense array-based IPF routine.
#

source('collapse.R')
source('IpfConstraint.R')
source('margin.table2.R')

# A simple Iterative Proportional Fitting algorithm. The input is
# 1) a list of marginal constraints, expressed as marginal contingency
#    tables (wrapped in an IpfConstraint data structure).
#
#    e.g., constraintList = list(
#             IpfConstraint(scalar), # 0D constraint
#             IpfConstraint(income), # 1D constraint - cannot be defined
#                                    # inline as an array, due to
#                                    # shortcomings in array() method.
#             # age by income crosstab
#             IpfConstraint(array(data = c(...),
#                                   dimnames=list(age = c(...),
#                                                 income = c(...))))
#
# 2) an optional array of priorArray cell values, containing all factor names
#    and levels. Zeros are treated as structural zeros and will remove
#    degrees of freedom from the fit.
# 3) an optional list of dimension names (as in array dimnames) for the
#    output array. A priorArray of all ones is assumed if this is present.
# 4) an iteration limit.
# 5) a tolerance for fitting
#
# NA values are legal in the margins, and will result in no constraint
# being enforced whatsoever.
# TODO: while they are accepted, they slow convergence substantially.
# I could use any 0D constraint to act simultaneously on all NA values to
# greatly speed up convergence.
#
# While it is possible to do some types of IPF using log-linear models
# (e.g., loglm or glm), those approaches have their limits. In particular,
# they cannot be used to do estimation on certain indirect models
# (e.g., XY,YZ,XZ), which we happen to need.
ipf <- function(constraintList, priorArray = NULL, dimnameList = NULL,
                maxIterations = 40, tolerance = 1/10,
                statistics = FALSE, verbose = FALSE) {
    assert(is.list(constraintList))
    lapply(constraintList, function(x) { assert(is.IpfConstraint(x)) })

    # Just a safety check for valid args.
    if(is.null(priorArray)) {
        assert(!is.null(dimnameList))
    } else {
        assert(is.array(priorArray))
        assert((!is.null(priorArray) && priorArray >= 0) && is.null(dimnameList))
        # Use order of dimensions from priorArray, if given.
        dimnameList <- dimnames(priorArray)
    }
    # Automatically do garbage collection if more than 50MB needed for
    # array.
    bigData <- prod(unlist(lapply(dimnameList, length)))*8 > 1024*1024*50
    if( bigData ) gc()

    ncon <- length(constraintList)

    # a list containing two arrays that represent the range that the
    # constraint can take.
    constraintList <- lapply(constraintList, function(con) {
        if(is.null(con$.min)) {
            con$.min <- con$.data
        }
        if(is.null(con$.max)) {
            con$.max <- con$.data
        }
        con
    } )

    # Find 0D constraints
    zerocon <- unlist(lapply(constraintList, function(x)
        { !is.array(x$.data) }))

    
    assert(!duplicated(names(dimnameList)))
    ndim <- length(dimnameList)
    if( is.null(priorArray) )
    {
        # Make a priorArray filled with ones
        makePriorArray <- function(dimnameList) {
            dimnameList <- dimnameList[
                !(duplicated(dimnameList) & duplicated(names(dimnameList)))]
            # Ensure that we don't have the same variable with different
            # numbers of levels.
            assert(length(dimnameList) == length(unique(names(dimnameList))))
            array(data = rep(1, prod(unlist(lapply(dimnameList, length)))),
                  dim = unlist(lapply(dimnameList, length)),
                  dimnames = dimnameList)
        }

        priorArray <- makePriorArray(dimnameList)
    }

    assert(priorArray>=0)
    result <- priorArray
    assert(names(dimnames(result)) == names(dimnameList))

    # Verify that the dimension names (and levels) are okay.
    lapply(constraintList[!zerocon], function(con) {
        for(d in 1:length(dimnames(con$.data))) {
            name <- names(dimnames(con$.data))[[d]]
            levels_full <- dimnames(result)[[name]]
            assert(!is.na(levels_full))
            levels_con <- dimnames(con$.data)[[d]]
            levelmap <- con$.levelmapList[[name]]
            if(is.null(levelmap)) {
                # It's not a collapsed/subset of the levels. So, verify
                # that the levels match up one-to-one with the big array.
                assert(levels_con == levels_full)
            } else {
                # It's a collapsed or subset of the levels. Verify that the
                # map is okay, and that the precollapse levels correspond to
                # the big array.
                assert(length(levels_con) == length(levelmap$.levels))
                assert(length(levelmap$.map) == length(levels_full))
            }
        }
    })

    # Retrieve the dimension numbers associated with each constraint.
    # Result is a list of vectors, one vector per constraint.
    # 0D constraintList will get a 0.
    conmargin <- lapply(constraintList, function(x)
        { match(names(dimnames(x$.data)), names(dimnameList)) } )
    # Complement - dimensions not associated with constraint
    conmarginc <- lapply(conmargin, function(x)
        { setdiff(1:length(dimnameList),x) })

    if(length(result) > 10000) {
        # We can't afford the memory to save a backup copy of the entire
        # result. Just save a random sample instead.
        set.seed(length(result))
        testpoints <- round(runif(10000,1,length(result)))
    } else {
        testpoints = 1:length(result)
    }

    for(iter in 1:maxIterations) {
        if(bigData) print(iter)
        condeltas <- NULL

        oldresult <- result[testpoints]
        for(j in 1:ncon) {
            #print(iter+j/100)
            if( bigData ) gc()

            scale2 <- result
            # First, collapse levels if necessary.
            # TODO: perhaps this should be after taking margin...
            if(!is.null(constraintList[[j]]$.levelmapList)) {
                scale2 <- collapseList.array(scale2,
                                             constraintList[[j]]$.levelmapList)
                if( bigData ) gc()
            }
            # Second, take the margin of the collapsed table along the
            # appropriate variables.
            # TODO: this one line is the slowest part of the entire
            # routine.
            scale2 <- margin.table2(scale2, conmargin[[j]])

            scale1 <- constraintList[[j]]$.min / scale2
            scale2 <- constraintList[[j]]$.max / scale2
            # NaN (0/0) or Inf (x/0) can both just leave the result as 1.0
            # Actually, Inf means that we'll never be able to meet that
            # constraint... we might want to do something about that.
            scale1 <- ifelse(is.nan(scale1) | is.infinite(scale1), 1, scale1)
            scale2 <- ifelse(is.nan(scale2) | is.infinite(scale2), 1, scale2)
            assert(!is.nan(scale1) & !is.infinite(scale1))
            assert(!is.nan(scale2) & !is.infinite(scale2))
            # If the margin is below the minimum (or above the maximum),
            # scale to equal the minimum (or maximum). If it's within the
            # min/max range, leave unchanged by scaling by 1.0.
            scale <- ifelse(scale1 > 1, scale1,
                            ifelse(scale2 < 1, scale2, 1))

            # Uncollapse according to map.
            if(!is.null(constraintList[[j]]$.levelmapList)) {
                scale <- uncollapseList.array(scale,
                    constraintList[[j]]$.levelmapList,
                    fullArray = result)
                if( bigData ) gc()
            }
            assert(scale>=0)

            #zz.n <- ls(); zz.s <- rep(0, length(zz.n))
            #for(i in seq(zz.n)) { zz.s[i] <- object.size(get(zz.n[i])) }
            #memusage <- data.frame(object = zz.n, size = zz.s)[order(-zz.s),]
            #print(memusage[1:10,])

            # Expand to full array, but with dimensions in wrong order.
            # TODO: this can be very space-inefficient...
            delta <- array(data = scale-1, # rely on array() to recycle data.
                           dim=unlist(lapply(dimnameList[c(conmargin[[j]],
                                             conmarginc[[j]])], length)),
                           dimnames=dimnameList[c(conmargin[[j]],
                                                  conmarginc[[j]])])
            # Return to the normal dimension order
            delta <- aperm(delta, match(1:ndim,
                                        c(conmargin[[j]], conmarginc[[j]])))
            assert(names(dimnames(delta)) == names(dimnameList))
            delta <- result * delta
            condeltas <- c(condeltas, max(abs(delta)))
            result <- result + delta
            remove(delta)

            assert(result>=0)
        }
        netdelta <- max(abs(result[testpoints] - oldresult))

        # Two termination criteria:
        # 1) If no constraint is individually changing the result by more
        # than the tolerance, stop.
        # 2) If the net effect of all constraintList in one iteration is below
        # the tolerance, stop. (This handles the case where the constraintList
        # are fighting with one another, but have reached a stable
        # equilibrium.)
        #print(condeltas)
        #cat('Max net delta = ', netdelta, '\n')
        if(max(condeltas) < tolerance || netdelta < tolerance) {
            break
        }
    }
    cat('# of iter:', iter, '\n')
    result <- list(
        fitted.values = result,
        numIterations = iter,
        params = list(
            maxIterations = maxIterations,
            tolerance = tolerance)
        )
    class(result) <- 'ipf'
    result$total.abs.err <- c()
    for(j in 1:ncon) {
        if( bigData ) gc()

        scale2 <- result$fitted.values
        # First, collapse levels if necessary.
        # TODO: perhaps this should be after taking margin...
        if(!is.null(constraintList[[j]]$.levelmapList)) {
            scale2 <- collapseList.array(scale2,
                                         constraintList[[j]]$.levelmapList)
            if( bigData ) gc()
        }
        # Second, take the margin of the collapsed table along the
        # appropriate variables.
        # TODO: this one line is the slowest part of the entire
        # routine.
        scale2 <- margin.table2(scale2, conmargin[[j]])
        result$total.abs.err <- c(result$total.abs.err,
            c(sum(pmax(constraintList[[j]]$.min - scale2, 0) +
                  pmax(scale2 - constraintList[[j]]$.max, 0))))
        names(result$total.abs.err)[[length(result$total.abs.err)]] <-
            paste(names(dimnames(scale2)), collapse = ':')
    }
    result$total.abs.err <- as.data.frame(result$total.abs.err)
    scale <- sum(priorArray) / sum(result$fitted.values)
    result$relative.entropy <-
        sum(ifelse(result$fitted.values==0, 0,
                   (result$fitted.values / sum(result$fitted.values))
                   * log2(scale * result$fitted.values/priorArray)))
    scale <- 1 / sum(result$fitted.values)
    result$absolute.entropy <-
        -sum(ifelse(result$fitted.values==0, 0,
                    (result$fitted.values / sum(result$fitted.values))
                    * log2(scale * result$fitted.values)))
    if(verbose) {
        cat('Information Entropy: ', result$absolute.entropy, '\n')
        cat('Total Absolute Error against each constraint: \n')
        print(round(result$total.abs.err, 2))
        cat('Relative Entropy: ', result$relative.entropy, '\n')
    }
    if(statistics) {
        return(result);
    } else {
        return(result$fitted.values);
    }
}
