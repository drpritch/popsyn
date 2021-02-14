#
# Sparse list-based version of IPF, closely following ipf.R
#

source('collapse.R')
source('IpfConstraint.R')

filename <- paste('ipf_list', .Platform$dynlib.ext, sep="")
if(class(try(dyn.load(filename), silent=TRUE)) == 'try-error') {
    stop(paste('Could not load', filename, '.'))
}
remove(filename)

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
# 2) a data frame of population observations. (Almost) All of the
#    variables included in the constraints should appear as columns in the
#    data frame. Each row represents an observation to fill into the prior
#    table for the IPF procedure. (i.e., the prior table is equal to the
#    cross-tabulation of the population by the constraint variables.)
#    Unlike the traditional IPF procedure, the output of this IPF procedure
#    is a set of *weights* on the original individual observations. This is
#    essentially equivalent, however.
#
#    If all variables are present in the population, the output of this
#    procedure is a similar data frame, but with a new "weight" column. If
#    *one* variable is absent, the "weight" entry in the data frame becomes
#    an array, with one column per level of the missing variable. This is
#    typically used when matching a PUM (prior population) with geographic
#    data (the one missing variable)
#
# 3) a definition of what the prior for IPF should be:
#    a) 'frompop' - equivalent to the array-based IPF when the prior is set
#       to the cross-classification of popFrame.
#    b) 'none' - equivalent to the array-based IPF when the prior is set to
#       a constant value (i.e., 1).
#    c) 'fromweight' - use existing weights to define prior.
# 4) an iteration limit.
# 5) a tolerance for fitting
#
# Example output structure when one four-level variable is missing:
#
#    hlosp   sexp   agep   occ81p   weight.1 weight.2 weight.3 weight.4
#  1   A      M     20-24  B        0.5      2        1.3      1.6
#  2   B      F     15-19  A        0        0        4        0.1
#
# Note that "weight" is actually an array in the result, of dimension
# nrow x 4
#

#
# TODO: handle array-only constraint
#
ipf_list <- function(constraintList, popFrame, priorAssoc = NULL,
                maxIterations = 40, tolerance = 1/10,
                statistics = FALSE, verbose = TRUE) {
    startTime <- proc.time()
    assert(is.list(constraintList))
    assert(is.data.frame(popFrame))
    dn_list <- colnames(popFrame)
    dn_list <- setdiff(dn_list, c('weight'))
    # All columns (except weight and id) must be factors.
    assert(lapply(popFrame[,setdiff(colnames(popFrame), c('weight'))],
                  is.factor))

    result <- popFrame
    lapply(constraintList, function(x) { assert(is.IpfConstraint(x)) })

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
    
    dn_array <- NULL
    # Verify that the dimension names (and levels) are okay.
    for(i in seq(constraintList[!zerocon])) {
        con <- constraintList[!zerocon][[i]]
        for(d in seq(dimnames(con$.data))) {
            name <- names(dimnames(con$.data))[[d]]
            levels_full <- levels(result[,name])
            levels_con <- dimnames(con$.data)[[d]]
            levelmap <- con$.levelmapList[[name]]
            if(is.null(levels_full)) {
                # Having a map on the new dimension is not supported yet.
                assert(is.null(levelmap))
                # Save and continue
                levels_new <- levels_con
                dn_array <- unique(c(dn_array, name))
            } else {
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
        }
    }
    if(priorAssoc == 'fromweight') {
        assert('weight' %in% colnames(result))
        # Force to numeric type (in case input is integer)
        if(is.array(result$weight)) {
            result$weight <- array(as.numeric(result$weight),
                dim=dim(result$weight), dimnames = dimnames(result$weight))
        } else {
            result$weight <- as.numeric(result$weight)
        }
    } else if(priorAssoc == 'none') {
        n <- unique(unlist(lapply(constraintList[!zerocon], function(con) {
            names(dimnames(con$.data))
        })))
        if(!is.null(dn_array)) {
            n <- n[n != dn_array]
        }
        # Set initial weights, if not present.
        result$weight <- ipf_initListWeights(popFrame[,n])
    } else if(priorAssoc == 'frompop') {
        result$weight <- 1
    } else {
        assert(priorAssoc %in% c('none', 'frompop', 'fromweight'))
    }

    # Carefully do this *prior* to expansion, to save memory.
    # priorWeight <- result$weight

    if(!is.null(dn_array)) {
        gc()
        assert(length(dn_array) == 1)
        print(paste('Adding new dimension', dn_array))
        # This is now a hybrid: an array-within-a-list.
        result$weight <- array(
            rep(result$weight, length(levels_new)),
            dim=c(nrow(result), length(levels_new)),
            dimnames=list(NULL, levels_new))
        names(dimnames(result$weight))[[2]] <- dn_array
        remove(levels_new)
    }

   if(length(result$weight) > 10000) {
        # We can't afford the memory to save a backup copy of the entire
        # result. Just save a random sample instead.
        set.seed(length(result$weight))
        testpoints <- round(runif(10000,1,length(result$weight)))
    } else {
        testpoints = seq(result$weight)
    }

    bigData <- length(result$weight)*8 > 1024*1024*50

    assert(length(dn_array) < 2)
    for(iter in 1:maxIterations) {
        #print(iter)
        condeltas <- NULL

        oldweight <- result$weight[testpoints]
        for(j in seq(constraintList)) {
            #print(iter+j/1000)
            if(bigData) gc()

            # Take margin of data along constraint dimensions.
            c_dn <- names(dimnames(constraintList[[j]]$.data))
            scale0 <- margin.list(result, c_dn)
            scale2 <- scale0
            # Collapse levels if necessary.
            if(!is.null(constraintList[[j]]$.levelmapList)) {
                scale2 <- collapseList.array(scale0,
                                             constraintList[[j]]$.levelmapList)
            }
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
                    fullArray = scale0)
            }
            assert(scale>=0)

            if(bigData) gc()
            if(zerocon[j]) {
                assert(length(scale) == 1)
                assert(scale>=0)
                maxDelta <- max(abs(result$weight) * scale)
                result$weight <- result$weight * scale
            }
            else {
                # Until here, scale has been an array containing just the
                # variables in the constraint.
                # Now, convert to the format of a column in result.
                #
                # In the very early days, this was done using this old (and
                # slow) approach:

                # expanded <- merge(ipf_list,
                #      as.data.frame.table(collapsed, responseName='...scale'),
                #      all.Y = FALSE, sort = FALSE)
                # expanded <- expanded$...scale[order(expanded$id)]
                #
                # ... which doesn't support "array" type dimensions.
                #
                # Later, it was made into a function. However, R's
                # call-by-value semantics made this highly inefficient when
                # the result became large (e.g., 300MB), so it was inlined
                # here.

                c_dn_list <- intersect(c_dn,
                    setdiff(colnames(result), 'weight'))
                if(length(dim(result$weight)) == 2) {
                    c_dn_array <- intersect(c_dn,
                        names(dimnames(result$weight))[[2]])
                } else {
                    c_dn_array <- c()
                }
                # TODO: handle case where constraint has *just* array dim.
                assert(length(c_dn_list) > 0)

                # Drop all columns from result that don't show up in the
                # collapsed table.
                callList <- result[, c_dn_list, drop=FALSE]
                assert(is.data.frame(callList))
                # Convert factors to integers, and convert to an array.
                callList <- array(
                    unlist(lapply(callList, as.integer)),
                    dim = dim(callList),
                    dimnames = dimnames(callList)
                )

                # WARNING:
                # Take care with this C function - result$weight is
                # modified *in-place* for maximum efficiency with large
                # datasets, completely counter to all normal conventions in
                # R.
                maxDelta <- .Call('ipfExpandToList',
                    result$weight,
                    callList,
                    # Reorder dimensions on collapsed table, such that "list"
                    # (addressing) dimensions come first, and array dimensions
                    # come after.
                    aperm(scale, c(match(c_dn_list, c_dn),
                                   match(c_dn_array, c_dn))),
                    if(length(c_dn_array) > 0) 1 else 0
                )
            }
            condeltas <- c(condeltas, maxDelta)
            remove(scale)

            assert(result$weight>=0)
        }
        netdelta <- max(abs(result$weight[testpoints] - oldweight))

        # Two termination criteria:
        # 1) If no constraint is individually changing the result by more
        # than the tolerance, stop.
        # 2) If the net effect of all constraintList in one iteration is below
        # the tolerance, stop. (This handles the case where the constraintList
        # are fighting with one another, but have reached a stable
        # equilibrium.)
        #print(condeltas)
        cat('Max net delta = ', netdelta, '\n')
        if(max(condeltas) < tolerance || netdelta < tolerance) {
            break
        }
    }
    if(bigData) gc()
    cat('# of iter:', iter, '\n')
    result <- list(
        fitted.values = result,
        numIterations = iter,
        elapsedTime = proc.time() - startTime,
        params = list(
            priorAssoc = priorAssoc,
            maxIterations = maxIterations,
            tolerance = tolerance,
            constraintDimNames =
                lapply(constraintList, function(x) { dimnames(x$.data)}),
            # Note that this is definitely *not* the correct number of
            # parameters. That's way too hard to calculate. Instead, this
            # is an *upper bound* on the number of parameters.
            numParams = 
                sum(unlist(lapply(constraintList,
                                  function(x) { prod(dim(x$.data))})))
        ))
    if(bigData) gc()
    class(result) <- 'ipf_list'
    result$errorstats <- ipf_errorstats_list(result$fitted.values,
                                               constraintList)
    if(verbose) {
        cat('Total Absolute Error and Standardized RMS Error*1000 against each constraint: \n')
        temp <- result$errorstats
        temp$srmse <- temp$srmse * 1000
        print(round(temp, 1))
    }
    if(bigData) gc()
    #scale <- sum(priorWeight) / sum(result$fitted.values$weight)
    #if( length(result$fitted.values$weight) == length(priorWeight) ) {
    #    result$relative.entropy <-
    #        sum(ifelse(result$fitted.values$weight==0, 0,
    #                   (result$fitted.values$weight /
    #                    sum(result$fitted.values$weight))
    #                   * log2(scale * result$fitted.values$weight /
    #                          priorWeight)))
    #} else {
    #    assert(dim(result$fitted.values$weight)[1] == length(priorWeight))
    #    result$relative.entropy <- 0
    #    scale <- scale * dim(result$fitted.values$weight)[2]
    #    # Do one column at a time, in an attempt to save memory!
    #    for(i in seq(dim(result$fitted.values$weight)[2])) {
    #        result$relative.entropy <- result$relative.entropy +
    #            sum(ifelse(result$fitted.values$weight[,i]==0, 0,
    #                       (result$fitted.values$weight[,i] / 
    #                        sum(result$fitted.values$weight))
    #                       * log2(scale * result$fitted.values$weight[,i] /
    #                              priorWeight)))
    #    }
    #}
    result$absolute.entropy <-
        .Call('absEntropyList', result$fitted.values$weight)
    if(verbose) {
        cat('Information Entropy: ', result$absolute.entropy, '\n')
        #cat('Relative Entropy: ', result$relative.entropy, '\n')
        cat('Number of Parameters: ', result$params$numParams, '\n')
    }
    result$completeTime <- proc.time() - startTime
    if(statistics) {
        return(result)
    } else {
        return(result$fitted.values)
    }
}

ipf_errorstats_list <- function(fitted.values_list, constraintList) {
    # TODO: assert inputs.
    assert(is.list(constraintList))
    lapply(constraintList, function(x) { assert(is.IpfConstraint(x)) })
    assert(is.data.frame(fitted.values_list))

    constraintList <- lapply(constraintList, function(con) {
        if(is.null(con$.min)) {
            con$.min <- con$.data
        }
        if(is.null(con$.max)) {
            con$.max <- con$.data
        }
        con
    } )

    result <- NULL
    for(j in seq(constraintList)) {
        gc()
        # Take margin of data along constraint dimensions.
        c_dn <- names(dimnames(constraintList[[j]]$.data))
        marginj <- margin.list(fitted.values_list, c_dn)
        # Collapse levels if necessary.
        if(!is.null(constraintList[[j]]$.levelmapList)) {
            marginj <-
                collapseList.array(marginj, constraintList[[j]]$.levelmapList)
        }
        delta <- pmax(constraintList[[j]]$.data - marginj, 0) +
                 pmax(marginj - constraintList[[j]]$.data, 0)
        newrow <- data.frame(tae = sum(delta))
        newrow$srmse <- sqrt(sum(delta^2) / sum(constraintList[[j]]$.data > 0)) /
                             (sum(marginj) / sum(constraintList[[j]]$.data > 0))
        newrow$srmse_old<- sqrt(sum(delta^2) / length(marginj)) /
                                (sum(marginj) / length(marginj))
        # Handling zeros in the constraint: take the maximum allowed value
        # (either 4 or 39) and take the middle of that range as the
        # ``expected'' value of some sort.
        constraintj <- constraintList[[j]]$.data
        sumconj <- sum(constraintj)
        marginj <- marginj / sum(marginj)
        constraintj <- constraintj / sumconj
        newrow$relent <- 
            sum(ifelse(constraintj==0, 0,
               constraintj * log(constraintj / marginj)))
        newrow$mdi <- newrow$relent * sumconj * 2
        newrow$mdi_null <- sum(ifelse(constraintj==0, 0,
            constraintj * log(constraintj / (1/sum(constraintj>0))))) *
            sumconj * 2
        newrow$phi <- sum(ifelse(constraintj==0, 0,
               constraintj * abs(log(constraintj / marginj))))
        sj <- (marginj + constraintj) / 2
        newrow$psibar <- 
            sum(ifelse(constraintj==0, 0,
                       constraintj * abs(log(constraintj/sj))) +
                ifelse(marginj==0, 0,
                       marginj * abs(log(marginj/sj))))

        newrow$name <- 
            paste(paste(c_dn, collapse = ':'), '(', length(marginj), ')')

        result <- rbind(result, newrow)
    }
    # Drop non-unique rows
    result <- result[!duplicated(result$name),]
    rownames(result) <- result$name
    result$name <- NULL
    result
}

ipf_initListWeights <- function(popFrame) {
    assert(is.data.frame(popFrame))

    # Add temporary row id
    popFrame$xxidxx <- seq(nrow(popFrame))
    # Sort frame - this will put records that belong to the same "cell"
    # next to each other.
    popFrame <- popFrame[do.call(order, popFrame), ]
    # Find duplicates - the first in each cell will now have "FALSE" and
    # the others in the cell will have "TRUE". (We exclude the new xxidxx
    # column from the duplicate testing)
    dup <- duplicated(popFrame[, -ncol(popFrame)])
    # Cumulative sum of !dup will set all rows that belong to the same cell
    # to the same value
    # e.g.,   F T T T F F T
    # becomes 1 1 1 1 2 3 3
    cell <- cumsum(!dup)
    # Build a frequency table of the cells, and map that back - i.e.,
    # convert from the cell "id" to the count of records in that cell. The
    # above example thus becomes
    # 4 4 4 4 1 2 2
    count <- as.vector(table(cell)[cell])
    # Finally, invert to get the weights.
    # Also, reorder back to the original order of records at input (before
    # the sort)
    result <- 1/count[order(popFrame$xxidxx)]
    assert(!is.array(result))

    assert(length(result) == nrow(popFrame))
    result
}

# Convert an array dimension to a list dimension.
# This is a bit ugly, but sometimes worthwhile. It's achieved by making
# many copies of the population frame: N copies are made of each row, where
# N is the number of levels in the array dimension.
#
# For large N, this is not advisable - the memory consumption will leap
# dramatically. For small N, it may be a good idea. Ideally, the user
# wouldn't have to keep track of the array/list dimension distinction - but
# at the moment, the tabulation method (xtabs) doesn't fully support array
# dimensions, and the IPF routine itself can only handle one array
# dimension.
ipf_list_arrayDimToListDim <- function(popFrame) {
    assert(is.data.frame(popFrame))
    assert(is.array(popFrame$weight) && length(dim(popFrame$weight) == 2))
    newColId <- ncol(popFrame) + 1
    numLevels <- dim(popFrame$weight)[2]
    result <- NULL
    for(i in seq(numLevels)) {
        temp <- popFrame
        temp$weight <- popFrame$weight[, i]
        temp[,newColId] <- i
        result <- rbind(result, temp)
    }
    colnames(result)[newColId] <- names(dimnames(popFrame$weight))[[2]]
    result[,newColId] <- factor(result[, newColId],
        levels=seq(numLevels),
        labels=dimnames(popFrame$weight)[[2]])
    result
}

montecarlo_condition_list <- function(fitGeogNew, popGiven) {
    # We should have ctcode as the array dim in both populations.
    assert(length(dim(fitGeogNew$weight)) == 2)
    assert(names(dimnames(fitGeogNew$weight))[[2]] %in% colnames(popGiven))
    n1 <- setdiff(colnames(fitGeogNew), c('id', 'weight'))
    n2 <- setdiff(colnames(popGiven), c('id', 'weight'))

    gc()
    givenDim <- intersect(n1,n2)
    newDim <- setdiff(n2,n1)
    print(paste('Conditioning on common variables',
                paste(c(givenDim,
                        names(dimnames(fitGeogNew$weight))[[2]]),
                      collapse=',')))

    # We want to compute the probability of the new dimensions newDim
    # given the old dimensions givenDim
    #
    # P(NEW|GIVEN) = P(GIVEN,NEW) / P(GIVEN)
    #
    # We have the joint counts n*P(GIVEN,NEW) right now. We need to calculate
    # a 1/(n*P(GIVEN)) table in the same format, and then multiply them to get
    # the conditional probabilities.

    # Keep only givenDims.
    fitGeogNewCall <- fitGeogNew[, givenDim, drop=FALSE]
    assert(is.data.frame(fitGeogNew))

    dn_collapsed <- lapply(fitGeogNewCall, levels)
    dn_collapsed <- c(dn_collapsed,
                      list(dimnames(fitGeogNew$weight)[[2]]))
    names(dn_collapsed)[length(dn_collapsed)] <-
        names(dimnames(fitGeogNew$weight))[[2]]
    # Convert factors to integers, and convert to an array.
    fitGeogNewCall <- array(
        # Drop id column.
        unlist(lapply(fitGeogNewCall, as.integer)),
        dim = dim(fitGeogNewCall),
        dimnames = dimnames(fitGeogNewCall)
    )

    scale <- array(
        .Call(
            'ipfCollapseFromList',
            fitGeogNewCall,
            fitGeogNew$weight,
            unlist(lapply(dn_collapsed, length)),
            1
        ),
        dim = unlist(lapply(dn_collapsed, length)),
        dimnames = dn_collapsed)
    dn_collapsed <- NULL

    # 2) Convert to 1/(n*P())
    scale <- 1 / scale
    scale <- ifelse(is.infinite(scale) | is.nan(scale), 0, scale)

    # 3) Re-expand.
    weightCond <- fitGeogNew$weight
    # force copy
    weightCond[1,1] <- 0
    weightCond[1,1] <- fitGeogNew$weight[1,1]

    # WARNING:
    # Take care with this C function - weightCond is
    # modified *in-place* for maximum efficiency with large
    # datasets, completely counter to all normal conventions in
    # R.
    #
    # (This is the rationale for the "Force copy" above - to ensure that we
    # don't modify the original.)
    .Call('ipfExpandToList', weightCond, fitGeogNewCall, scale, 1)

    fitGeogNewCallIds <- as.integer(fitGeogNew$id)
    # Drop non-given dimensions, including weight and id.
    fitGeogNewCall <- fitGeogNew[, givenDim, drop=FALSE]
    fitGeogNewCall <- array(
        unlist(lapply(fitGeogNewCall, as.integer)),
        dim = dim(fitGeogNewCall),
        dimnames = dimnames(fitGeogNewCall)
    )
    # Look up the appropriate columns in popGiven
    popGivenCall <- popGiven[, givenDim, drop=FALSE]
    popGivenCall <- array(
        unlist(lapply(popGivenCall, as.integer)),
        dim = dim(popGivenCall),
        dimnames = dimnames(popGivenCall)
    )
    result <- .Call(
        'montecarloConditionList',
        # ids of each row in list
        fitGeogNewCallIds,
        # Given dimension levels of each row in list. Sorted by given
        # dimension.
        fitGeogNewCall,
        # Matrix - each row shows the conditional weight for the PUM
        # individual represented by the row, given the row's list dimension
        # levels. Each column within the row shows the conditional weight
        # for the individual given a different ctcode.
        weightCond,
        # Already synthesized population - their list dimension levels (for
        # lookup in fitGeogNewCall)
        popGivenCall,
        # Already synthesized population - their ctcodes.
        as.integer(popGiven$ctcode),
        runif(nrow(popGiven))
    )
    # Convert "not found" errors to NA.
    result <- ifelse(result < 0, NA, result)
    # Return factor if input was a factor.
    if(is.factor(fitGeogNew$id)) {
        result <- factor(result, levels=seq(nlevels(fitGeogNew$id)),
            labels=levels(fitGeogNew$id))
    }
}

# Equivalent of margin.table, but for list representation of table.
#
# Could be done using
#   xtabs(weight ~ dn1 + dn2, popList)
# (where dn=c('dn1', 'dn2')
# However, that's much much much much slower and memory hungry.
# Also, it requires special treatment of the array dimension in popList
# (i.e., the second dimension of weight) - xtabs always returns that array
# dimension as part of the result. The array dimension is returned without
# a name, and extra work is required to remove it if it's not wanted.
margin.list <- function(popList, dn)
{
    if(length(dn) == 1 && dn == 'weight') {
        return(margin.table2(popList$weight, 1))
    }
    assert(!('weight' %in% dn))
    dn_list <- intersect(dn, setdiff(colnames(popList), 'weight'))
    if(length(dim(popList$weight)) == 2) {
        dn_array <- intersect(dn, names(dimnames(popList$weight))[[2]])
    } else {
        dn_array <- c()
    }
    if(length(dn_list) == 0) {
        if(length(dn_array) == 0) {
            return(sum(popList$weight))
        } else {
            return(margin.table2(popList$weight, 2))
        }
    }
    # Drop all columns from result that don't show up in the
    # collapsed table.
    callList <- popList[, dn_list, drop=FALSE]
    assert(is.data.frame(callList))
    dn_collapsed <- lapply(callList, levels)
    if(length(dn_array) > 0) {
        dn_collapsed <- c(dn_collapsed,
                          list(dimnames(popList$weight)[[2]]))
        names(dn_collapsed)[length(dn_collapsed)] <- dn_array
    }
    # Convert factors to integers, and convert to an array.
    callList <- array(
        unlist(lapply(callList, as.integer)),
        dim = dim(callList),
        dimnames = dimnames(callList)
    )
    result <- array(
        .Call(
            'ipfCollapseFromList',
            callList,
            popList$weight,
            unlist(lapply(dn_collapsed, length)),
            if(length(dn_array) > 0) 1 else 0
        ),
        dim = unlist(lapply(dn_collapsed, length)),
        dimnames = dn_collapsed)
    result <- aperm(result, match(dn, names(dimnames(result))))
    result
}

