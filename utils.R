#
# Miscellaneous utility funcitons, a few of which are obsolete.
#

library('gtools')
library('RODBC')

source('IpfConstraint.R')
source('margin.table2.R')

# Take a 1D slice from a 2D array and return it as an array.
# Essentially the same as array2[index,] for dim=1 and array2[,index] for
# dim=2, but keeps it as an array instead of converting it to a vector.
#
# index: which row to keep (number or level name)
# dim: which dimension to discard (1=return all cols from one row, 2=return
# all rows from one col).
array1 <- function(array2, index=1, dim=1) {
    if(is.list(array2)) {
        return(lapply(array2, function(x) { array1(x, index, dim) }))
    }
    assert(is.array(array2) && length(dim(array2) == 2))
    result <- if(dim==1) {
        array(data = array2[index,])
    } else {
        array(data = array2[,index])
    }
    dimnames(result) <- dimnames(array2)[3-dim]
    return(result)
}

# Take an arbitrary slice from a high-dimensional array.
#
# e.g.,
# aslice(arr, list(sexp = 'Male'))
# aslice(arr, list(1:5, 1:2, 1:7, NA))
#
# TODO: probably doesn't handle negative indices correctly.
aslice <- function(arr, slice, drop=TRUE) {
    assert(is.array(arr))
    assert(is.list(slice))
    
    if(!is.null(names(slice))) {
        # If we have a list of dimension names and values within that
        # dimension to keep in the slice, convert it into a vector showing
        # which indices to retain from each dimension of the array.
        slice <- lapply(
            match(names(dimnames(arr)), names(slice)),
            function(x) {
                if(is.na(x)) { x }
                else {
                  slice[[x]]
                }
            }
        )
    }
    names(slice) <- names(dimnames(arr))

    assert(length(slice) == length(dim(arr)))

    if(!is.null(names(dimnames(arr)))) {
        # Verify that the levels line up correctly, and force slice to a factor
        # variable.
        for(i in seq(slice)) {
            if(any(!is.na(slice[[i]]))) {
                if(!is.factor(slice[[i]])) {
                    # Force to factor.
                    if(is.character(slice[[i]])) {
                        slice[[i]] <- match(slice[[i]], dimnames(arr)[[i]])
                    }
                    slice[[i]] <- factor(slice[[i]],
                        levels = seq(dim(arr)[i]),
                        labels = dimnames(arr)[[i]])
                }
                assert(levels(slice[[i]]) == dimnames(arr)[[i]])
            }
        }
    }

    dm <- dim(arr)
    # Dimensions after collapse - NA dimensions match array, only slice
    # levels kept in non-NA dimensions.
    dm2 <- ifelse(is.na(slice), dm, unlist(lapply(slice, length)))
    dmnames2 <- ifelse(is.na(slice), dimnames(arr), slice)
    for(d in which(!is.na(slice))) {
        # Convert uninteresting dimensions down to 1D on either side.
        if(d == 1) {
            dim(arr) <- c(1, dm[d], prod(dm[-d]))
        } else if(d == length(dim(arr))) {
            dim(arr) <- c(prod(dm[-d]), dm[d], 1)
        } else {
            dim(arr) <- c(prod(dm[1:(d-1)]), dm[d], prod(dm[-(1:d)]))
        }
        # Extract slice along this dimension.
        arr <- arr[,unlist(slice[[d]]),]
        # Update dimension to 
        dm[d] <- dm2[d]
        dim(arr) <- dm
    }
    if(drop) {
        dim(arr) <- dm2[dm2 > 1]
        dimnames(arr) <- dmnames2[dm2 > 1]
    } else {
        dimnames(arr) <- dmnames2
    }
    arr
}

# Make an ipf constraint with min/max projections of +/- 4.
# DISABLED - projections are turned off now.
err4 <- function(con) {
    IpfConstraint(data = con)
    #IpfConstraint(data = con,
    #              min = ifelse(con - 4 >= 0, con - 4, 0),
    #              max = con + 4)
}

errct <- function(con) {
    assert(is.vector(profile86i$suppressed))
    # A bit hackish - assumes that census tract code is the first
    # dimension.
    assert(dimnames(con)[[1]] == names(profile86i$suppressed))
    if(FALSE) {
        # adjustSuppressed is our new alternate method for dealing with
        # zero-person census tracts - we actually have some data on the totals
        # per tract, accidentally released in SC86B01.
        #
        # If we didn't have it, we'd use the following to allow anything
        # from 0-39 for zero-person CTs.
        err <- ifelse(profile86i$suppressed, 39, 4)
    } else {
        err <- rep(4, length(profile86i$suppressed))
    }
    err <- array(data = rep(err, prod(dim(con)[-1])), dim = dim(con))
    IpfConstraint(data = con,
                  min = ifelse(con - err >= 0, con - err, 0),
                  max = con + err)
}

# Test if two items are equivalent within the effects of random rounding -
# i.e., within +/- 5 of each other.
censusEqual <- function(a, b, errorA = 5, errorB = 5)
{
    assert(errorA >= 0)
    assert(errorB >= 0)
    abs(a-b) <= max(errorA, errorB)
}

# In the given data frame, adjust area-suppressed rows to have the same
# PDF as row #1 (i.e., CMA total row), but with a different total
# population.
#
# frame: data frame to modify
# suppressed: vector indicating which rows to modify.
# totalCT: size of population in each row.
# ignorefields: columns to leave unmodified.
#
# Integer fields may have non-integer values after completion.
adjustSuppressed <- function(frame, suppressed, scale, ignoreFields)
{
    assert(is.data.frame(frame))
    assert(nrow(frame) == length(suppressed) + 1)
    assert(is.vector(suppressed) && is.logical(suppressed))
    # If it's effectively a 1D array.
    if(is.array(scale) && max(dim(scale)) == prod(dim(scale))) {
        scale <- as.vector(scale)
    }
    assert(is.vector(scale) && is.double(scale))
    assert(length(suppressed) == length(scale))
    assert(is.vector(ignoreFields))

    # Match length of frame.
    suppressed <- c(FALSE, suppressed)
    scale <- c(1, scale)

    # Dimensions to ignore
    nd <- match(ignoreFields, names(frame))

    # Set suppressed rows to equal row #1 times a scale factor.
    for(i in which(suppressed)) {
        frame[i, -nd] <- frame[1, -nd] * scale[i]
    }

    return(frame)
}

# Using the given table, take a series of margins and put them in a list.
#   table - table to take margins of.
#   vars - variables on which to take margins.
#   numways - how to combine variables. e.g., 1 means take 1-way margin of
#     each var, 2 means take all possible 2-way margins of the vars, etc. A
#     vector can be supplied - so 1:3 gives all 1-, 2- and 3-way margins of
#     the variables.
multi.margin.table <- function(table, vars, numways)
{
    assert(is.array(table))
    unlist(lapply(numways, function(numway) {
        # Create a 2D array containing
        combs<-combinations(length(vars), numway, vars)
        result <- list()
        for(i in seq(nrow(combs))) {
            result <- c(result, list(margin.table2(table, combs[i,])))
        }
        result
    }), recursive=FALSE)
}

#
# Convert an index into a high-dimensional array into an index into the
# flattened version of the same array
# That is,
#   arr[idx[1], idx[2], ..., idx[n]]
# is equivalent to
#   as.vector(arr)[flattenIdx(arr, idx)]
# This makes it possible to use high-dimensional array indices, since
# there's no usable R syntax for the first version when the array is
# variable-length (non-constant n).
#
# idx can be either a single index vector into the array, or an array of
# indices, one index per row.
array.flattenIndex <- function(arr = NULL, idx, arrDim = NULL) {
    assert(length(dim(idx)) >= 1 && length(dim(idx)) <= 2)
    if(is.null(arrDim)) {
        assert(is.array(arr))
        arrDim <- dim(arr)
    } else {
        assert(is.null(arr))
    }
    assert(length(arrDim) >= 1)
    assert(idx >= 1 && idx <= arrDim)
    if(length(dim(idx)) == 2) {
        assert(ncol(idx) == length(arrDim))
        result <- idx[,length(arrDim)] - 1
        for(i in rev(seq_along(arrDim))[-1]) {
            result <- result * arrDim[i] + idx[,i] - 1
        }
    } else {
        result <- idx[length(arrDim)] - 1
        for(i in rev(seq_along(arrDim))[-1]) {
            result <- result * arrDim[i] + idx[i] - 1
        }
    }
    names(result) <- NULL
    result + 1
}

#
# Reverses the process in array.flattenIndex.
#
array.unflattenIndex <- function(arr = NULL, idx,
                                 arrDim = NULL,
                                 arrDimnames = NULL,
                                 forceFrame = FALSE) {
    assert(is.null(dim(idx)) || length(dim(idx)) == 1)
    if(is.null(arrDim) || is.null(arrDimnames)) {
        assert(!is.null(arr))
        assert(is.array(arr))
        arrDim <- dim(arr)
        arrDimnames <- dimnames(arr)
    } else {
        assert(is.null(arr))
    }

    if(length(idx) == 1 && !forceFrame) {
        result = c()
    } else {
        result <- data.frame(dummy = seq_along(idx))
    }
    idx <- idx - 1
    for(i in seq_along(arrDim)) {
        result <- cbind(result,
            factor((idx %% arrDim[i]) + 1,
                   levels = seq(arrDim[i]),
                   labels = unlist(arrDimnames[i])))
        idx <- idx %/% arrDim[i]
    }
    if(is.data.frame(result)) {
        result$dummy <- NULL
        assert(is.data.frame(result))
    }
    names(result) <- names(arrDimnames)
    result
}

# Do a Monte Carlo synthesis from a multidimensional array, as described in
# Beckmann et al. (1996)
# Each cell represents the expected frequency of that set of levels, and
# the array sum represents the total number of individuals to synthesise.
#
# The result is a data frame containing the list of synthesised agents.
montecarlo <- function(arr) {
    assert(is.array(arr))

    gc()
    count <- sum(arr)
    # Use a random decision to floor/ceil the total. Bias the decision
    # in favour of the value that's closest.
    if(runif(1) < count - floor(count)) {
        count <- ceiling(count)
    } else {
        count <- floor(count)
    }
    if(count == 0) {
        return(NULL)
    }
    d <- dim(arr)
    dn <- dimnames(arr)
    # Convert the high-dimensional array to a 1D CDF.
    arr <- cumsum(as.vector(arr)) / sum(arr)
    result <- data.frame()

    r <- runif(count)
    # Find the corresponding entry in the CDF.
    r <- findInterval(r, arr) + 1
    # Convert CDF indices to levels of the respective factors.
    result <- 
        array.unflattenIndex(arrDim = d, arrDimnames = dn, idx = r)
}

# Do a Monte Carlo synthesis of (dense) joint probability table "arr",
# conditional upon already-synthesised population popGiven.
#
# Each row of the result corresponds one-to-one with a row in the given
# population.
#
# Only used in a few cases; sparse version is usually used instead.
montecarlo_condition <- function(arr, popGiven) {
    n1 <- names(dimnames(arr))
    n2 <- names(popGiven) #[-1]
    matchDims <- intersect(n1, n2)
    
    # Build a conditional PDF matrix.
    # arr[matchDim1, matchDim2, matchDim3, nonMatchDim1, nonMatchDim2]
    #   contains P(nonMatchDim1, nonMatchDim2 | matchDim1, matchDim2, matchDim3)
    arr <- aperm(arr, c(match(matchDims, n1), seq(n1)[-match(matchDims, n1)]))
    n1 <- names(dimnames(arr))
    denom <- margin.table2(arr, matchDims)
    denom <- ifelse(denom==0, 1, denom)
    denom <- array(
        rep(denom, times=prod(dim(arr)[-seq(matchDims)])),
        dim = dim(arr),
        dimnames = dimnames(arr))
    arr <- arr / denom
    remove(denom)

    # Next, convert to a CDF matrix.
    # First dimension is CDF for various combinations of nonmatchdims.
    # Remaining dimensions will be matchdims
    cdf <- apply(arr, match(matchDims, n1), cumsum)
    # Collapse matchdims down to one dim.
    dim(cdf) <- c(dim(cdf)[1], prod(dim(cdf)[-1]))
    # Reverse - matchdims first, nonmatchdims after.
    cdf <- aperm(cdf, c(2,1))
    #names(dimnames(arr))[1] <- 'cdf'
    #arr <- aperm(arr, c(seq(matchDims) + 1, 1))

    matchIdx <- array.flattenIndex(
        arrDim = dim(arr)[match(matchDims, n1)],
        # This part converts factor levels to integers.
        idx = data.frame(lapply(as.list(popGiven[, matchDims, drop=FALSE]),
                                as.integer))
    )
    r <- runif(nrow(popGiven))
    for(i in seq_along(r)) {
        # Find the corresponding entry in the CDF.
        r[i] <- findInterval(r[i], cdf[matchIdx[i],]) + 1
    }
    remove(cdf)
    # Convert CDF indices to levels of the respective factors.
    # This is more than a little devious... but it works.
    result <- cbind(
        id = seq_along(r), popGiven[, matchDims, drop=FALSE],
        array.unflattenIndex(
            arrDim = dim(arr)[-match(matchDims, n1)],
            arrDimnames = dimnames(arr)[-match(matchDims, n1)],
            idx = r, forceFrame = TRUE))
}

#
# Attempt at stratified monte carlo.
# No longer in use. Superseded by montecarlo_condition (and sparse)
# approaches.
#
montecarlo_stratify <- function(arr, stratify) {
    if( is.null(stratify) ) {
        return(montecarlo(arr));
    }
    assert(is.vector(stratify))
    arrDims <- match(stratify, names(dimnames(arr)))
    assert(!is.na(arrDims))
    strata <- expand.grid(dimnames(arr)[arrDims])
    result <- NULL
    for (i in seq(nrow(strata))) {
        # TODO: does this deal with non-integer totals correctly?
        result <- rbind(result, montecarlo(
            aslice(arr, as.list(strata[i,,drop=FALSE]), drop=FALSE)))
    }
    # Fix up stratified factors. Any levels that go unsynthesized
    # won't appear in the output factor otherwise.
    for(i in seq(arrDims)) {
        col <- which(names(result) == stratify[i])
        labels <- dimnames(arr)[[arrDims[i]]]
        result[,col] <- factor(match(result[,col], labels),
            levels = seq(labels), labels = labels)
    }
    result
}

# Rename column names in a data frame.
renameFrame <- function(frame, fromNames, toNames) {
    names(frame)[match(fromNames, names(frame))] <- toNames
    frame
}

#
# Convert a field + set of values to an SQL-style OR query
#
makeOrQuery <- function(field, values) {
     paste('(', field, '=', values[1],
           paste(unlist(lapply(values[-1], function(x) {
                                paste(' OR ', field, '=', x, sep='')
                              })), collapse=''),
           ')', sep='')
}

#
# Generic data input facility. Can use either
#
# a) input from Postgres database.
# b) input from CSV files.
#
# The "whereExpression" is *only* applied to SQL queries - it's just to
# speed things up. Callers should still filter results using regular R
# expressions to replicate the where expression.
inputData <- function(tablename, columns, sortColumns = NULL,
                      whereExpression = NULL) {

    # Change the following line to either 'sql' or 'csv' to choose
    # different input sources.
    inputSource <- 'csv'

    if( inputSource != 'csv' ) {
        channel <- odbcConnect("PostgreSQL census", uid="david", case="tolower")
        # Read in pum86 tables.
        query <- paste('SELECT ', paste(columns, collapse=', '),
                     ' FROM ', tablename,
                     sep='')
        if(!is.null(whereExpression)) {
            query <- paste(query, ' WHERE ', whereExpression, sep='')
        }
        if(!is.null(sortColumns)) {
            query <- paste(query, ' ORDER BY ',
                         paste(sortColumns, collapse=', '),
                         sep='')
        }
        result <- sqlQuery(channel, query)
        odbcClose(channel)
    } else {
        result <- read.csv(paste('../data/', tablename, '.csv', sep=''))
        if(!('*' %in% columns)) {
            result <- result[, columns]
        }
        if(!is.null(sortColumns)) {
            result <- result[do.call(order, result[, sortColumns, drop=FALSE]),]
        }
    }
    result
}
