#
# Category collapsing routines using "levelmap" class.
#
# Collapse a factor's levels (categories) to a smaller number of levels.
# A mapping vector is used to map the input levels to the smaller output
# set of levels.
#
# e.g., take a set of input levels:
#
#      c( 'sky blue', 'navy blue', 'midnight blue',
#         'forest green', 'light green',
#         'mustard', 'gold', 'beige')
#
#  and "collapse" categories to form a smaller set of output levels:
#
#      c( 'blue', 'green', 'yellow' )
#
# as follows:
#
#  collapse.factor(inFactor,
#    LevelMap(map=c(1,1,1,2,2,3,3,3),
#             levels=1:3,
#             labels=c('blue', 'green', 'yellow'))
#  )
#
# There's no intelligence to the collapsing - we have to be told which
# input levels to merge to form a set of output levels, as in the map
# parameter above.

library('abind')
library('gtools')

source('LevelMap.R')

# Argument
#   fac: input factor, with levels from 1:M (possibly missing some).
# Return value: factor with new set of levels, from 1:N
collapse.factor <- function(fac, levelmap) {
    #assert(is.factor(fac))
    assert(is.LevelMap(levelmap))
    assert(min(levelmap$.map) >= 1)
    assert(length(levelmap$.labels) >= max(levelmap$.map))

    factor(levelmap$.map[unclass(fac)],
           levels = levelmap$.levels,
           labels = levelmap$.labels,
           ordered = is.ordered(fac))
}

# Collapse an array.
collapse.array <- function(x, levelmap) {
    assert(is.array(x))
    assert(is.LevelMap(levelmap))
    assert(min(levelmap$.map) == 1)
    assert(length(levelmap$.labels) >= max(levelmap$.map))

    d <- which(levelmap$.factor == names(dimnames(x)))
    assert(!is.na(d))
    dm <- dim(x)
    assert(dm[d] == length(levelmap$.map))
    # Dimensions after collapse
    dm2 <- dm
    dm2[d] <- length(levelmap$.levels)
    dmnames2 <- dimnames(x)
    dmnames2[[d]] <- levelmap$.labels
    # Convert uninteresting dimensions down to 1D on either side.
    if(d == 1) {
        dim(x) <- c(1, dm[d], prod(dm[-d]))
    } else if(d == length(dim(x))) {
        dim(x) <- c(prod(dm[-d]), dm[d], 1)
    } else {
        dim(x) <- c(prod(dm[1:(d-1)]), dm[d], prod(dm[-(1:d)]))
    }
    unmap <- unmap(levelmap)
    result <- NULL
    for(i in seq(along=unmap)) {
        slice <- x[,unmap[[i]],,drop=FALSE]
        # margin.table is slow - only take it if we have to.
        if( length(unmap[[i]]) > 1 ) {
            slice <- margin.table2(slice, -2)
            assert(length(dim(slice)) == 2)
            # Make sure that abind doesn't get confused.
            dim(slice) <- c(dim(slice)[1], 1, dim(slice)[2])
        }
        assert(length(dim(slice)) == 3)
        result <- abind(result, slice, along=2)
    }
    # Revert to the correct shape, with all dimensions.
    dim(result) <- dm2
    dimnames(result) <- dmnames2
    result
}

# Collapse an array using the given levelmaps.
collapseList.array <- function(arr, levelmapList) {
    assert(is.array(arr))
    assert(is.list(levelmapList))
    lapply(levelmapList, function(x) { assert(is.LevelMap(x)) })

    for(i in seq(along=levelmapList)) {
        levelmap <- levelmapList[[i]]
        # Only try to collapse if the dimension exists.
        if(levelmap$.factor %in% names(dimnames(arr))) {
            arr <- collapse.array(arr, levelmap)
        }
    }
    arr
}

# Reverse the effect of the given list of levelmaps: expand the array to match
# the levels before the collapse.
#
# TODO: eliminate need for fullArray (currently required to know dimnames
# after uncollapse).
uncollapseList.array <- function(arr, levelmapList, fullArray) {
    # Uncollapse one dimension at a time.
    for(i in seq(along=levelmapList)) {
        levelmap <- levelmapList[[i]]
        ndimc <- length(dim(arr))
        d <- which(names(dimnames(arr)) == levelmap$.factor)
        if(length(d) == 0) {
            # This variable isn't in the matrix. Ignore it.
            next
        }
        assert(length(d) == 1)

        # Dimensions before uncollapse
        dm <- dim(arr)
        # Dimensions after uncollapse
        dm2 <- dm
        dm2[d] <- length(dimnames(fullArray)[[levelmap$.factor]])
        dmnames2 <- dimnames(arr)
        dmnames2[[d]] <- dimnames(fullArray)[[levelmap$.factor]]
        # The mapping. This is unfortunately difficult because
        # there the only reasonable way to do this is the [
        # operator (e.g., scale[,,map,,,]), but there an unknown
        # number of commas (dimensions) before and after the map.
        # So, to work around this, I do this:
        # 1) Reduce other dimensions down to 1D on either
        # side of this D.
        # 2) Uncollapse this dimension.
        # 3) Restore other dimensions.
        if(d == 1) {
            dim(arr) <- c(1, dm[d], prod(dm[-d]))
        } else if(d == ndimc) {
            dim(arr) <- c(prod(dm[-d]), dm[d], 1)
        } else {
            dim(arr) <- c(prod(dm[1:(d-1)]), dm[d],
                            prod(dm[-(1:d)]))
        }
        arr <- arr[,levelmap$.map,]
        arr <- array(arr, dim = dm2, dimnames = dmnames2)
    }
    arr
}
