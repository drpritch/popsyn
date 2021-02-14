# Define a constraint for the iterative proportional fitting algorithm.
#
# Data, min and max are all contingency tables of the same data type and
# dimensions. Data represents the target for a margin; optionally, min/max
# can be specified in addition.
#
# For 1D, 2D or higher constraints, an array should be used, and the dimname
# of each dimension corresponds to the factor name.
# For 0D constraints representing the total for the entire IPF,
# data/min/max can be a scalar.
#
IpfConstraint <- function(data, min = NULL, max = NULL)
{
    assert(!is.null(data))
    assert(is.array(data) || length(data) == 1)
    assert(!is.na(data))
    # Replace NAs with +/- infinity
    if( !is.null(min)) {
        min <- ifelse(is.na(min), 0, min)
        assert(is.array(min) || length(min) == 1)
        assert(length(min) == length(data))
        assert(dim(min) == dim(data))
        assert(names(dimnames(min)) == names(dimnames(data)))
        assert(unlist(dimnames(min)) == unlist(dimnames(data)))
        assert(min >= 0)
    }
    if( !is.null(max)) {
        max <- ifelse(is.na(max),  Inf, max)
        assert(is.array(max) || length(max) == 1)
        assert(length(max) == length(data))
        assert(dim(max) == dim(data))
        assert(names(dimnames(max)) == names(dimnames(data)))
        assert(unlist(dimnames(max)) == unlist(dimnames(data)))
        assert(max >= 0)
    }
    if( !is.null(min) && !is.null(max) ) {
        assert(min <= max)
    }
    result <- list(.data = data, .min = min, .max = max);
    class(result) <- 'IpfConstraint'
    result
}

is.IpfConstraint <- function(x) {
    is.list(x) && inherits(x, 'IpfConstraint')
}

# Prepare an IpfConstraint for application:
# 1) attach maps telling it how to collapse the array to match the constraint
#    levels (levelmapList). This is for cases where the array has more
#    levels than the constraint.
# 2) collapse the constraint using invLevelmapList. This is for cases where
#    the constraint has more levels than the array.
prepare.IpfConstraint <- function(this, levelmapList, invLevelmapList = NULL)
{
    assert(is.IpfConstraint(this))
    if( !is.null(levelmapList)) {
        assert(is.list(levelmapList))
        lapply(levelmapList, function(x) { assert(is.LevelMap(x)) })

        # Can't include multiple collapses on the same factor.
        # Yet.
        factors <- unlist(lapply(levelmapList,
                                 function(levelmap) { levelmap$.factor }))
        if(any(duplicated(factors))) {
            stop('Duplicated factors in levelmapList')
        }
    }
    if( !is.null(invLevelmapList)) {
        assert(is.list(invLevelmapList))
        lapply(invLevelmapList, function(x) { assert(is.LevelMap(x)) })
    }

    # TODO: could just merge the maps in, but it gets tricky if we're
    # recollapsing a factor that has already been collapsed once.
    assert(is.null(this$.levelmapList))
    if( !is.null(levelmapList) ) {
        this$.levelmapList <- levelmapList
        names(this$.levelmapList) <-
            unlist(lapply(this$.levelmapList, function(levelmap)
                { levelmap$.factor }))
    }
    if( !is.null(invLevelmapList) ) {
        this$.data <- collapseList.array(this$.data, invLevelmapList)
        if( !is.null(this$.min)) {
            this$.min <- collapseList.array(this$.min, invLevelmapList)
        }
        if( !is.null(this$.max)) {
            this$.max <- collapseList.array(this$.max, invLevelmapList)
        }
    }

    # TODO: set min/max if not set
    this
}
