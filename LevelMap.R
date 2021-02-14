#
# See collapse.R for detail.
# Category collapsing auxiliary class, storing the information necessary to
# do a collapse.
#

library('gtools')

# Collapse a factor's levels (categories) to a smaller number of levels.
# A mapping vector is used to map the input levels to the smaller output
# set of levels.
#
# Arguments:
#   map: array mapping input levels to output levels. Output levels
#       must range from 1:N, with no gaps.
#   labels: array of labels associated with each output level, at least N
#       dimensions.
#   factor: name of factor affected (necessary when collapsing an array)
#
LevelMap <- function(map, levels, labels, factor = NULL) {
    # TODO: handle NA in levels, to allow definition of constraints on a subset
    # of the original levels.
    assert(!is.na(levels))
    result <- list(.factor = factor, .map = map, .levels = levels,
                   .labels = labels)
    class(result) <- 'LevelMap'
    result
}

is.LevelMap <- function(x) {
    is.list(x) && inherits(x, 'LevelMap')
}

# Build a (one-to-many) list mapping the new levels back to the original levels.
unmap.LevelMap <- function(this) {
    assert(is.LevelMap(this))
    lapply(this$.levels, function(level) {
        (1:length(this$.map))[this$.map == level]
    })
}

unmap <- function(...)
    UseMethod('unmap')
