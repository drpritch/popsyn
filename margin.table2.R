#
# Replacement for margin.table()
# Calculates the sum of a table over the given margins.
#
# * Allows margins to be either the numbers of dimensions to keep (like
#   margin.table), or the names of dimensions to keep.
# * Much faster than margin.table() when the result is a large array
#   (100,000+ cells)

filename <- paste('margintable2', .Platform$dynlib.ext, sep="")
if(class(try(dyn.load(filename), silent=TRUE)) == 'try-error') {
    print(paste('Could not load ', filename,
          '. Using slow native array sum method instead.'))
}
remove(filename)

# Variation of margin.table that also takes dimension names.
margin.table2 <- function(x, margin = NULL)
{
    sumCols <- function(x) {
        if(length(dim(x)) == 2 && is.array(x)) {
            result <- x[1,]
            array(.C('sumCols', as.double(result), as.double(x),
                     as.integer(nrow(x)), as.integer(ncol(x)))[[1]])
        } else {
            stop('2D array required')
        }
    }

    if (!is.array(x))
        stop("'x' is not an array")
    if (length(margin) == 0)
        return(sum(x))

    # Convert margin names to integer dimensions, if named margined are
    # given.
    if(class(margin) == 'character') {
        margin <- match(margin, names(dimnames(x)))
        if( any(is.na(margin)) ) {
            stop('Unknown dimname in margin')
        }
    }

    if( is.loaded('sumCols') ) {
        d <- dim(x)
        dl <- length(d)
        if (dl == 0)
            stop("dim(x) must have a positive length")
        if (min(d) <= 0)
            stop("dim(x) must be non-zero")
        ds <- 1:dl
        if (length(oldClass(x)) > 0)
            x <- if (dl == 2)
                as.matrix(x)
            else as.array(x)
        dn <- dimnames(x)
        s.call <- ds[-margin]
        s.ans <- ds[margin]
        d.call <- d[-margin]
        d.ans <- d[margin]
        dn.call <- dn[-margin]
        dn.ans <- dn[margin]
        d2 <- prod(d.ans)
        newx <- aperm(x, c(s.call, s.ans))
        dim(newx) <- c(prod(d.call), d2)
        ans <- sumCols(newx)
    } else {
        # Revert to the normal margin.table approach.
        ans <- apply(x, margin, sum)
    }
    dim(ans) <- dim(x)[margin]
    dimnames(ans) <- dimnames(x)[margin]
    class(ans) <- oldClass(x)
    ans
}
