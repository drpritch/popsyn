//
// C auxiliary file to make margin.table2() method work.
//

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <assert.h>
#include <stdio.h>

// To build, execute from the Unix command line:
//
// MAKEFLAGS="CFLAGS=-O3" R CMD SHLIB margintable2.c

/*
dyn.load('margintable2.so')

sumRows <- function(x) {
    if(length(dim(x)) == 2 && is.array(x)) {
        result <- x[,1]
        array(.C('sumRows', as.double(result), as.double(x),
                 as.integer(dim(x)[1]), as.integer(dim(x)[2]))[[1]])
    } else {
        stop('2D array required')
    }
}

sumCols <- function(x) {
    if(length(dim(x)) == 2 && is.array(x)) {
        result <- x[1,]
        array(.C('sumCols', as.double(result), as.double(x),
                 as.integer(dim(x)[1]), as.integer(dim(x)[2]))[[1]])
    } else {
        stop('2D array required')
    }
}
*/
static void
sumRows(double *outData,
        const double *inData, const int *numRows, const int *numCols)
{
    int row, col;

    assert(inData && outData && numRows && numCols);
    assert(*numRows > 0 && *numCols > 0);

    for(row = 0; row < *numRows; ++row) {
        *outData = 0;
        for(col = 0; col < *numCols; ++col) {
            *outData += inData[col * *numRows + row];
        }
        ++outData;
    }
}

static void
sumCols(double *outData,
        const double *inData, const int *numRows, const int *numCols)
{
    int row, col;

    assert(inData && outData && numRows && numCols);
    assert(*numRows > 0 && *numCols > 0);

    for(col = 0; col < *numCols; ++col) {
        *outData = 0;
        for(row = 0; row < *numRows; ++row) {
            *outData += *inData;
            ++inData;
        }
        ++outData;
    }
}

static R_NativePrimitiveArgType argTypes[4] =
    { REALSXP,   REALSXP,  INTSXP,   INTSXP };
static R_NativeArgStyle argStyles[4] =
    { R_ARG_OUT, R_ARG_IN, R_ARG_IN, R_ARG_IN };

static const R_CMethodDef cMethods[] = {
    {"sumRows", (DL_FUNC) &sumRows, 4, argTypes, argStyles},
    {"sumCols", (DL_FUNC) &sumCols, 4, argTypes, argStyles},
    {NULL, NULL, 0}
};

void
R_init_margintable2(DllInfo *info)
{
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);
}

void
R_unload_margintable2(DllInfo *info)
{
}
