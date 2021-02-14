//
// C++ auxiliary file to make ipf_list.R methods work.
//

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <assert.h>
#include <stdio.h>
#include <algorithm>
#include <functional>
#include <vector>
// Seems to be broken...
//#include <iterator>

// To build, execute from the Unix command line:
//
// MAKEFLAGS="CFLAGS=-O3" R CMD SHLIB ipf_list.cpp

//#define DEBUG

#define MYPROTECT(x) ++numProtected; PROTECT(x)
#define MYERROR(x) \
    { UNPROTECT(numProtected); \
    error(x); \
    return a_result; }

/*
 * Dimensions of pointer arguments:
 *
 * outCollapsed: dimList[0] x dimList[1] x ... x dimList[numList] [ x *dimArray]
 *   Only includes dimArray if "collapsedIncludesArray" flag is set.
 * inList: numRows x numList
 * inWeight: numRows x dimArray
 * inDimList: numList
 *
 * Dimensions described in dimList are in the R convention - i.e., first
 * dimension varies fastest (a.k.a. row-minor).
 */
#define INLIST(r,c)    inList[(c) * numRows + (r)]
#define INWEIGHT(r,a)  inWeight[(a) * numRows + (r)]
static SEXP
ipfCollapseFromList(SEXP a_inList,
                    SEXP a_inWeight,
                    SEXP a_inDimList,
                    SEXP a_collapsedIncludesArray)
{
    SEXP a_result;
    int collapsedIncludesArray = asLogical(a_collapsedIncludesArray);
    int numRows, numList, dimArray;
    double *outCollapsed = NULL;
    const int *inList = NULL, *dimList = NULL;
    const double *inWeight = NULL;
    int numProtected = 0;

    // TODO: get a sensible default, until we know how big to make it
    MYPROTECT(a_result = NEW_NUMERIC(1));
    *REAL(a_result) = 0;

    ///
    // #1: Type conversions, and protection of input data.
    ///
    MYPROTECT(a_inWeight = AS_NUMERIC(a_inWeight));
    MYPROTECT(a_inList = AS_INTEGER(a_inList));
    MYPROTECT(a_inDimList = AS_INTEGER(a_inDimList));
    if(collapsedIncludesArray == NA_LOGICAL ) {
        MYERROR("collapsedIncludesArray (arg 4) cannot be NA");
    }

    ///
    // #2: Dimension checks
    ///
    // Forbid more than two dimensions on weight.
    if(GET_DIM(a_inWeight) != R_NilValue &&
       GET_LENGTH(GET_DIM(a_inWeight)) > 2) {
        MYERROR("Weight (arg 2) cannot be higher than 2D");
    }
    // Require presence of 2D dimension on inList
    if(GET_DIM(a_inList) == R_NilValue ||
       GET_LENGTH(GET_DIM(a_inList)) != 2) {
        MYERROR("List (arg 1) must be 2D");
    }
    if(GET_DIM(a_inWeight) != R_NilValue &&
       GET_LENGTH(GET_DIM(a_inWeight)) == 2) {
        numRows = INTEGER(GET_DIM(a_inWeight))[0];
        dimArray = INTEGER(GET_DIM(a_inWeight))[1];
    } else {
        numRows = GET_LENGTH(a_inWeight);
        dimArray = 1;
    }
    numList = INTEGER(GET_DIM(a_inList))[1];
    // Require numrows to match up.
    if(INTEGER(GET_DIM(a_inList))[0] != numRows ) {
        MYERROR("Rows in List and Weight do not match (args 1 and 2)");
    }
    // Require presence of numlist (+1)-dimension on inCollapsed
    if(GET_LENGTH(a_inDimList) !=
          numList + (collapsedIncludesArray ? 1 : 0)) {
        MYERROR("DimList (arg 3) has wrong dimensions");
    }
    dimList = INTEGER(a_inDimList);

    if(numRows <= 0 || numList <= 0) {
        MYERROR("Zero rows/list columns not allowed");
    }

    ///
    // #3: get pointers to the actual data
    ///
    inWeight = REAL(a_inWeight);
    inList = INTEGER(a_inList);
    //outCollapsed = REAL(a_outCollapsed);

    ///
    // #4: Do it!
    ///
    int row, listIdx, arr, addr;
    int maxOutCollapsed;
    int warn = 0;

    assert(inList && inWeight && dimList);
    assert(numRows > 0 && numList > 0 && dimArray > 0);

    maxOutCollapsed = collapsedIncludesArray ? dimArray : 1;
    for(listIdx = 0; listIdx < numList; ++listIdx) {
        assert(dimList[listIdx] > 0);
        maxOutCollapsed *= dimList[listIdx];
    }

    MYPROTECT(a_result = NEW_NUMERIC(maxOutCollapsed));
    outCollapsed = REAL(a_result);
    memset(outCollapsed, 0, sizeof(double) * maxOutCollapsed);
    for(row = 0; row < numRows; ++row) {
        for(arr = 0; arr < dimArray; ++arr) {
            // Goal is something like this.
            // addr = listIdx0 +
            //        dimList[0]*(listIdx1 +
            //          dimList[1]*(listIdx2 +
            //            dimList[2]*arr))
            addr = collapsedIncludesArray ? arr : 0;
            for(listIdx = numList - 1; listIdx >= 0; --listIdx) {
                addr = (addr * dimList[listIdx]) +
                    // -1 is required because the indices are 1-based
                    // rather than 0-based.
                    INLIST(row, listIdx)-1;
            }
#ifdef DEBUG
            if(addr < maxOutCollapsed) {
                outCollapsed[addr] += INWEIGHT(row, arr);
            } else {
                if(!warn) {
                    printf("Exceed limit %d/%d: %d,%d\n",
                        addr, maxOutCollapsed, row, arr);
                }
                warn = 1;
            }
#else
            assert(addr < maxOutCollapsed);
            outCollapsed[addr] += INWEIGHT(row, arr);
#endif
        }
    }
    UNPROTECT(numProtected);
    return a_result;
}
#undef INLIST
#undef INWEIGHT

/*
 * Dimensions of pointer arguments:
 *
 * weight: numRows x dimArray
 * outMaxDelta: 1
 * inList: numRows x numList
 * inCollapsed: dimList[0] x dimList[1] x ... x dimList[numList] [ x *dimArray ]
 *   Only includes dimArray if "collapsedIncludesArray" flag is set.
 *
 * Dimensions described in dimList are in the R convention - i.e., first
 * dimension varies fastest (a.k.a. row-minor).
 */
#define WEIGHT(r,a)  weight[(a) * numRows + (r)]
#define INLIST(r,c)  inList[(c) * numRows + (r)]
static SEXP
ipfExpandToList(SEXP a_inOutWeight,
                SEXP a_inList,
                SEXP a_inCollapsed,
                SEXP a_collapsedIncludesArray)
{
    SEXP a_result, a_dimList;
    int collapsedIncludesArray = asLogical(a_collapsedIncludesArray);
    int numRows, numList, dimArray;
    double *weight = NULL, *outMaxDelta = NULL;
    const double *inCollapsed = NULL;
    const int *inList = NULL, *dimList = NULL;
    int numProtected = 0;

    MYPROTECT(a_result = NEW_NUMERIC(1));
    outMaxDelta = REAL(a_result);
    *outMaxDelta = 0;

    ///
    // #1: Type conversions, and protection of input data.
    ///
    if(!IS_NUMERIC(a_inOutWeight)) {
        MYERROR("Weight (arg 1) must be numeric");
    }
    MYPROTECT(a_inOutWeight);
    MYPROTECT(a_inList = AS_INTEGER(a_inList));
    MYPROTECT(a_inCollapsed = AS_NUMERIC(a_inCollapsed));
    if(collapsedIncludesArray == NA_LOGICAL ) {
        MYERROR("collapsedIncludesArray (arg 4) cannot be NA");
    }

    ///
    // #2: Dimension checks
    ///
    // Forbid more than two dimensions on weight.
    if(GET_DIM(a_inOutWeight) != R_NilValue &&
       GET_LENGTH(GET_DIM(a_inOutWeight)) > 2) {
        MYERROR("Weight (arg 1) cannot be higher than 2D");
    }
    // Require presence of 2D dimension on inList
    if(GET_DIM(a_inList) == R_NilValue ||
       GET_LENGTH(GET_DIM(a_inList)) != 2) {
        MYERROR("List (arg 2) must be 2D");
    }
    if(GET_DIM(a_inOutWeight) != R_NilValue &&
       GET_LENGTH(GET_DIM(a_inOutWeight)) == 2) {
        numRows = INTEGER(GET_DIM(a_inOutWeight))[0];
        dimArray = INTEGER(GET_DIM(a_inOutWeight))[1];
    } else {
        numRows = GET_LENGTH(a_inOutWeight);
        dimArray = 1;
    }
    numList = INTEGER(GET_DIM(a_inList))[1];
    // Require numrows to match up.
    if(INTEGER(GET_DIM(a_inList))[0] != numRows ) {
        MYERROR("Rows in Weight and List do not match (args 1 and 2)");
    }
    // Require presence of numlist (+1)-dimension on inCollapsed
    if(GET_DIM(a_inCollapsed) == R_NilValue ||
       GET_LENGTH(GET_DIM(a_inCollapsed)) !=
          numList + (collapsedIncludesArray ? 1 : 0)) {
        MYERROR("Collapsed (arg 3) has wrong dimensions");
    }
    MYPROTECT(a_dimList = GET_DIM(a_inCollapsed));
    dimList = INTEGER(a_dimList);

    if(numRows <= 0 || numList <= 0) {
        MYERROR("Zero rows/list columns not allowed");
    }

    ///
    // #3: get pointers to the actual data
    ///
    weight = REAL(a_inOutWeight);
    inList = INTEGER(a_inList);
    inCollapsed = REAL(a_inCollapsed);

    ///
    // #4: Do it!
    ///
    int row, listIdx, arr, addr;
    int maxInCollapsed;
    int warn = 0;
    double delta;
    assert(weight && outMaxDelta && inList && inCollapsed);
    assert(numRows && numList && dimList && dimArray);

    maxInCollapsed = collapsedIncludesArray ? dimArray : 1;
    for(listIdx = 0; listIdx < numList; ++listIdx) {
        assert(dimList[listIdx] > 0);
        maxInCollapsed *= dimList[listIdx];
    }

    for(row = 0; row < numRows; ++row) {
        for(arr = 0; arr < dimArray; ++arr) {
            // Goal is something like this.
            // addr = listIdx0 +
            //        dimList[0]*(listIdx1 +
            //          dimList[1]*(listIdx2 +
            //            dimList[2]*arr))
            addr = collapsedIncludesArray ? arr : 0;
            for(listIdx = numList - 1; listIdx >= 0; --listIdx) {
                addr = (addr * dimList[listIdx]) +
                    // -1 is required because the indices are 1-based
                    // rather than 0-based.
                    INLIST(row, listIdx)-1;
            }
#ifdef DEBUG
            if(addr < maxInCollapsed)
#else
            assert(addr < maxInCollapsed);
#endif
            {
                assert(addr < maxInCollapsed);
                delta = WEIGHT(row, arr) * (inCollapsed[addr] - 1);
                if(delta > *outMaxDelta) {
                    *outMaxDelta = delta;
                }
                WEIGHT(row, arr) *= inCollapsed[addr];
            }
#ifdef DEBUG
            else {
                if(!warn) {
                    printf("Exceed limit %d/%d: %d,%d\n",
                        addr, maxInCollapsed, row, arr);
                }
                warn = 1;
            }
#endif
        }
    }

    UNPROTECT(numProtected);
    return a_result;
}
#undef WEIGHT
#undef INLIST


class ListRow {
public:
    ListRow(const int *list, const std::vector<int> *indirectOrder,
            int numRows, int numCols, int row) :
        myList(list), myNumRows(numRows), myNumCols(numCols), myRow(row)
    {
        // Convert to indirected row id.
        if(NULL != indirectOrder) {
            myRow = (*indirectOrder)[myRow];
        }
    }
    bool operator<(const ListRow &rhs) const
    {
        for(int col = 0; col < myNumCols; ++col) {
            if(getData(col) < rhs.getData(col)) {
                return true;
            } else if(getData(col) > rhs.getData(col)) {
                return false;
            }
        }
        return false;
    }
private:
    int getData(int col) const
    { return myList[col*myNumRows + myRow]; }
    const int *myList;
    const int myNumRows, myNumCols;
    int myRow;
};

// Should derive from std::iterator<std::input_iterator_tag, ListRow>
// ... but it's causing errors in my compiler right now.
class ListRowIterator {
public:
    typedef std::input_iterator_tag iterator_category;
    typedef ListRow                 value_type;
    typedef ptrdiff_t               difference_type;
    typedef ListRow*                pointer;
    typedef ListRow&                reference;

    ListRowIterator()
        : myList(NULL), mySortOrder(NULL), myNumRows(0), myNumCols(0), myRow(0)
    { }
    ListRowIterator(const int *inList, const std::vector<int> &sortOrder,
                      int numRows, int numCols,
                      int row)
        : myList(inList), mySortOrder(&sortOrder),
        myNumRows(numRows), myNumCols(numCols),
        myRow(row)
    { }
    static ListRowIterator
    begin(const int *inList, const std::vector<int> &sortOrder,
          int numRows, int numCols)
    {
        return ListRowIterator(inList, sortOrder, numRows, numCols, 0);
    }
    static ListRowIterator
    end(const int *inList, const std::vector<int> &sortOrder,
        int numRows, int numCols)
    {
        return ListRowIterator(inList, sortOrder, numRows, numCols, numRows);
    }

    ListRowIterator &operator++(int)
    {
        ++myRow;
        return *this;
    }
    ListRowIterator operator++()
    {
        ListRowIterator result(*this);
        ++myRow;
        return result;
    }
    bool operator!=(const ListRowIterator &rhs) const {
        return myRow != rhs.myRow;
    }
    ListRow operator*() const {
        return ListRow(myList, mySortOrder, myNumRows, myNumCols, myRow);
    }
    int getRow() const {
        return myRow;
    }

private:
    const int *myList;
    const std::vector<int> *mySortOrder;
    int myNumRows, myNumCols;
    int myRow;
};

class IndirectListComp : public std::binary_function<int*,int*,bool> {
public:
    IndirectListComp(const int *inList, int numRows, int numCols) :
        myList(inList), myNumRows(numRows), myNumCols(numCols)
    { }
    bool operator()(int row1, int row2) const {
        return
            ListRow(myList, NULL, myNumRows, myNumCols, row1) <
            ListRow(myList, NULL, myNumRows, myNumCols, row2);
    }
private:
    const int *myList;
    int myNumRows;
    int myNumCols;
};

#define WEIGHTCOND(r,a)  inOutWeightCond[(a) * numRows + (r)]
#define INLIST(r,c)      inList[(c) * numRows + (r)]
static SEXP
montecarloConditionList(SEXP a_inIds,
                        SEXP a_inList,
                        SEXP a_inOutWeightCond,
                        SEXP a_inPopGivenList,
                        SEXP a_inPopGivenArray,
                        SEXP a_inRandom)
{
    SEXP a_result, a_dimList;
    int numRows, numList, dimArray, numPop;
    int *outIds = NULL;
    double *inOutWeightCond = NULL;
    const int *inIds = NULL, *inList = NULL;
    const int *inPopGivenList = NULL, *inPopGivenArray = NULL;
    const double *inRandom = NULL;
    int numProtected = 0;
    int row, lastRow, row2, list, arr, pop;
    double total;

    MYPROTECT(a_result = NEW_INTEGER(1));
    outIds = INTEGER(a_result);
    *outIds = 0;

    ///
    // #1: Type conversions, and protection of input data.
    ///
    MYPROTECT(a_inIds = AS_INTEGER(a_inIds));
    MYPROTECT(a_inList = AS_INTEGER(a_inList));
    if(!IS_NUMERIC(a_inOutWeightCond)) {
        MYERROR("Weight (arg 1) must be numeric");
    }
    MYPROTECT(a_inOutWeightCond);
    MYPROTECT(a_inPopGivenList = AS_INTEGER(a_inPopGivenList));
    MYPROTECT(a_inPopGivenArray = AS_INTEGER(a_inPopGivenArray));
    MYPROTECT(a_inRandom = AS_NUMERIC(a_inRandom));

    ///
    // #2: Dimension checks
    ///
    // Require presence of 2D dimension on several args
    if(GET_DIM(a_inList) == R_NilValue ||
       GET_LENGTH(GET_DIM(a_inList)) != 2) {
        MYERROR("List (arg 2) must be 2D");
    }
    if(GET_DIM(a_inOutWeightCond) == R_NilValue ||
       GET_LENGTH(GET_DIM(a_inOutWeightCond)) != 2) {
        MYERROR("WeightCond (arg 3) must be 2D");
    }
    if(GET_DIM(a_inPopGivenList) == R_NilValue ||
       GET_LENGTH(GET_DIM(a_inPopGivenList)) != 2) {
        MYERROR("PopGivenList (arg 4) must be 2D");
    }

    numRows = INTEGER(GET_DIM(a_inOutWeightCond))[0];
    dimArray = INTEGER(GET_DIM(a_inOutWeightCond))[1];
    numList = INTEGER(GET_DIM(a_inList))[1];
    numPop = INTEGER(GET_DIM(a_inPopGivenList))[0];
    // Require numrows to match up.
    if(GET_LENGTH(a_inIds) != numRows ||
       INTEGER(GET_DIM(a_inList))[0] != numRows ) {
        MYERROR("Rows in Ids/List/Weight do not match (args 1-3)");
    }
    if(GET_LENGTH(a_inPopGivenArray) != numPop) {
        MYERROR("Rows in PopGivenList/Array do not match (args 4,5)");
    }
    if(INTEGER(GET_DIM(a_inPopGivenList))[1] != numList) {
        MYERROR("Columns in List/PopGivenList do not match (args 2,4)");
    }
    if(GET_LENGTH(a_inRandom) != numPop) {
        MYERROR("Rows in Random do not match PopGivenList");
    }

    if(numRows <= 0 || numList <= 0 || dimArray <= 0) {
        MYERROR("Zero rows/list columns not allowed");
    }

    ///
    // #3: get pointers to the actual data
    ///
    inIds = INTEGER(a_inIds);
    inList = INTEGER(a_inList);
    inOutWeightCond = REAL(a_inOutWeightCond);
    inPopGivenList = INTEGER(a_inPopGivenList);
    inPopGivenArray = INTEGER(a_inPopGivenArray);
    inRandom = REAL(a_inRandom);
    if(*std::min_element(inPopGivenArray, inPopGivenArray + numPop) < 1 ||
       *std::max_element(inPopGivenArray, inPopGivenArray + numPop) > dimArray)
    {
        MYERROR("PopGiven array levels out of range 1:ncol(weight)")
    }
    // TODO: check that all popgivenlist items show up in inlist.

    MYPROTECT(a_result = NEW_INTEGER(numPop));
    outIds = INTEGER(a_result);

    ///
    // #4: Build an array contain the *sorted* order of the rows.
    // If you iterate through the rows in the order of sortOrder, you'll
    // get them in ascending sequence, according to the list values.
    ///
    std::vector<int> sortOrder(numRows);
    for(row = 0; row < numRows; ++row) {
        sortOrder[row] = row;
    }
    std::sort(sortOrder.begin(), sortOrder.end(),
              // Use an indirect comparator: compare the contents of the
              // referenced list row, not the indices themselves.
              IndirectListComp(inList, numRows, numList));

    ///
    // #5: For each member of the given population, draw a member of the
    // new population using the conditioned PDFs.
    ///
    for(pop = 0; pop < numPop; ++pop) {
        std::pair<ListRowIterator, ListRowIterator> groupRows =
            std::equal_range(ListRowIterator::begin(inList, sortOrder,
                                                    numRows, numList),
                             ListRowIterator::end(inList, sortOrder,
                                                  numRows, numList),
                             ListRow(inPopGivenList, NULL, numPop, numList,pop),
                             std::less<ListRow>());
        assert(groupRows.first.getRow() <= groupRows.second.getRow());
        if(groupRows.first.getRow() == groupRows.second.getRow())
        {
            // Signals "not found".
            outIds[pop] = -1;
        } else {
            assert(groupRows.first.getRow() >= 0 &&
                   groupRows.first.getRow() < numRows);
            assert(groupRows.second.getRow() > 0 &&
                   groupRows.second.getRow() <= numRows);
            outIds[pop] = inIds[groupRows.second.getRow() - 1];

            // We've identified a set of rows [begin,end-1] that have
            // identical values for the list dimension. Build a cdf.
            int begin = groupRows.first.getRow();
            int end = groupRows.second.getRow();
            arr = inPopGivenArray[pop] - 1;
            assert(arr >= 0 && arr < dimArray);
            total = 0;
            for(row = begin; row != end; ++row) {
                total += WEIGHTCOND(sortOrder[row], arr);
            }
            double cdf = 0.;
            // Signals "found, but all have zero probability"
            outIds[pop] = -2;
            for(row = begin; row != end; ++row) {
                cdf += WEIGHTCOND(sortOrder[row], arr);
                if(inRandom[pop]*total <= cdf) {
                    outIds[pop] = inIds[sortOrder[row]];
                    break;
                }
            }
        }
    }
    UNPROTECT(numProtected);
    return a_result;
}
#undef WEIGHTCOND
#undef INLIST

/*
 * Dimensions of pointer arguments:
 *
 * inList: numRows x numList
 */
#define INWEIGHT(r,a)  inWeight[(a) * numRows + (r)]
static SEXP
absEntropyList(SEXP a_inWeight)
{
    SEXP a_result;
    int numRows, dimArray;
    const double *inWeight = NULL;
    int numProtected = 0;

    MYPROTECT(a_result = NEW_NUMERIC(1));
    *REAL(a_result) = 0;

    ///
    // #1: Type conversions, and protection of input data.
    ///
    MYPROTECT(a_inWeight = AS_NUMERIC(a_inWeight));

    ///
    // #2: Dimension checks
    ///
    // Forbid more than two dimensions on weight.
    if(GET_DIM(a_inWeight) != R_NilValue &&
       GET_LENGTH(GET_DIM(a_inWeight)) > 2) {
        MYERROR("Weight (arg 1) cannot be higher than 2D");
    }
    // Require presence of 2D dimension on inWeight
    if(GET_DIM(a_inWeight) != R_NilValue &&
       GET_LENGTH(GET_DIM(a_inWeight)) == 2) {
        numRows = INTEGER(GET_DIM(a_inWeight))[0];
        dimArray = INTEGER(GET_DIM(a_inWeight))[1];
    } else {
        numRows = GET_LENGTH(a_inWeight);
        dimArray = 1;
    }

    ///
    // #3: get pointers to the actual data
    ///
    inWeight = REAL(a_inWeight);

    int row, arr;
    double total = 0.;
    for(row = 0; row < numRows; ++row) {
        for(arr = 0; arr < dimArray; ++arr) {
            total += INWEIGHT(row, arr);
        }
    }
    for(row = 0; row < numRows; ++row) {
        for(arr = 0; arr < dimArray; ++arr) {
            double w = INWEIGHT(row, arr);
            if( w != 0 ) {
                // Convert from count to probability
                w /= total;
                // Take entropy
                *REAL(a_result) += -w * log2(w);
            }
        }
    }
    UNPROTECT(numProtected);
    return a_result;
}
#undef INWEIGHT

static const R_CMethodDef cMethods[] = {
    {NULL, NULL, 0}
};

static const R_CallMethodDef callMethods[] = {
    {"ipfCollapseFromList", (DL_FUNC) &ipfCollapseFromList, 4},
    {"ipfExpandToList", (DL_FUNC) &ipfExpandToList, 4},
    {"montecarloConditionList", (DL_FUNC) &montecarloConditionList, 6},
    {"absEntropyList", (DL_FUNC) &absEntropyList, 1},
    {NULL, NULL, 0}
};

extern "C" {
void
R_init_ipf_list(DllInfo *info)
{
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
}

void
R_unload_ipf_list(DllInfo *info)
{
}
}
