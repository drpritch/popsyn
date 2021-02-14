#
# An early set of experiments from January 2008, trying to look at the
# effects of increasing number of variables on IPF fit. Data source was the
# business establishment database.
#

library('RODBC')
library('lattice')
library('vcd')
library('gtools')

source('ipf.R')
source('utils.R')

# Set random number seed
set.seed(0)

channel <- odbcConnect("PostgreSQL census", uid="david", case="tolower")
# Read in pum86 tables.
be <- list()
# 531 CTs in CD=3520 (Toronto)
# 1003 CTs in CMA=535 (Greater Toronto)
# 72 CSDs in CMAs (COZ)
#
# Removed n/a numemployees - we know almost nothing about these firms,
# usually including sales volume and sic code.
be$truth <- sqlQuery(channel, "SELECT infoca_id, csduid,
    employee_size_code as numemployees, sales_volume_code as salesvolume,
    sic_code as sic, year_first_appeared,
    credit_rating_code as creditrating, office_size_code as officesize
    FROM be WHERE cmauid > 0 AND employee_size_code > 0 ORDER BY infoca_id")
odbcClose(channel)

be$truth$csduid <- factor(be$truth$csduid)
be$truth$numemployees <- factor(be$truth$numemployees,
    levels = 1:11,
    labels = c('1-4', '5-9', '10-19', '20-49',
        '50-99', '100-249', '250-499', '500-999',
        '1000-4999', '5000-9999', '10000+'))
be$truth$numemployees <- collapse.factor(be$truth$numemployees,
    LevelMap(map=c(1:7,7,7,7,7), levels=1:7,
             labels=c(levels(be$truth$numemployees)[1:6], '250+')))
be$truth$salesvolume <- factor(be$truth$salesvolume + 1,
    levels = 1:12,
    labels = c('NA',
        '$0-0.5m', '$0.5-1.0m', '$1-2.5m', '$2.5-5m',
        '$5-10m', '$10-20m', '$20-50m', '$50-100m',
        '$100-500m', '$500-1000m', '$1000m+'))
be$truth$salesvolume <- collapse.factor(be$truth$salesvolume,
    LevelMap(map=c(1:9,9,9,9,9), levels=1:9,
             labels=c(levels(be$truth$salesvolume)[1:8], '$50m+')))

# The major SIC divisions
be$truth$sic_div <- cut(be$truth$sic/10000, 
    breaks=c(-Inf, 14, 17, 39, 49, 51, 59, 67, 89, 97, Inf),
    labels=c('Agriculture, forestry & fishing; mining',
             'Construction',
             'Manufacturing',
             'Transportation & pub. utilities',
             'Wholesale trade',
             'Retail trade',
             'Finance, insurance & real estate',
             'Services',
             'Public administration',
             'Nonclassifiable'))
#be$truth$sic <- factor(be$truth$sic)
be$truth$year_first_appeared[
    be$truth$year_first_appeared >= 1900 &
    be$truth$year_first_appeared < 1990] <- 1990
be$truth$age <- factor(pmax(1, be$truth$year_first_appeared - 1988),
    levels=1:18,
    labels=c('NA', '16+', '15', '14', '13', '12', '11', '10', '9', '8',
    '7', '6', '5', '4', '3', '2', '1', '0'))
levels(be$truth$creditrating) <- c('Excellent', 'Very Good', 'Good',
'Institution', 'NA')
levels(be$truth$officesize) <- c('NA', '1 pro', '2 pros', '3 pros',
    '4 pros', '5-9 pros', '10+ pros')


validation <- runif(nrow(be$truth)) >= .2
be$sample2b <- be$truth[!validation,]
be$validate <- be$truth[validation,]
be$pum <- be$sample2b[runif(nrow(be$sample2b)) < .1,]
be$pum <- be$pum[,-match('csduid', names(be$pum))]

be$margin1 <- list()
disclosure_control <- function(table) {
    # Random rounding
    result <- runif(length(table))
    dim(result) <- dim(table)
    dimnames(result) <- dimnames(table)
    result <- ifelse(result < .5, floor(table/5)*5, ceiling(table/5)*5)
    # Area suppression
    if(length(table) > 1) {
        assert(!('csduid' %in% names(dimnames(table))[-1]))
        if(names(dimnames(table))[1] == 'csduid') {
            nd = length(dim(table))
            if(nd==1) {
                result[margin.table2(table, 'csduid') < 40] <- 0
            } else if(nd == 2) {
                result[margin.table2(table, 'csduid') < 40,] <- 0
            }
        }
    }
    result
}
be$margin1$csduid <-
    disclosure_control(xtabs(~csduid, be$sample2b))
be$margin1$numemployees <-
    disclosure_control(xtabs(~numemployees, be$sample2b))
be$margin1$csduid_numemployees <-
    disclosure_control(xtabs(~csduid + numemployees, be$sample2b))
be$margin1$salesvolume <-
    disclosure_control(xtabs(~salesvolume, be$sample2b))
be$margin1$csduid_salesvolume <-
    disclosure_control(xtabs(~csduid + salesvolume, be$sample2b))
be$margin1$creditrating <-
    disclosure_control(xtabs(~creditrating, be$sample2b))
be$margin1$csduid_creditrating <-
    disclosure_control(xtabs(~csduid + creditrating, be$sample2b))
be$margin1$sic_div <-
    disclosure_control(xtabs(~sic_div, be$sample2b))
be$margin1$csduid_sic_div <-
    disclosure_control(xtabs(~csduid + sic_div, be$sample2b))
be$margin1$age <-
    disclosure_control(xtabs(~age, be$sample2b))
be$margin1$csduid_age <-
    disclosure_control(xtabs(~csduid + age, be$sample2b))
be$margin1$total <-
    disclosure_control(nrow(be$sample2b))

be$sample2b_xtab <- xtabs(
    formula = ~csduid + numemployees + salesvolume + creditrating + sic_div +
        age,
    data = be$sample2b)
be$validate_xtab <- xtabs(
    formula = ~csduid + numemployees + salesvolume + creditrating + sic_div +
        age,
    data = be$validate) * (nrow(be$sample2b) / nrow(be$validate))


allVars <- c('numemployees', 'salesvolume', 'creditrating', 'sic_div', 'age')

resultAll <- list()
for(nvars in 2:5) {

print('')
print(paste('# VARS:', nvars))

vars <- allVars[1:nvars]
result <- list()
#
# Perform synthesis using the fake census
#
result$pum_xtab <- xtabs(
    formula = as.formula(paste('~', paste(vars, collapse=' + '))),
    data = be$pum)
result$pum_xtab_2b <- ipf(
    constraintList = c(
        lapply(vars, function(x) { err4(be$margin1[[x]]) }),
        list(err4(be$margin1$total))),
    priorArray = result$pum_xtab,
    maxIterations = 100,
    tolerance = 1/100)
profile86i <- list()
profile86i$suppressed <- as.vector(be$margin1$csduid == 0)
# Replicate zero-nonzero pattern over all zones.
nnz <-
    array(data = rep(1 * (result$pum_xtab_2b > 0), length(be$margin1$csduid)),
          dim = c(dim(result$pum_xtab_2b), length(be$margin1$csduid)),
          dimnames = c(dimnames(result$pum_xtab_2b), dimnames(be$margin1$csduid)))
# Reorder to make zone the first dim.
nnz <- aperm(nnz, c(length(dim(result$pum_xtab_2b)) + 1, seq(dim(result$pum_xtab_2b))))
result$fitted <- list(
    beckmann = ipf(constraintList =
        c(
            lapply(vars, function(x) {
                errct(be$margin1[[paste('csduid_', x, sep='')]])
            }),
            list(IpfConstraint(result$pum_xtab_2b)),
            list(
                errct(be$margin1$csduid),
                err4(be$margin1$total)
            )
        ),
        dimnameList = c(dimnames(be$margin1$csduid),
                        dimnames(result$pum_xtab_2b)),
        maxIterations = 100,
        tolerance = 1/100),
    pritchard2 = ipf(constraintList =
        c(
            lapply(vars, function(x) {
                errct(be$margin1[[paste('csduid_', x, sep='')]])
            }),
            lapply(multi.margin.table(result$pum_xtab_2b, vars, 2:1),
                function(x) { IpfConstraint(x) }),
            list(
                errct(be$margin1$csduid),
                err4(be$margin1$total)
            )
        ),
        priorArray = nnz,
        maxIterations = 100,
        tolerance = 1/100)
)
if (length(dim(result$pum_xtab_2b)) > 2) {
    result$fitted <- c(result$fitted, list(
        pritchard3 = ipf(constraintList =
            c(
                lapply(vars, function(x) {
                    errct(be$margin1[[paste('csduid_', x, sep='')]])
                }),
                lapply(multi.margin.table(result$pum_xtab_2b, vars, 3:1),
                    function(x) { IpfConstraint(x) }),
                list(
                    errct(be$margin1$csduid),
                    err4(be$margin1$total)
                )
            ),
            priorArray = nnz,
            maxIterations = 100,
            tolerance = 1/100)
        ))
}

evaluate <- function(table, table2 = be$validate_xtab) {
    assert(names(dimnames(table2)) == names(dimnames(table)))
    assert(abs(sum(table) - sum(table2)) <= 5)
    # Take all 3-way marginals
    delta <- multi.margin.table(table2 - table, names(dimnames(table)), 1:3)
    delta <- lapply(delta, function(x) {
        result <- list()
        # Keep only the ones involving csduid.
        # Take L1/L2/Linf norms for each marginal table.
        {#if('csduid' %in% names(dimnames(x)) & length(dim(x)) > 2) {
            result <- list(c(
                sum(abs(as.vector(x))) / length(x),
                sqrt(sum(as.vector(x)^2) / length(x)),
                max(abs(as.vector(x)))
            ))
            # Give it a name
            names(result) <- paste(names(dimnames(x)), collapse = ':')
        }
        result
    })
    # Build into a table.
    delta <- t(as.data.frame(unlist(delta, recursive=FALSE)))
    colnames(delta) <- c('L1', 'L2', 'Linf')

    as.data.frame(delta)
}
# It's not impossible that we'd see better fit for observed marginals too -
# there are fewer constraints when using my algorithm. However, I'm
# focusing here on testing generalization - how well does it predict things
# that it doesn't know anything about?
#
# That said, it hasn't seen the 100% estimates of several of the 1D
# marginals, or of the PUM. Hmm.

result$eval <- lapply(result$fitted, function(x) {
    evaluate(x, margin.table2(be$validate_xtab, c('csduid', vars))) })
#print(list(L1 = sum(abs(delta)), L2 = sqrt(sum(delta^2)), Linf = max(abs(delta))))

resultAll <- c(resultAll, list(result))
}

#
# RESULTS
# -------
#
# Population size: 113570
# Margins: 20%      22714
# PUM:      2%       2271
# Zones: 531 CTs in Toronto (CD 3520)
# 
# Total df:  30279744
# Margin df:    24380
#
# Error test: full matrix (30m cells)
#                                            PUM   | Error
# Margins   RoundFix   PUM                   df    | L1      L2      Linf  
# -------------------------------------------------+-----------------------
#   1D         Y       All-way (Beckmann)    38080 | 30314   103.25  12.57
#   1D         Y       3-way (Pritchard II)   3280 | 30386   102.60  12.58 
#   1D         Y       2-way (Pritchard II)    799 | 31746   102.28  11.36
#
#
#
# Population size: 313340
# Margins: 20%      62675
# PUM:      2%       6293
# Zones: 72 CSDs in Central Ontario Zone
# 
# Total df:  4105728
# Margin df:    3024
#

#                                            PUM   | Error
# Margins   RoundFix   PUM                   df    | L1      L2      Linf  
# -------------------------------------------------+------------------------
# Error test: full matrix (30m cells)
#   1D         Y       All-way (Beckmann)    38080 | 41817   485.33  229.42
#   1D         Y       2-way (Pritchard II)    799 | 44701   552.47  238.37 
#   1D         Y       2-way (Pritchard II)    799 | 44321   544.59  227.70
#      (tol=0.1; 0.01 for last two entries)
#
# Error test: geo x numemp x salesvol (5184 cells)
#   1D         Y       All-way (Beckmann)    38080 |  7013   323.95   88.54
#   1D         Y       2-way (Pritchard II)    799 |  7087   333.45   82.39
#     (tol=0.001)
#
# Error test: geo x numemp x age (10368 cells)
#   1D         Y       All-way (Beckmann)    38080 | 12555   509.25  221.85
#   1D         Y       2-way (Pritchard II)    799 | 12447   505.91  223.81
#     (tol=0.001)
#
#
# BUUUUT - Beckmann PUM NNZ = 1795 (vs. Pritchard 846). So, actually much
# fewer df.



# Show sparsity of results
#
# t(as.data.frame(lapply(resultAll, function(x) {
#   c(prod(dim(x$fitted$beckmann)),
#     sum(x$fitted$beckmann > 0), sum(margin.table(x$fitted$beckmann, -1) > 0),
#     sum(x$fitted$beckmann)) })))


# merger <- function(x,y) {
#   result <- merge(x,y,all=T, by='row.names');
#   rownames(result) <- result$Row.names;
#   result$Row.names<-NULL; result;
# }
# merger(merger(resultAll[[1]]$eval$beckmann, resultAll[[2]]$eval$beckmann),
#        merger(resultAll[[3]]$eval$beckmann, resultAll[[4]]$eval$beckmann))
# [,2 + c(1,4,7,10)]

