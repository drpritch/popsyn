#
# Second stage of synthesis: monte carlo synthesis from fitted tables
# Input: result/latest/fitGeog-{cma}.Rdata
# Output: result/latest/pop-{cma}.Rdata
#

library('gtools')
library('gdata')

synthesis2 <- list()
synthesis2$startTime <- list(full = proc.time())
synthesis2$elapsedTime <- list()

source('ipf.R')
source('ipf_list.R')
source('utils.R')

if(!exists('cmaname')) {
    cmaname <- 'Toronto'
}
cmacodes <- list(Toronto = 535,
                 HSK = c(537, 539, 541),
                 Oshawa = 532)[[cmaname]]
source('pum86.R')

print('Loading PUMS')
pum86 <- switch(cmaname,
    Toronto = makePUMS(cmacodes_i = cmacodes, cmacodes_f = cmacodes,
        cmacodes_h = cmacodes),
    HSK = makePUMS(cmacodes_i = 599, cmacodes_f = cmacodes,
        cmacodes_h = cmacodes),
    # We have no i or f data for Oshawa. Use HSK and Hamilton instead.
    Oshawa = makePUMS(cmacodes_i = 599, cmacodes_f = 537,
        cmacodes_h = cmacodes))

print('Loading fitted tables')
load(paste('../results/latest/fitGeog-', cmaname, '.Rdata', sep=''))

# Rename a few columns to enable connection of tables.
# TODO: look out! Tenurh applies to *some* individual(s) in the household,
# while tenure applies to particular family's tenure status.
colnames(fitGeog$d$fitted.values)[colnames(fitGeog$d$fitted.values) == 'tenurh'] <- 'tenure'
#colnames(fitGeog$d$fitted.values)[colnames(fitGeog$d$fitted.values) == 'payh'] <- 'payf'

# Replace childde with childd and childe vars.
fitGeog$f$fitted.values$childd <- factor(c(1,1,1,2,2,2,3,3,3)[as.integer(fitGeog$f$fitted.values$childde)],
    levels=1:3, labels=c('0', '1', '2+'))
fitGeog$f$fitted.values$childe <- factor(c(1,2,3,1,2,3,1,2,3)[as.integer(fitGeog$f$fitted.values$childde)],
    levels=1:3, labels=c('0', '1', '2+'))
fitGeog$f$fitted.values$childde <- NULL


# Load census/TTS zone mapping table.
source('censusTTS.R')
ctcode_tts96_map <- makeCensusTTSMap(margin.table2(fitGeog$d$fitted.values$weight, 'ctcode'))
# Cross-tab of list.
ctcode_tts96_map_tab <- xtabs(weight ~ ctcode+tts96, ctcode_tts96_map)
# Add a near-zero probability column for "NA". It'll be used by any ctcode
# that has no known TTS zone (e.g., those outside GTAH)
ctcode_tts96_map_tab <- cbind(ctcode_tts96_map_tab,
    array(1e-7,
          dim = c(nrow(ctcode_tts96_map_tab),1),
          dimnames = list(ctcode = dimnames(ctcode_tts96_map_tab)$ctcode,
                          tts96 = -1))
)
names(dimnames(ctcode_tts96_map_tab)) <- c('ctcode', 'tts96')



pop <- list()
# Global variable if present, otherwise zero.
if(exists('popseed')) {
    pop$seed <- popseed
} else {
    pop$seed <- 0
}
set.seed(pop$seed)

print('Synthesizing Dwellings')

synthesis2$startTime$d <- proc.time()
# Add id dimension to table, for montecarlo
names(dimnames(fitGeog$d$fitted.values$weight))[[1]] <- 'id'
dimnames(fitGeog$d$fitted.values$weight)$id <- fitGeog$d$fitted.values$id
pop$d <- montecarlo(fitGeog$d$fitted.values$weight)
gc()
# Copy all columns over to pop - including id.
pop$d <- cbind(
    pop$d, fitGeog$d$fitted.values[match(pop$d$id, fitGeog$d$fitted.values$id),
        setdiff(colnames(fitGeog$d$fitted.values),
                c('weight', 'id',
                  # ... except these useless ones that were just for IPF
                  'hhnuef', 'pperroom'))])
# Now get rid of the useless id. (pumhId and hhnucf contain all the
# necessary information.)
pop$d$id <- NULL
# Kill off id-based rownames.
rownames(pop$d) <- NULL

pop$d$tts96 <- montecarlo_condition(ctcode_tts96_map_tab, pop$d)$tts96
#pop$d$tts96 <- montecarlo_condition_list(ctcode_tts96_map, pop$d)$tts96
if(cmaname=='HSK') {
    # Throw away non-Hamilton dwellings.
    pop$d <- pop$d[pop$d$tts96 != -1,]
}
# Throw away some data to save on RAM...
fitGeog$d$fitted.values <- NULL
synthesis2$elapsedTime$d <- proc.time() - synthesis2$startTime$d

# And add a real id.
pop$d$id <- seq(nrow(pop$d))


synthesis2$startTime$f <- proc.time()
backup_seed <- .Random.seed
# We want to synthesize cfsize using just this table:
xtab_cfsize <- xtabs(weight ~ cfsize + hhsize + hhnumcf, pum86$f_weighted_i)
# Drop hhnumcf='0' level, which doesn't exist in the families.
xtab_cfsize <- xtab_cfsize[,,-1]
# However, because this doesn't (and shouldn't) include room / tenure / payf,
# some cfsizes get generated that don't have corresponding
# PUMF families with matching {HHCFLINK}+cfsize values.
# To deal with this, we build a fake higher-order table, with just the 0/1
# pattern correct.
xtab_fitgeog <- margin.list(fitGeog$f$fitted.values,
                            c('cfsize', 'hhnumcf', 'tenure', 'room')) #,'payf'))
xtab_fitgeog <- ifelse(xtab_fitgeog > 0, 1, 0)
# Add another dimension for hhsize, spreading equally across.
xtab_fitgeog <- ipf(constraintList=list(IpfConstraint(xtab_fitgeog)),
           dimnameList=c(dimnames(xtab_fitgeog),
           list(hhsize = dimnames(xtab_cfsize)$hhsize)))
# Fit 0/1 sparsity pattern to the cfsize table.
xtab_cfsize <- ipf(constraintList=list(IpfConstraint(xtab_cfsize)),
           priorArray=xtab_fitgeog)
remove(xtab_fitgeog)

# Restore random seed state - ipf can mess with it, sometimes.
.Random.seed <- backup_seed


# FIXME: Top two priorities for montecarlo_condition_list
# 1) signal NaN when the pivoting attributes have non-zero entries in the
# PUMF, but zero probability.
# 2) verify that levels line up

print('Synthesizing Family #1')
##
# Synthesize Family #1
##
# Add first family
dsubset <- pop$d[pop$d$hhnumcf!='0',]
# Drop the '0' level to match the family hhnumcf variable.
dsubset$hhnumcf <- dsubset$hhnumcf[,drop=TRUE]
# Use a random number draw to synthesize the first family's size.
# TODO: find the best possible explanatory factors for cfsize.
assert(is.null(dsubset$cfsize))
dsubset$cfsize <- montecarlo_condition(xtab_cfsize, dsubset)$cfsize
temp <- data.frame(
    id = seq(nrow(dsubset)),
    fid = montecarlo_condition_list(fitGeog$f$fitted.values, dsubset),
    householdId = dsubset$id)
gc()
dsubset$cfsize <- NULL
assert(!is.na(temp$fid))
pop$d$familyId1 <- NA
pop$d$familyId1[pop$d$hhnumcf!='0'] <- temp$id
pop$f <- temp


print('Synthesizing Family #2')
##
# Synthesize Family #2
##
# Add second family
# TODO: take into account family #1's size.
dsubset <- dsubset[dsubset$hhnumcf=='2+',]
# *Don't* drop the "1" level from hhnumcf - fitGeog$f$fitted.values does include that
# level.
assert(is.null(dsubset$cfsize))
dsubset$cfsize <-
    montecarlo_condition(xtab_cfsize, dsubset)$cfsize
temp <- data.frame(
    id = seq(nrow(dsubset)) + max(pop$f$id),
    fid = montecarlo_condition_list(fitGeog$f$fitted.values, dsubset),
    householdId = dsubset$id)
assert(!is.na(temp$fid))
dsubset$cfsize <- NULL
pop$d$familyId2 <- NA
pop$d$familyId2[pop$d$hhnumcf=='2+'] <- temp$id
pop$f <- rbind(pop$f, temp)

remove(dsubset)
remove(temp)
remove(xtab_cfsize)

# Copy all columns over to pop
pop$f <- cbind(
    pop$f, fitGeog$f$fitted.values[match(pop$f$fid, fitGeog$f$fitted.values$id),
        setdiff(colnames(fitGeog$f$fitted.values),
                c('weight', 'id',
                  # ... except these useless ones that all already present
                  # in the household.
                  'hhnumcf', 'room', 'payf'))])
# Kill temporary fid. pumfid+hhnumcf is all we need.
pop$f$fid <- NULL

# Throw away some data to save on RAM...
fitGeog$f$fitted.values <- NULL

# Kill off some now-unneeded columns on the dwelling, to save on RAM...
pop$d <- pop$d[,c('id', 'pumhId', 'ctcode', 'tts96',
                  'hhsize', 'familyId1', 'familyId2')]
synthesis2$elapsedTime$f <- proc.time() - synthesis2$startTime$f



synthesis2$startTime$i1 <- proc.time()
# Our family definition has a problem with its children categories: for
# each child age category (e.g., childa), the final category is an
# "overflow" category labelled "X+" where the family has X or more children
# of that age. While this is the only data available, we need an accurate
# number to do the synthesis.
#
# We know how many children are missing (assuming no 9+ child families).
# We don't know which ages the missing children belong to, unless the
# family only has one overflow category. It's fairly rare that a family
# has two overflow categories and has a non-zero delta, but it does
# happen - so we randomly assign the delta to overflow categories.

# The one downside of this procedure is that we don't incorporate two
# pieces of data from the individual population:
#
# 1) the number of individuals with age 15+ and "child" cfstat in the
#    CF86A04 table, by census tract.
# 2) the number of persons with age <15 in the profile table (no "child"
#    cfstat to ensure that they're not orphans etc., though)
#
# Those crosschecks would be useful to ensure we get the correct population
# size. Still, it's a fairly small error, I suspect.
finalizeChildren <- function(popf) {
    # Factor-to-int conversion for the child categories.
    f2i <- function(x) { as.integer(x) - 1 }
    nuchild_delta = f2i(popf$nuchild) - (f2i(popf$childa) +
        f2i(popf$childb) + f2i(popf$childc) + f2i(popf$childd) +
        f2i(popf$childe))
    # Find which child column could conceivably take more children.
    childCols <- c('childa','childb','childc','childd','childe')
    overflow <- (popf[,childCols] == t(array(c('2+','3+','2+','2+','2+'),
                                             dim = c(5, nrow(popf)))))
    overflowCount <- margin.table2(overflow, 1)
    # Convert them to integers - keeping in mind that the last category may be
    # in overflow.
    popf$childa <- as.integer(popf$childa) - 1
    popf$childb <- as.integer(popf$childb) - 1
    popf$childc <- as.integer(popf$childc) - 1
    popf$childd <- as.integer(popf$childd) - 1
    popf$childe <- as.integer(popf$childe) - 1

    while(max(nuchild_delta) > 0) {
        subset <- (nuchild_delta > 0)
        # This converts it to a list of lists of column ids.
        overflowColNos <- apply(overflow[subset,, drop=FALSE], 1, 
            function(x) { as.list(which(x)) })
        assert(is.list(overflowColNos))

        # Randomly allocate this child to one of the overflow columns.
        select <- ceiling(runif(sum(subset)) * overflowCount[subset])
        for(i in seq(select)) {
            # Convert from index into overflow column list to the actual
            # column number.
            select[i] <- overflowColNos[[i]][[ select[i] ]]
        }
        # Add the child to the overflow column of the family.
        for(i in seq(childCols)) {
            subset2 <- select==i
            popf[subset, childCols[i]][subset2] <-
                popf[subset, childCols[i]][subset2] + 1
        }
        # Subtract one from the remaining children to be allocated.
        nuchild_delta[subset] <- nuchild_delta[subset] - 1
    }
    popf
}
print('Finalizing children')
pop$f <- finalizeChildren(pop$f)
pop$f$cfsize <-
    pop$f$childa + pop$f$childb + pop$f$childc + pop$f$childd +
    pop$f$childe + ifelse(pop$f$cfstruc=='Husband-wife', 2, 1)

print('Synthesizing wives/single mothers')
# Take family info about female parent / wife:
fsubset <- renameFrame(
    pop$f[pop$f$cfstruc!='Lone male parent',],
    fromNames=c('lfactf', 'agef'),
    toNames=c('lfact', 'agep'))
fsubset$ctcode <- pop$d$ctcode[match(fsubset$householdId, pop$d$id)]
# Map family "cfstruc" to individual "cfstat"
fsubset$cfstat <- factor(
    ifelse(fsubset$cfstruc=='Husband-wife', 1, 2),
    levels=seq(nlevels(fitGeog$i$fitted.values$cfstat)),
    labels=levels(fitGeog$i$fitted.values$cfstat))
fsubset$sexp <- factor(which(levels(fitGeog$i$fitted.values$sexp) == 'Female'),
    levels=seq(nlevels(fitGeog$i$fitted.values$sexp)),
    labels=levels(fitGeog$i$fitted.values$sexp))
assert(fsubset$cfstat[fsubset$cfstruc=='Husband-wife']
        == 'Partner')
assert(fsubset$cfstat[fsubset$cfstruc=='Lone female parent']
        == 'Lone parent')
fsubset$cfstruc <- NULL
result <- montecarlo_condition_list(fitGeog$i$fitted.values, fsubset)
gc()
result <- data.frame(
    id = seq(length(result)),
    familyId = fsubset$id,
    iid = as.integer(levels(result))[result])
pop$f$indId_f <- NA
pop$f$indId_f[pop$f$cfstruc!='Lone male parent'] <- result$id
pop$i <- result
remove(fsubset)
remove(result)
gc()

print('Synthesizing husbands/single fathers')
# Take family info about male parent / husband:
fsubset <- renameFrame(
    pop$f[pop$f$cfstruc!='Lone female parent',],
    fromNames=c('lfactm', 'agem'),
    toNames=c('lfact', 'agep'))
fsubset$ctcode <- pop$d$ctcode[match(fsubset$householdId, pop$d$id)]
# Map family "cfstruc" to individual "cfstat"
fsubset$cfstat <- factor(
    ifelse(fsubset$cfstruc=='Husband-wife', 1, 2),
    levels=seq(nlevels(fitGeog$i$fitted.values$cfstat)),
    labels=levels(fitGeog$i$fitted.values$cfstat))
fsubset$sexp <- factor(which(levels(fitGeog$i$fitted.values$sexp) == 'Male'),
    levels=seq(nlevels(fitGeog$i$fitted.values$sexp)),
    labels=levels(fitGeog$i$fitted.values$sexp))
assert(fsubset$cfstat[fsubset$cfstruc=='Husband-wife'] ==
    'Partner')
assert(fsubset$cfstat[fsubset$cfstruc=='Lone male parent'] ==
    'Lone parent')
fsubset$cfstruc <- NULL
result <- montecarlo_condition_list(fitGeog$i$fitted.values, fsubset)
gc()
result <- data.frame(
    id = nrow(pop$i) + seq(length(result)),
    familyId = fsubset$id,
    iid = as.integer(levels(result))[result])
pop$f$indId_m <- NA
pop$f$indId_m[pop$f$cfstruc!='Lone female parent'] <- result$id
pop$i <- rbind(pop$i, result)
remove(fsubset)
remove(result)

# Remove now-redundant demographic info about mother/father.
# ... and nuchild and tenure, while we're at it.
pop$f <- pop$f[,setdiff(colnames(pop$f), c('agef', 'lfactf', 'agem', 'lfactm',
                                           'nuchild', 'tenure'))]


print('Synthesizing children 15+')
childCols <- c('childc', 'childd', 'childe')
given <- NULL
temp <- margin.list(fitGeog$i$fitted.values, c('cfstat', 'agep', 'ctcode'))
names(dimnames(temp))[[3]] <- 'ctcode'
for(i in seq(childCols)) {
    count <- pop$f[,childCols[i]]
    while(any(count) > 0) {
        subset <- count > 0
        newInds <-
            data.frame(
                familyId = pop$f$id[subset, drop=FALSE],
                ctcode = pop$d$ctcode[match(pop$f$householdId[subset],
                                      pop$d$id), drop=FALSE],
                cfstat = factor(
                    ifelse(pop$f$cfstruc[subset, drop=FALSE]=='Husband-wife',
                           which(levels(fitGeog$i$fitted.values$cfstat) ==
                              'Child w/ parents'),
                           which(levels(fitGeog$i$fitted.values$cfstat) ==
                              'Child w/ lone parent')),
                    levels=seq(nlevels(fitGeog$i$fitted.values$cfstat)),
                    labels=levels(fitGeog$i$fitted.values$cfstat)),
                agep = factor(which(levels(fitGeog$i$fitted.values$agep) == '15-17'),
                    levels=seq(nlevels(fitGeog$i$fitted.values$agep)),
                    labels=levels(fitGeog$i$fitted.values$agep))
            )
        # Fix ages.
        if(childCols[i] == 'childc') {
            # Already set to '15-17'
        } else if(childCols[i] == 'childd') {
            # Split up the 18-24 age group into 18-19 and 20-24 groups.
            oldInds <- newInds
            newInds$agep <- NULL
            newInds <- montecarlo_condition(
                aslice(temp, list(agep=c('18-19', '20-24')), drop=FALSE),
                newInds)
            newInds$familyId <- oldInds$familyId
            newInds$id <- NULL
        } else {
            # Split up the 25+ age group.
            # TODO: child cannot be older than parent!
            oldInds <- newInds
            newInds$agep <- NULL
            newInds <- montecarlo_condition(
                aslice(temp,
                    list(agep=c('25-34', '35-44', '45-54', '55-64', '65+')),
                    drop=FALSE),
                newInds)
            newInds$familyId <- oldInds$familyId
            newInds$id <- NULL
        }
        given <- rbind(given, newInds)
        count[subset] <- count[subset] - 1
    }
}
remove(count)
remove(newInds)
remove(oldInds)
remove(subset)
remove(temp)
assert(is.factor(given$agep))
result <- data.frame(
    id = nrow(pop$i) + seq(nrow(given)),
    familyId = given$familyId,
    iid = montecarlo_condition_list(fitGeog$i$fitted.values, given)
)
# TODO: give each family a list of children ids.
pop$i <- rbind(pop$i, result)
pop$f <- pop$f[,setdiff(colnames(pop$f), childCols)]
remove(childCols)
remove(given)
remove(result)
gc()

print('Synthesizing children 0-15')
childCols <- c('childa', 'childb')
agedesc <- list(childa = '0-5', childb = '6-14')
pum86$i_young$weight <-
    array(1, dim = c(nrow(pum86$i_young), ncol(fitGeog$i$fitted.values$weight)),
    dimnames = list(NULL, ctcode = dimnames(fitGeog$i$fitted.values$weight)$ctcode))
given <- NULL
for(i in seq(childCols)) {
    count <- pop$f[,childCols[i]]
    while(any(count) > 0) {
        subset <- count > 0
        given <- rbind(given,
            data.frame(
                familyId = pop$f$id[subset],
                ctcode = pop$d$ctcode[match(pop$f$householdId[subset],
                                      pop$d$id)],
                cfstat = factor(
                    ifelse(pop$f$cfstruc[subset]=='Husband-wife',
                           which(levels(pum86$i_young$cfstat) ==
                              'Child w/ parents'),
                           which(levels(pum86$i_young$cfstat) ==
                              'Child w/ lone parent')),
                    levels=seq(nlevels(pum86$i_young$cfstat)),
                    labels=levels(pum86$i_young$cfstat)),
                agep = factor(
                    which(levels(pum86$i_young$agep) == agedesc[childCols[i]]),
                    levels=seq(nlevels(pum86$i_young$agep)),
                    labels=levels(pum86$i_young$agep))
            ))
        count[subset] <- count[subset] - 1
    }
}
remove(count)
remove(subset)
assert(is.factor(given$agep))
result <- data.frame(
    id = seq(nrow(given)),
    familyId = given$familyId,
    # Since we're working directly from the PUM, we get back pum ids here,
    # not iids.
    pumiId = montecarlo_condition_list(pum86$i_young, given)
)
# We have to be careful with the young individuals - they have NA for
# almost all attributes, and a totally different set of levels (e.g., for
# agep) from other individuals.
#
# To handle this, we keep them in a separate list for now.
pop$i_young <- result

pop$f <- pop$f[,setdiff(colnames(pop$f), childCols)]
remove(childCols)
remove(given)
gc()

pop$i$householdId <- NULL
pop$i_young$householdId <- NULL
synthesis2$elapsedTime$i1 <- proc.time() - synthesis2$startTime$i1




synthesis2$startTime$i2 <- proc.time()
pop$d$nfsize <- as.integer(pop$d$hhsize) -
    ifelse(!is.na(pop$d$familyId1),
           pop$f$cfsize[match(pop$d$familyId1, pop$f$id)],
           0) -
    ifelse(!is.na(pop$d$familyId2),
           pop$f$cfsize[match(pop$d$familyId2, pop$f$id)],
           0)
#
# About nfsize: on a sample run, we got these results,
# 
# For hhsize != '8+':
#       hhnumcf
# nfsize      0      1     2+
#    -3       0      0    187
#    -2       0      0   1568
#    -1       0      0   3474
#    0        0 759967  12776
#    1   257238  76050   4903
#    2    48589   9499   2141
#    3    10862   2057    447
#    4     2189    436      0
#    5      349     31      0
#    6       50      0      0
#    7       24      0      0
#
# The negative values are clear errors, but they only happen in multifamily
# households. They occur because we currently don't constrain the two
# family sizes together - their sizes are synthesized independently, and
# can sometimes synthesize too many people.

# For hhsize == '8+':
#       hhnumcf
# nfsize      0      1     2+
#    -12      0      0      5
#    -10      0      0     12
#    -9       0      0      3
#    -8       0      0     26
#    -7       0      0     45
#    -6       0      0     67
#    -5       0      0    126
#    -4       0      0    196
#    -3       0      0    244
#    -2       0    340    347
#    -1       0    535    511
#    0        0    436    630
#    1        0    799    580
#    2        0    357    481
#    3        0    254    310
#    4        0    134    163
#    5        0     43      0
#    6        0    188      0
#
# nfsize is calculated by treating 8+ as equal to eight, so many of the
# negatives here are not actually errors - the prediction was for an 8+ person
# household, and we got that. However, we don't actually force a valid
# distribution of sizes for the 8+ category, so the results probably aren't
# perfect.

print('Synthesizing lone individuals')
given <- data.frame(
    householdId = pop$d$id[pop$d$hhsize==1],
    ctcode = pop$d$ctcode[pop$d$hhsize==1],
    cfstat = factor(6, seq(nlevels(fitGeog$i$fitted.values$cfstat)),
                    levels(fitGeog$i$fitted.values$cfstat)))
result <- data.frame(
    id = max(pop$i$id) + seq(nrow(given)),
    familyId = NA,
    iid = montecarlo_condition_list(fitGeog$i$fitted.values, given),
    householdId = given$householdId)

print('Synthesizing non-family persons')
count <- ifelse(pop$d$hhsize!=1, pop$d$nfsize, 0)
# Split non-family count up into young (<15) and regular (15+) counts.
# Roughly 4% of non-lone non-family persons are below age 15.
# TODO: this doesn't take into account zonal variations... we could use
# fitGeog$i to figure out zonal effects.
prob <- c(sum(pum86$i_young$cfstat== 'Non-CF person w/ others'),
          sum(pum86$i$cfstat== 'Non-CF person w/ others'))
prob <- prob[1] / sum(prob)
countYoung <- count * 0
countNew <- count * 0
while(any(count > 0)) {
    r <- runif(length(count))
    countYoung <- countYoung + ifelse((r < prob) & (count > 0), 1, 0)
    countNew <- countNew + ifelse((r >= prob) & (count > 0), 1, 0)
    count <- count - 1
}

while(any(countNew > 0)) {
    given <- data.frame(
        householdId = pop$d$id[countNew > 0],
        ctcode = pop$d$ctcode[countNew > 0],
        cfstat = factor(5, seq(nlevels(fitGeog$i$fitted.values$cfstat)),
                        levels(fitGeog$i$fitted.values$cfstat)))
    result <- rbind(result,
        data.frame(
            id = max(result$id) + seq(nrow(given)),
            familyId = NA,
            iid = montecarlo_condition_list(fitGeog$i$fitted.values, given),
            householdId = given$householdId))
    countNew <- countNew - 1
}
pop$i$householdId <- NA
pop$i <- rbind(pop$i, result)

# Young non-family persons
while(any(countYoung) > 0) {
    subset <- countYoung > 0
    given <- rbind(given,
        data.frame(
            householdId = pop$d$id[countYoung > 0],
            ctcode = pop$d$ctcode[countYoung > 0],
            cfstat = factor(5, seq(nlevels(pum86$i_young$cfstat)),
                            levels(pum86$i_young$cfstat))))
    countYoung[subset] <- countYoung[subset] - 1
}
result <- data.frame(
    id = max(pop$i_young$id) + seq(nrow(given)),
    familyId = NA,
    # Since we're working directly from the PUM, we get back pum ids here,
    # not iids.
    pumiId = montecarlo_condition_list(pum86$i_young, given),
    householdId = given$householdId
)
pop$i_young$householdId <- NA
pop$i_young <- rbind(pop$i_young, result)
synthesis2$elapsedTime$i2 <- proc.time() - synthesis2$startTime$i2




synthesis2$startTime$i3 <- proc.time()
print('Synthesizing non-dwelling persons')
isubset <- fitGeog$i$fitted.values$weight[fitGeog$i$fitted.values$cfstat=='NA',]
names(dimnames(isubset))[[1]] <- 'iid'
dimnames(isubset)$iid <- fitGeog$i$fitted.values$id[fitGeog$i$fitted.values$cfstat=='NA']
result <- montecarlo(isubset)
result <- data.frame(
    id = seq(nrow(result)),
    iid = result$iid,
    ctcode = result$ctcode
)
remove(isubset)
pop$i_collective <- result
# TODO: Need to also add non-dwelling persons below age 15. (Roughly 7.5%
# of 15+ total.)
synthesis2$elapsedTime$i3 <- proc.time() - synthesis2$startTime$i3

pop$i <- cbind(
    pop$i, fitGeog$i$fitted.values[match(pop$i$iid, fitGeog$i$fitted.values$id),
        setdiff(colnames(fitGeog$i$fitted.values),
                c('weight', 'id',
                  # ... except these useless ones that all already present
                  # in the family
                  'cfstat'))])
pop$i$iid <- NULL
pop$i_collective <- cbind(
    pop$i_collective, fitGeog$i$fitted.values[match(pop$i_collective$iid, fitGeog$i$fitted.values$id),
        setdiff(colnames(fitGeog$i$fitted.values),
                c('weight', 'id',
                  # ... except these useless ones that all already present
                  # in the family
                  'cfstat'))])
pop$i_collective$iid <- NULL
# And add a TTS zone
pop$i_collective$tts96 <-
    montecarlo_condition(ctcode_tts96_map_tab, pop$i_collective)$tts96


synthesis2$elapsedTime$full <- proc.time() - synthesis2$startTime$full

print('Saving')
synthesis2$startTime <- NULL
save(synthesis2, file='../results/latest/synthesis2-times.Rdata')

pop$i <- pop$i[,c('id', 'pumiId', 'familyId', 'householdId')]
pop$i_collective <- pop$i_collective[,c('id', 'pumiId', 'ctcode', 'tts96')]
pop$f <- pop$f[,c('id', 'pumfId', 'householdId')]
pop$d <- pop$d[,c('id', 'pumhId', 'ctcode', 'tts96')]

# Last stage: combine individual populations, as desired.
# i, i_young and i_collective have been independent populations with their
# own ids, to date.
pop$i_young$id <- pop$i_young$id + max(pop$i$id)
pop$i <- rbind(pop$i, pop$i_young)
write.csv(pop$d, file=paste('../results/latest/pop_d-', cmaname,
                            '-s', formatC(pop$seed, width=3, flag='0'),
                            '.csv', sep=''),
          row.names=FALSE, na='-1', quote=FALSE)
write.csv(pop$f, file=paste('../results/latest/pop_f-', cmaname,
                            '-s', formatC(pop$seed, width=3, flag='0'),
                            '.csv', sep=''),
          row.names=FALSE, na='-1', quote=FALSE)
write.csv(pop$i, file=paste('../results/latest/pop_i-', cmaname,
                            '-s', formatC(pop$seed, width=3, flag='0'),
                            '.csv', sep=''),
          row.names=FALSE, na='-1', quote=FALSE)
write.csv(pop$i_collective, file=paste('../results/latest/pop_ic-', cmaname,
                                       '-s', formatC(pop$seed,width=3,flag='0'),
                                       '.csv', sep=''),
          row.names=FALSE, na='-1', quote=FALSE)
print('DONE!')

