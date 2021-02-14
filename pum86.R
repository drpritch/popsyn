#
# pum86.R
#
# Load the PUM tables for synthesis from postgres. The raw data there just
# uses numeric values for the categories; here, we name and collapse the
# categories in preparation for synthesis.
#
# The collapsing is done in anticipation of the marginal tables: usually,
# we collapse categories unless they are separated in a margin somewhere.
#

source('LevelMap.R')
source('collapse.R')
source('utils.R')

maps <- list()
makePUMS <- function(cmacodes_i = 535, cmacodes_f = cmacodes_i,
                     cmacodes_h = cmacodes_i) {

inputDataPUMS <- function(tablename, columns, cmacodes, rename = NULL) {
    result <- inputData(tablename, c(columns, 'prov', 'cmapust'),
        sortColumns='id',
        whereExpression = paste('prov=35 AND ',
                                makeOrQuery(field='cmapust', values=cmacodes),
                                sep=''))
    result <- result[result$prov==35 & result$cmapust %in% cmacodes,]
    if(!is.null(rename)) {
        result <- renameFrame(result, fromNames=rename$from, toNames=rename$to)
    }
    result
}

pum86 <- list()
# Read in pum86 tables.
pum86$i <- inputDataPUMS('census.pum_i_1986_ontario',
    c('id', 'agep', 'sexp', 'hhclass', 'htype', 'unitsp', 'efstat',
      'efsize', 'cfstat', 'lfact', 'occ81p', 'ind80p', 'hlosp', 'totincp',
      'wagesp', 'selfip', 'room', 'tenurp', 'rcondp'), cmacodes_i)

# Fake economic family PUM - derived from individuals who are observed to
# belong to economic families.
#
# One weird thing: there are two records with tenurp=0, for no discernable
# reason.
pum86$ef <- inputDataPUMS('census.pum_i_1986_ontario',
    c('id', 'efsize', 'htype', 'unitsp', 'efstat', 'room', 'tenurp', 'hhclass',
      'rcondp'), cmacodes_i,
    rename=list(from=c('unitsp', 'tenurp', 'rcondp'),
                to=c('hhsize', 'tenurh', 'rcondh')))
pum86$ef$weight <- 1.0 / pum86$ef$efsize
pum86$ef <- pum86$ef[pum86$ef$efsize > 1 & pum86$ef$hhclass == 1 &
                     pum86$ef$tenurh != 0,]

pum86$f <- inputDataPUMS('census.pum_cf_1986_canada',
    c('id', 'cftype', 'cfstruc', 'cfsize', 'agem', 'agef', 'lfactm',
    'lfactf', 'occ81m', 'occ81f', 'ind80m', 'ind80f', 'hlosm', 'hlosf',
    'wageh', 'wagew', 'selfh', 'selfw', 'nuchild', 'childa', 'childb',
    'childc', 'childd', 'childe', 'room', 'tenure', 'rcondf', 'morg',
    'ompc', 'grosrtc', 'totalc'), cmacodes_f)
# Remove all families with "N/A" family structure - i.e., one individual
# non-family person.
#
# Note that these non-family persons may not be adults - there are some
# children and even infants included. Those non-family persons are part of
# a household (and probably an economic family), but not part of a census
# family. See Stats Can user guide on families for details.
pum86$f <- pum86$f[pum86$f$cfstruc>0,]

# Fake family PUM - derived from individuals who are observed to belong to
# census families.
pum86$f_weighted_i <- inputDataPUMS('census.pum_i_1986_ontario',
    c('id', 'cfsize', 'htype', 'cfsize', 'unitsp', 'room', 'tenurp',
      'rcondp', 'hhclass'), cmacodes_i,
      rename=list(from=c('unitsp', 'tenurp', 'rcondp'),
                  to=c('hhsize', 'tenure', 'rcondf')))
pum86$f_weighted_i$weight <- 1.0/pum86$f_weighted_i$cfsize
pum86$f_weighted_i <- pum86$f_weighted_i[
    pum86$f_weighted_i$cfsize>1 & pum86$f_weighted_i$hhclass==1 &
    pum86$f_weighted_i$tenure!=0,]

pum86$h <- inputDataPUMS('census.pum_h_1986_canada',
    c('id', 'weight', 'hhcomp', 'hhnuef', 'efcomp', 'hhsize', 'hhnonfam',
      'dtypeh', 'builth', 'tenurh', 'morg', 'rcondh', 'room', 'valueh',
      'grosrth', 'omph'), cmacodes_h)
# Some Hamilton dwellings seem to have dtypeh=9
pum86$h <- pum86$h[pum86$h$dtypeh!=9,]


pum86$raw <- pum86

pum86$i$id <- factor(pum86$i$id)
pum86$ef$id <- factor(pum86$ef$id)
pum86$f$id <- factor(pum86$f$id)
pum86$f_weighted_i$id <- factor(pum86$f_weighted_i$id)
pum86$h$id <- factor(pum86$h$id)

# Split individual data into two sets, one for young and one for 15+.
# The actual data available for <15 is tiny, so it's simpler to just
# treat them separately. That said, there might be a small benefit to pooling
# - we could pool them with others when figuring out the family status
# variables. But children in these age groups are different in their
# relation to family anyways...
pum86$i_young <- pum86$i[pum86$i$agep < 15,]
pum86$i <- pum86$i[pum86$i$agep >= 15,]


# Much of our labour-force data is only in ten-year intervals (15-64)
# We have given five-year intervals for those who really need it:
# 1) children, where family status can be more volatile.
# 2) high school/uni. transition, where education/labour status is highly
# variable. Unfortunately, some data sources use slightly different data
# organisation - family tables use 15-17 and 18-24, while other tables use
# 15-19 and 20-24. We're forced to do a 15-17, 18-19 and 20-24
# disaggregation to deal with these disparities.
# 3) TODO: late retirement in the 65-69 group.
# We might not need 1), and we might need a 60-64 disaggregation as well.
# We might need higher than 69 too, to capture nursing home transitions,
# etc.
assert(pum86$i$agep>=15);
pum86$i$agep <- cut(pum86$i$agep,
    breaks=c(-Inf, 17, 19, 24, 34, 44, 54, 64, Inf),
    labels=c('15-17', '18-19', '20-24', '25-34', '35-44', '45-54', '55-64',
             '65+'))
# These cuts are based on the childa/childb levels in the family PUM. We
# could do a 4,5,9,14 split to combine with profile totals... but not yet.
assert(pum86$i_young$agep >= 0 && pum86$i_young$agep<15);
pum86$i_young$agep <- cut(pum86$i_young$agep,
    breaks=c(-Inf, 5, Inf),
    labels=c('0-5', '6-14'))

assert(pum86$f$agem >= 15)
assert(pum86$f$agef >= 15)
# Create a generic "age of parent" variable - age of mother, unless
# single-father family, in which case it's age of father.
#
# Age of mother was used in husband-wife couples because childrens' ages
# are more likely related to mother's age (and typical childbearing years).
#
# Note that we don't separate 15-19 into 15-17 and 18-19 categories here -
# it's the one place where we can get away without it, and the one place
# where it's most necessary, since family synthesis is already way too
# memory-intensive.
pum86$f$agep <- cut(ifelse(pum86$f$agef != 99, pum86$f$agef, pum86$f$agem),
    breaks=c(-Inf, 19, 24, 34, 44, 54, 64, 98, Inf),
    labels=c('15-19', '20-24', '25-34', '35-44',
             '45-54', '55-64', '65+', 'NA'))
assert(pum86$f$agep != 'NA')
pum86$f$agep <- pum86$f$agep[,drop=TRUE]

pum86$f$agem <- cut(pum86$f$agem,
    breaks=c(-Inf, 17, 19, 24, 34, 44, 54, 64, 98, Inf),
    labels=c('15-17', '18-19', '20-24', '25-34', '35-44',
             '45-54', '55-64', '65+', 'NA'))
pum86$f$agef <- cut(pum86$f$agef,
    breaks=c(-Inf, 17, 19, 24, 34, 44, 54, 64, 98, Inf),
    labels=c('15-17', '18-19', '20-24', '25-34', '35-44',
             '45-54', '55-64', '65+', 'NA'))


pum86$i$sexp <- factor(pum86$i$sexp, levels=1:2, labels=c('Female', 'Male'))

pum86$i$totincp <- cut(pum86$i$totincp,
    breaks=c(-Inf, -1, 0, 999, 2999, 4999, 6999, 9999, 14999, 19999, 24999,
             29999, 34999, Inf),
    labels=c('<0', '0', '0.0-0.9k', '1.0-2.9k', '3.0-4.9k', '5.0-6.9k',
             '7.0-9.9k', '10.0-14.9k', '15.0-19.9k', '20.0-24.9k',
             '25.0-29.9k', '30.0-34.9k', '35.0k+'))

# This ignores:
# 1) overflows: there is a minimum/maximum income reported
# 2) negative incomes.
pum86$i$wageselfp <- cut(pum86$i$wagesp + pum86$i$selfip,
    breaks=c(-Inf, -1, 0, 9999, 19999, 29999, 49999, Inf),
    labels=c('<0', '0', '1-9.9k', '10-19.9k', '20-29.9k', '30-49.9k', '50k+'))

pum86$f$wageselfm <- cut(pum86$f$wageh + pum86$f$selfh,
    breaks=c(-Inf, -1, 0, 9999, 19999, 29999, 49999, Inf),
    labels=c('<0', '0', '1-9.9k', '10-19.9k', '20-29.9k', '30-49.9k', '50k+'))

pum86$f$wageselff <- cut(pum86$f$wagew + pum86$f$selfw,
    breaks=c(-Inf, -1, 0, 9999, 19999, 29999, 49999, Inf),
    labels=c('<0', '0', '1-9.9k', '10-19.9k', '20-29.9k', '30-49.9k', '50k+'))

# Marital status is less useful than family status. Ignore it.
#pum86$i$marstp <- collapse(pum86$i$marstp, levelmap=c(1,2,1,1,1), levels=1:2,
#    labels=c('Single', 'Married'))

# TODO is this the right breakdown? We might want to combine 1-3 of the
# children roles, and non-cf living with others.
#
# Justification for existing collapses:
# 1) Gender of individual is redundant w/ existing gender variable
# 2) While gender of parent might be useful for family assignment, it isn't
#    hypothesised to significantly correlate with the child's attributes.
#    Child-of-lone-male is a small category too, with only 370
#    observations.
# 3) Difference between living-with-relatives and non-relatives is too
#    subtle to bother with.
#
# Meaning of N/A: either a collective resident (non-institutional, since
# PUM excludes all institutional residents), a temporary resident, or a
# person whose household is outside Canada (military, etc.)
pum86$i$cfstat <- collapse.factor(pum86$i$cfstat + 1,
    LevelMap(map=c(7,1,1,3,2,2,4,4,5,5,6), levels=1:7,
             labels=c('Partner', 'Lone parent',
                      'Child w/ parents', 'Child w/ lone parent',
                      'Non-CF person w/ others',
                      'Non-CF person living alone',
                      'NA')))
pum86$i_young$cfstat <- collapse.factor(pum86$i_young$cfstat + 1,
    LevelMap(map=c(7,1,1,3,2,2,4,4,5,5,6), levels=1:7,
             labels=levels(pum86$i$cfstat)))
# All hhclass=2 residents (Not in private household) should have an NA
# family status. These are those living in collective households or outside
# Canada.
assert(pum86$i[pum86$i$hhclass == 2,]$cfstat == 'NA')
# Some hhclass=1 residents (In private household) also have NA - they
# appear to be temporary residents.

pum86$i$hhnumcf <- collapse.factor(pum86$i$htype + 1,
    LevelMap(map=c(4,2,2,2,2,2,2,2,2,2,3,1,1), levels=1:4, 
        labels=c('0', '1', '2+', 'NA')))
# Anyone in an economic family but in a "Non-family household" (htype=12) must
# be an "Economic family person but not census family person" (efstat=2).
assert(pum86$ef$efstat[pum86$ef$htype==12] == 2)
assert(pum86$ef$htype %in% c(1:10, 12))
pum86$ef$hhnumcf <- collapse.factor(pum86$ef$htype,
    LevelMap(map=c(2,2,2,2,2,2,2,2,2,3,1,1), levels=1:3, 
        labels=c('0', '1', '2+')))
assert(pum86$f$htype != 0 & pum86$f_weighted_i$htype < 11)
pum86$f_weighted_i$hhnumcf <- collapse.factor(pum86$f_weighted_i$htype,
    LevelMap(map=c(2,2,2,2,2,2,2,2,2,3), levels=1:3, 
        labels=c('0', '1', '2+')))
# I have put N/A (collectives etc.) into non-family household category.
pum86$i$htype <- collapse.factor(pum86$i$htype + 1,
    LevelMap(map=c(4,1,1,1,1,1,1,2,2,2,3,4,4), levels=1:4, 
        labels=c('One family household, primary family',
                 'One family household, secondary family',
                 'Multiple family household',
                 'Non-family household')))

assert(pum86$f$cftype != 0)
pum86$f$cftype <- factor(pum86$f$cftype, levels=1:2,
    labels=c('Primary family', 'Secondary family'))
pum86$f$cfstruc <- collapse.factor(pum86$f$cfstruc,
    LevelMap(map=c(1,1,1,1,1,1,1,1,3,3,3,3,2,2,2,2), levels=1:3,
             labels=c('Husband-wife',
                      'Lone female parent',
                      'Lone male parent')))

assert(pum86$hhcomp>=1 & pum86$hhcomp<=5)
# Note: it's not clear that efcomp is usable for hhcomp=4 or hhcomp=5.
# Neither of those hhcomps have values of 2,3,4,5 for efcomp, for no clear
# reason.
pum86$h$hhefcomp <- collapse.factor(
    ifelse(pum86$h$hhcomp==3, pum86$h$efcomp + 5, pum86$h$hhcomp),
    LevelMap(map=c(1,2, 3, 6, 7,
                   1, 3,3,4,4, 7,7,7, 5),
             levels=1:7,
             labels=c('Non-family, 1 person',
                      'Non-family, 2+ persons',
                      'Husband-wife',
                      'Lone parent',
                      '1-family other, 0 unattached',
                      '1-family and 1+ unattached',
                      'Other')))
pum86$h$hhnuef <- collapse.factor(pum86$h$hhnuef + 1,
    LevelMap(map=c(1,2,2), levels=1:2, labels=c('0', '1+')))

assert(pum86$ef$hhsize != 0)
assert(pum86$ef$room != 0)
pum86$ef$pperroom <- cut(pum86$ef$hhsize / pum86$ef$room,
    breaks=c(-Inf, 0.5, 1.0, 1.5, 2.0, Inf),
    labels=c('0-0.5', '0.6-1.0', '1.1-1.5', '1.6-2.0', '2.1+'))
assert(pum86$h$hhsize != 0)
assert(pum86$h$room != 0)
pum86$h$pperroom <- cut(pum86$h$hhsize / pum86$h$room,
    breaks=c(-Inf, 0.5, 1.0, 1.5, 2.0, Inf),
    labels=c('0-0.5', '0.6-1.0', '1.1-1.5', '1.6-2.0', '2.1+'))

pum86$i$unitsp <- factor(ifelse(pum86$i$unitsp == 0, 9, pum86$i$unitsp),
    levels=1:9, labels=c(1:7, '8+', 'NA'))
pum86$ef$hhsize <- factor(pum86$ef$hhsize,
    levels=1:8, labels=c(1:7, '8+'))
pum86$h$hhsize <- factor(pum86$h$hhsize,
    levels=1:8, labels=c(1:7, '8+'))
pum86$f_weighted_i$hhsize <- factor(pum86$f_weighted_i$hhsize,
    levels=1:8, labels=c(1:7, '8+'))

# We allow the size=1, just to make life easier when comparing against
# hhsize.
assert(pum86$f$cfsize!=1)
pum86$f$cfsize <- factor(pum86$f$cfsize,
    levels=1:8, labels=c(1:7, '8+'))
assert(pum86$f$cfsize!=1)
pum86$f_weighted_i$cfsize <- factor(pum86$f_weighted_i$cfsize,
    levels=1:8, labels=c(1:7, '8+'))


assert(pum86$f$nuchild != 9)
assert(pum86$f$childa != 9)
assert(pum86$f$childb != 8)
assert(pum86$f$childb != 9)
assert(pum86$f$childc != 9)
assert(pum86$f$childd != 9)
assert(pum86$f$childe != 9)
pum86$f$nuchild <- factor(pum86$f$nuchild + 1, levels=1:9, labels=c(0:7, '8+'))
# children in census fam at home <6 years
pum86$f$childa <- factor(pum86$f$childa + 1, levels=1:3, labels=c(0:1, '2+'))
# children in census fam at home 6-14 years
pum86$f$childb <- factor(pum86$f$childb + 1, levels=1:4, labels=c(0:2, '3+'))
# children in census fam at home 15-17 years
pum86$f$childc <- factor(pum86$f$childc + 1, levels=1:3, labels=c(0:1, '2+'))
# children in census fam at home 18-24 years
pum86$f$childd <- factor(pum86$f$childd + 1, levels=1:3, labels=c(0:1, '2+'))
# children in census fam at home 25+ years
pum86$f$childe <- factor(pum86$f$childe + 1, levels=1:3, labels=c(0:1, '2+'))
# Combine D&E levels into a single factor, just to make CF86A03 viable.
pum86$f$childde <- factor(
    (unclass(pum86$f$childd)-1)*3 + unclass(pum86$f$childe),
    levels=1:9,
    labels=c('0,0', '0,1', '0,2+',
             '1,0', '1,1', '1,2+',
             '2+,0','2+,1','2+,2+'))


assert(pum86$i_child$lfact == 0)
assert(pum86$i$lfact != 0)
pum86$i$lfact <- collapse.factor(pum86$i$lfact,
    LevelMap(map=c(1,1,2,2,2,2,2,2,2,2,3,3,4,4),
             levels=1:4,
             labels=c('Employed',
                      'Unemployed',
                      'Not labour force (>=1985)',
                      'Not labour force (<1985)')))
assert(pum86$f$lfactm!=99)
assert(pum86$f$lfactf!=99)
pum86$f$lfactp <- ifelse(pum86$f$cfstruc == 'Lone male parent',
                         pum86$f$lfactm, pum86$f$lfactf)
pum86$f$lfactp <- collapse.factor(pum86$f$lfactp + 1,
    LevelMap(map=c(5,1,1,2,2,2,2,2,2,3,3,4,4),
             levels=1:5,
             labels=c('Employed',
                      'Unemployed',
                      'Not labour force (>=1985)',
                      'Not labour force (<1985)',
                      'NA')))
assert(pum86$f$lfactp != 'NA')
pum86$f$lfactp <- pum86$f$lfactp[,drop=TRUE]
pum86$f$lfactm <- collapse.factor(pum86$f$lfactm + 1,
    LevelMap(map=c(5,1,1,2,2,2,2,2,2,3,3,4,4),
             levels=1:5,
             labels=c('Employed',
                      'Unemployed',
                      'Not labour force (>=1985)',
                      'Not labour force (<1985)',
                      'NA')))
pum86$f$lfactf <- collapse.factor(pum86$f$lfactf + 1,
    LevelMap(map=c(5,1,1,2,2,2,2,2,2,3,3,4,4),
             levels=1:5,
             labels=c('Employed',
                      'Unemployed',
                      'Not labour force (>=1985)',
                      'Not labour force (<1985)',
                      'NA')))

# Meaning of N/A: described below.
# Merge agri & primary together.
assert(pum86$i_child$occ81p == 0)
pum86$i$occ81p <- collapse.factor(pum86$i$occ81p + 1,
    LevelMap(map=c(16, 1:10, 10:15),
             levels=1:16,
             labels=c(
                'Manag admin & related occns',
                'Occs in nat sci engi & math',
                'Occs in soc sci & rel fields',
                'Teaching and related occns',
                'Occns in medicine and health',
                'Art literary recreat & rel occ',
                'Clerical and related occns',
                'Sales occupations',
                'Service occupations',
                'Primary (farm fish forest mine)',
                'Processing occupations',
                'Mach prod fab assmb repair occ',
                'Construction trades occns',
                'Transp equip operating occns',
                'Other (religion mat hand other)',
                'NA')))
pum86$f$occ81p <- ifelse(pum86$f$cfstruc == 'Lone male parent',
                         pum86$f$occ81m, pum86$f$occ81f)
pum86$f$occ81p <- collapse.factor(pum86$f$occ81p + 1,
    LevelMap(map=c(16, 1:10, 10:15),
             levels=1:16,
             labels=levels(pum86$i$occ81p)))
pum86$f$occ81m <- collapse.factor(pum86$f$occ81m + 1,
    LevelMap(map=c(16, 1:10, 10:15),
             levels=1:16,
             labels=levels(pum86$i$occ81p)))
pum86$f$occ81f <- collapse.factor(pum86$f$occ81f + 1,
    LevelMap(map=c(16, 1:10, 10:15),
             levels=1:16,
             labels=levels(pum86$i$occ81p)))

# Merge agri & other primary together
# Merge govt services fed & other together
assert(pum86$i_child$ind80p == 0)
pum86$i$ind80p <- collapse.factor(pum86$i$ind80p + 1,
    LevelMap(map = c(15, 1, 1:10, 10:14),
             levels = 1:15,
             labels = c(
                'Primary (agri & other)',
                'Manufacturing',
                'Construction',
                'Transportation and storage',
                'Communication & oth utilities',
                'Wholesale trade',
                'Retail trade',
                'Finance, insurance & real est',
                'Business services',
                'Govt services',
                'Educational services',
                'Health and social services',
                'Accomodation food & bev serv',
                'Other services',
                'NA')))
pum86$f$ind80p <- ifelse(pum86$f$cfstruc == 'Lone male parent',
                         pum86$f$ind80m, pum86$f$ind80f)
pum86$f$ind80p <- collapse.factor(pum86$f$ind80p + 1,
    LevelMap(map = c(15, 1, 1:10, 10:14),
             levels = 1:15,
             labels=levels(pum86$i$ind80p)))
pum86$f$ind80m <- collapse.factor(pum86$f$ind80m + 1,
    LevelMap(map = c(15, 1, 1:10, 10:14),
             levels = 1:15,
             labels=levels(pum86$i$ind80p)))
pum86$f$ind80f <- collapse.factor(pum86$f$ind80f + 1,
    LevelMap(map = c(15, 1, 1:10, 10:14),
             levels = 1:15,
             labels=levels(pum86$i$ind80p)))

# The N/As in the occupation and industry dataset come from three sources:
# 1) Those outside the labour force (NLF) who haven't worked since 1985
# 2) The inexperienced unemployed
#
# We verify #1 here, and that the remaining types are not N/A. We can't do
# any verification on the unemployed, since they're a mixed bunch,
# containing both N/As (inexperienced) and values (experienced)
assert(pum86$i[pum86$i$lfact %in% c('Not labour force (<1985)', 'NA'),
              c('occ81p','ind80p')] == 'NA')
assert(pum86$i[pum86$i$lfact %in% c('Employed', 'Not labour force (>=1985)'),
              c('occ81p','ind80p')] != 'NA')

# There is one inconsistency here - we have occupation data for those who
# are NLF but have worked since 1985. This data is *only* present in the
# PUMS and not in the summary tabulations.
#
# TODO: change LF86B04 to act only on the relevant dimensions of lfact,
# i.e., map for lfact is c(1, 1, NA, NA, NA) - it acts on employed and
# unemployed, but no one else.
#
# Actually, better yet - do multiple syntheses, one for each subpopulation.
# It'll be much faster. These are basically separable subpopulations:
# 1) Children: age x sex x family status
# 2) Non-labour force: (1) x education x lfact
# 3) Labour force: (2) x occupation x industry
pum86$i[pum86$i$lfact == 'Not labour force (>=1985)',
       c('occ81p','ind80p')] <- 'NA'

# There are issues with having no known occupation/industry for those out
# of the labour force. We will eventually have to synthesise them
# using the data for non-LF but worked-since-1985 people, which is present
# in the PUMS but absent everywhere else. It looks fairly similar to the
# occupations of the unemployed group - i.e., fewer managers/scientists
# than employed, more clerical and service.



# Unfortunately, SC86B01 and LF86B03 collapse these categories quite
# differently, and we can only aggregate a few things.
#
# Could combine 5 & 6, if that's useful.
assert(pum86$i_child$hlosp == 0)
assert(pum86$i$hlosp != 0)
assert(pum86$f$hlosm != 99)
assert(pum86$f$hlosf != 99)
pum86$i$hlosp <- collapse.factor(pum86$i$hlosp,
    LevelMap(map=c(1,1,2,3,4,5,4,6,7,8,9),
             levels=1:9,
             labels=c('Less than grade 9', 'Grades 9-13', 'High school',
                      'Trade certificate/diploma/non-uni',
                      'Non-uni w/o diploma', 'Non-uni w/ diploma',
                      'Uni w/o diploma', 'Uni w/ diploma',
                      'Uni w/ degree')))
pum86$f$hlosp <- ifelse(pum86$f$cfstruc == 'Lone male parent',
                         pum86$f$hlosm, pum86$f$hlosf)
assert(pum86$f$hlosp != 0)
pum86$f$hlosp <- collapse.factor(pum86$f$hlosp,
    LevelMap(map=c(1,2,2,2,3,4,5,4,6,7,8,9,9,9,9,9),
             levels=1:9, labels=levels(pum86$i$hlosp)))
pum86$f$hlosp <- pum86$f$hlosp[,drop=TRUE]
pum86$f$hlosm <- collapse.factor(pum86$f$hlosm + 1,
    LevelMap(map=c(10,1,2,2,2,3,4,5,4,6,7,8,9,9,9,9,9),
             levels=1:10, labels=c(levels(pum86$i$hlosp), 'NA')))
pum86$f$hlosf <- collapse.factor(pum86$f$hlosf + 1,
    LevelMap(map=c(10,1,2,2,2,3,4,5,4,6,7,8,9,9,9,9,9),
             levels=1:10, labels=c(levels(pum86$i$hlosp), 'NA')))


# Note that individuals may also have tenurp=0
pum86$i$tenurp <- factor(pum86$i$tenurp, levels=1:2,
    labels=c('Owned', 'Rented'))
pum86$f$tenure <- factor(pum86$f$tenure, levels=1:2,
    labels=c('Owned', 'Rented'))
assert(pum86$ef$tenurh != 0)
pum86$ef$tenurh <- factor(pum86$ef$tenurh, levels=1:2,
    labels=c('Owned', 'Rented'))
assert(pum86$f_weighted_i$tenure != 0)
pum86$f_weighted_i$tenure <- factor(pum86$f_weighted_i$tenure, levels=1:2,
    labels=c('Owned', 'Rented'))
pum86$h$tenurh <- factor(pum86$h$tenurh, levels=1:2,
    labels=c('Owned', 'Rented'))
pum86$i$rcondp <- factor(pum86$i$rcondp + 1, levels=1:3,
    labels=c('NA', 'Yes', 'No'))
pum86$f$rcondf <- factor(pum86$f$rcondf + 1, levels=1:3,
    labels=c('NA', 'Yes', 'No'))
pum86$ef$rcondh <- factor(pum86$ef$rcondh + 1, levels=1:3,
    labels=c('NA', 'Yes', 'No'))
pum86$h$rcondh <- factor(pum86$h$rcondh + 1, levels=1:3,
    labels=c('NA', 'Yes', 'No'))
pum86$i$room <- collapse.factor(pum86$i$room + 1,
    LevelMap(map=c(1,1,1:9), levels=1:9,
             labels=c('1-2', '3', '4', '5', '6', '7', '8', '9', '10+')))
pum86$f$room <- collapse.factor(pum86$f$room,
    LevelMap(map=c(1,1:9), levels=1:9,
             labels=c('1-2', '3', '4', '5', '6', '7', '8', '9', '10+')))
assert(pum86$ef$room != 0)
pum86$ef$room <- collapse.factor(pum86$ef$room,
    LevelMap(map=c(1,1:9), levels=1:9,
             labels=c('1-2', '3', '4', '5', '6', '7', '8', '9', '10+')))
assert(pum86$h$room != 0)
pum86$h$room <- collapse.factor(pum86$h$room,
    LevelMap(map=c(1,1:9), levels=1:9,
             labels=c('1-2', '3', '4', '5', '6', '7', '8', '9', '10+')))
assert(pum86$h$dtypeh != 9)
pum86$h$dtypeh <- collapse.factor(pum86$h$dtypeh,
    LevelMap(map = c(1:4, 3, 5, 5, 6), levels=1:6,
             labels=c('Single-detached',
                      'Apartment, 5+ storeys',
                      'Apartment, 1-4 storeys',
                      'Semi-detached house',
                      'Attached house (row and other)',
                      'Movable')))
pum86$h$builth <- factor(pum86$h$builth, levels=1:7,
    labels=c('-1920',     '1921-1945', '1946-1960', '1961-1970', '1971-1975',
             '1976-1980', '1981-1986'))
pum86$f$morg <- factor(pum86$f$morg + 1, levels=1:3,
    labels=c('NA', 'Yes', 'No'))
pum86$h$morg <- factor(pum86$h$morg + 1, levels=1:3,
    labels=c('NA', 'Yes', 'No'))
# TODO: valueh, renth, omph

# Household payments (owner payments or rent)
#   omph is clipped to [99,1100]
#   grosrth is clipped to [99,1000]
# This is (just barely) enough to get the categories used by the BST
# tables.
pum86$h$payh <- cut(
    ifelse(pum86$h$tenurh == 'Owned', pum86$h$omph, pum86$h$grosrth),
    breaks=c(-Inf, 200, 399, 699, 999, Inf),
    labels=c('0-199', '200-399', '400-699', '700-999', '1000+'))
pum86$f$payf <- cut(
    ifelse(pum86$f$tenure == 'Owned', pum86$f$ompc, pum86$f$grosrtc),
    breaks=c(-Inf, 200, 399, 699, 999, Inf),
    labels=c('0-199', '200-399', '400-699', '700-999', '1000+'))

pum86
}
