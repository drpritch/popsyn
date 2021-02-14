#
# IPF fitting procedure: first part of population synthesis.
# Inputs: none (reads from postgres database using bst86.R and pum86.R)
# Outputs: fit2A, prefitGeog and fitGeog files.
#

library('RODBC')
library('gtools')
library('gdata')

synthesis <- list()
synthesis$startTime <- proc.time()

source('ipf.R')
source('ipf_list.R')
source('utils.R')

# HSK = Hamilton/St. Catharines/Niagara, which are grouped together.
cmaname <- 'Oshawa'
cmacodes <- list(Toronto = 535,
                 HSK = c(537, 539, 541),
                 Oshawa = 532)[[cmaname]]

print('Loading public use microdata')
source('pum86.R')

pum86 <- switch(cmaname,
    Toronto = makePUMS(cmacodes_i = cmacodes, cmacodes_f = cmacodes,
        cmacodes_h = cmacodes),
    # Use combined 599 code for HSK together for i, and likewise pool f/h.
    # TODO: eventually, separate H/S/K for f/h.
    HSK = makePUMS(cmacodes_i = 599, cmacodes_f = cmacodes,
        cmacodes_h = cmacodes),
    # We have no i or f data for Oshawa. Use HSK and Hamilton instead.
    Oshawa = makePUMS(cmacodes_i = 535, cmacodes_f = 535,
        cmacodes_h = cmacodes)
)

# Hamilton:
# cmacode = 537 for family, household
# cmacode = 599 for individual (includes kitchener & st catherines too)
#
# Oshawa:
# cmacode = 532 for household
# cmacode = 535 or 537 or 0 for family?
# cmacode = 535 or 599 or 0 for individual?

###########################################################################

print('Loading profile tables')
# Uses cmacodes as input.
source('profile86.R')


###########################################################################

print('Loading basic summary tabulations')
source('bst86.R')
bst <- makeBSTs(profile86i = profile86i, pum86 = pum86, cmacodes = cmacodes)

###########################################################################

print('Fitting population')
print('Applying BST maps')

# Remap BSTs
mapBST <- function(bst, map, mapInv = NULL) {
    bst$data <- prepare.IpfConstraint(bst$data, map, mapInv)
    if(!is.null(bst$ipf)) {
        bst$ipf <- prepare.IpfConstraint(bst$ipf, map, mapInv)
    }
    if(!is.null(bst$ipf_a)) {
        bst$ipf_a <- prepare.IpfConstraint(bst$ipf_a, map, mapInv)
    }
    if(!is.null(bst$ipf_a_noctcode)) {
        bst$ipf_a_noctcode <-
            prepare.IpfConstraint(bst$ipf_a_noctcode, map, mapInv)
    }
    for(i in seq(bst$margin)) {
        bst$margin[[i]] <- prepare.IpfConstraint(
            bst$margin[[i]], map, mapInv)
    }
    bst
}

pt$sexinc_2b <- mapBST(pt$sexinc_2b,
    list(LevelMap(factor = 'totincp',
                  map = c(2,1,2,3:12),
                  levels = 1:12,
                  labels = dimnames(pt$sexinc_2b$data$.data)$totincp))
)
# The BST_SC86B01 table uses a collapsed version of the age dimension,
# with combined 15-17, 18-19 and 20-24 categories
bst$sc86b01 <- mapBST(bst$sc86b01,
    list(LevelMap(factor = 'hlosp',
                  map = c(1:4,4,4,5,5,6),
                  levels = 1:6,
                  labels = dimnames(bst$sc86b01$data$.data)$hlosp),
         LevelMap(factor = 'agep',
                  map = c(1, 1, 1:6),
                  levels = 1:6,
                  labels = dimnames(bst$sc86b01$data$.data)$agep))
)
bst$lf86b01 <- mapBST(bst$lf86b01,
    list(LevelMap(factor = 'lfact',
                  map = c(1,2,3,3), levels=1:3,
                  labels = dimnames(bst$lf86b01$data$.data)$lfact),
         LevelMap(factor = 'agep',
                  map = c(1, 1, 1:6),
                  levels = 1:6,
                  labels = dimnames(bst$lf86b01$data$.data)$agep))
)
bst$lf86b03 <- mapBST(bst$lf86b03,
    list(LevelMap(factor = 'lfact',
                  map = c(1,2,3,3), levels=1:3,
                  labels = dimnames(bst$lf86b03$data$.data)$lfact),
         LevelMap(factor = 'hlosp',
                  map = c(1:6,5:7), levels=1:7,
                  labels = dimnames(bst$lf86b03$data$.data)$hlosp))
)
bst$cf86a03 <- mapBST(bst$cf86a03,
    list(LevelMap(factor = 'childa',
                  map = c(1,2,2), levels = 1:2,
                  labels = dimnames(bst$cf86a03$data$.data)$childa),
         LevelMap(factor = 'childb',
                  map = c(1,2,2,2), levels = 1:2,
                  labels = dimnames(bst$cf86a03$data$.data)$childb),
         LevelMap(factor = 'childc',
                  map = c(1,2,2), levels = 1:2,
                  labels = dimnames(bst$cf86a03$data$.data)$childc),
         LevelMap(factor = 'childde',
                  map = c(1,2,2,2,2,2,2,2,2), levels = 1:2,
                  labels = dimnames(bst$cf86a03$data$.data)$childde))
)
bst$cf86a04 <- mapBST(bst$cf86a04,
    list(LevelMap(factor = 'agep',
                  map = c(1,1,1,2,2,3,3,4),
                  levels = 1:4,
                  labels = c('15-24','25-44','45-64','65+')),
         LevelMap(factor = 'cfstat',
                  map = c(1,2,3,3,4,4,5),
                  levels = 1:5,
                  labels = dimnames(bst$cf86a04$data$.data)$cfstat))
)

maps$dtypeh <-
    LevelMap(factor = 'dtypeh',
             map = c(1,2,4,4,4,3), levels=1:4,
             labels = dimnames(bst$dw86a01$data$.data)$dtypeh)
bst$dw86a01 <- mapBST(bst$dw86a01,
    list(maps$dtypeh),
    list(LevelMap(factor = 'tenurh',
                  map = c(1,2,2), levels=1:2,
                  labels = levels(pum86$h$tenurh)))
)
bst$dw86a02 <- mapBST(bst$dw86a02,
    list(maps$dtypeh),
    list(LevelMap(factor = 'hhsize',
                  map = c(1:8,8,8), levels=1:8,
                  labels = levels(pum86$h$hhsize)))
)
bst$dw86b02 <- mapBST(bst$dw86b02,
    list(maps$dtypeh),
    list(LevelMap(factor = 'builth',
             map = c(1:7,7), levels=1:7,
             labels = levels(pum86$h$builth)))
)

bst$dw86b04 <- mapBST(bst$dw86b04,
    list(maps$dtypeh)
)

bst$hh86a02 <- mapBST(bst$hh86a02,
    list(),
    list(LevelMap(factor = 'hhsize',
                  map = c(1:8,8,8), levels=1:8,
                  labels = levels(pum86$h$hhsize)))
)

###########################################################################

print('Fitting PUM to 2A form (2%->100% sample)')

# Fit the PUM (2% sample) to the 2A form totals (100% sample)
fit2A <- list(cmacodes = cmacodes, cmaname = cmaname)
#
# TODO: use 'none' as assoc, and fit to the 3-way margins of the pop.
# Very low priority, though...
#

# # #
# DWELLING
# # #

#
# Dwelling synthesis is a little elaborate.
# The first IPF would normally be enough. However, the household PUM does
# not contain information about whether a given dwelling has any census
# families in it, and we need this to do the family synthesis. The
# household pum does have an hhnuef variable to tell us whether there are
# any economic families, however. Further, the individual PUM can tell us
# what types of economic families contain no census families.
#
# So, we do an elaborate dance to convert hhnuef to hhnumcf, a variable that
# tells us whether the dwelling contains 0/1/2 census families.
# 
# Special case here: the 
fit2A$d <- ipf_list(
    constraintList = list(
        # For the B (20%) tables, use the variant that's been fitted to
        # A (100%) margins.
        bst$dw86b02$ipf_a_noctcode,
        bst$dw86b04$ipf_a_noctcode,
        bst$hh86b01_b02$ipf_a_noctcode,
        # The A (100%) tables can just be used as is.
        bst$dw86a01$margin$tenurh_dtypeh,
        bst$dw86a02$margin$hhsize_dtypeh,
        bst$dw86a01$margin$tenurh,
        bst$dw86a02$margin$hhsize,
        bst$dw86a01$margin$dtypeh,
        bst$dw86a01$total
    ),
    popFrame = pum86$h[,c('id', 'weight',
                          'hhnuef', 'hhsize', 'tenurh', 'pperroom', 'room',
                          'dtypeh', 'builth', 'payh')],
    priorAssoc = 'fromweight',
    maxIterations = 100,
    tolerance = 1/100,
    statistics = TRUE)

# For dwellings that contain economic families, we want to know how
# many census families are present. We have a vague glimmering about this
# from the individual PUM, which has been culled and reweighted to give a
# sort of "economic family PUM". We want to use this to form a constraint
# on the dwelling IPF, but we first need to fit that PUM to the 2A totals.
#
# TODO: use a proper loglinear modelling process to determine the best
# model for the hhnumcf variable!
temp <- list()
# First fit the ef data to the 2A margins.
temp$ef <- ipf(
    constraintList = list(
        IpfConstraint(
            margin.list(fit2A$d$fitted.values,
                        c('hhnuef', 'hhsize', 'tenurh'))['1+',,]
        )
    ),
    priorArray = xtabs(weight ~ hhnumcf + hhsize + tenurh, pum86$ef),
    maxIterations = 100,
    tolerance = 1/100)
# Next, add a 0-ef dimension to make it match the dwelling universe.
temp$d <- abind(
    array(0, dim(temp$ef), dimnames(temp$ef)),
    temp$ef,
    along = 0.5
)
names(dimnames(temp$d)) <- c('hhnuef', names(dimnames(temp$ef)))
dimnames(temp$d)$hhnuef <- c('0', '1+')
temp$d['0','0',,] <-
    margin.list(fit2A$d$fitted.values, c('hhnuef', 'hhsize', 'tenurh'))['0',,]
# Then, fit to a dwelling-universe table (HH86A02) that also refers to the
# number of census families.
temp$d <- ipf(
    constraintList = list(
        # Constrain to the fit we've already established.
        IpfConstraint(
            margin.list(fit2A$d$fitted.values, c('hhnuef', 'hhsize', 'tenurh'))
        ),
        # Also include margins from HH86A02, which does give us
        # information about the number of census families per household.
        # This takes priority over hhnuef, which is not observed directly
        # in a 20% or 100% table.
        bst$hh86a01$margin$tenurh_hhnumcf,
        bst$hh86a02$margin$hhsize_hhnumcf,
        bst$hh86a02$margin$hhnumcf),
    priorArray = temp$d,
    maxIterations = 100,
    tolerance = 1/100)

#
# One more layer of complexity: need to match sparsity pattern of families,
# since we'll later conditionally synthesize them.
# ... buuuut, we need the hhsize/cfsize linkage for this to ever work.
# That's not going to happen.
#
#link <- xtabs(~tenure + room + payf + cfsize, pum86$f) > 0
#names(dimnames(link)) <- c('tenurh', 'room', 'payh', 'cfsize')

# Repeat the dwelling IPF, but this time use the EF-derived table as a
# constraint, giving a new (array) variable "hhnumcf".
fit2A$d <- ipf_list(
    constraintList = list(
        # For the B (20%) tables, use the variants that have been fitted to
        # A (100%) margins.
        bst$dw86b02$ipf_a_noctcode,
        bst$dw86b04$ipf_a_noctcode,
        bst$hh86b01_b02$ipf_a_noctcode,
        # The A (100%) tables can just be used as is.
        bst$dw86a01$margin$tenurh_dtypeh,
        bst$dw86a02$margin$hhsize_dtypeh,
        # These two are replaced by the EF fit.
        #bst$dw86a01$margin$tenurh,
        #bst$dw86a02$margin$hhsize,
        IpfConstraint(temp$d),
        bst$dw86a01$margin$dtypeh,
        bst$dw86a01$total
    ),
    popFrame = pum86$h[,c('id', 'weight',
                          'hhnuef', 'hhsize', 'tenurh', 'pperroom', 'room',
                          'dtypeh', 'builth', 'payh')],
    priorAssoc = 'fromweight',
    maxIterations = 100,
    tolerance = 1/100,
    statistics = TRUE)

# The hhnumcf variable comes back as an array dimension, since it isn't
# present in the dwelling PUM. Convert it to a list dimension, for easier
# processing later on.
fit2A$d$fitted.values <- ipf_list_arrayDimToListDim(fit2A$d$fitted.values)
colnames(fit2A$d$fitted.values)[colnames(fit2A$d$fitted.values) == 'id'] <- 'pumhId'
fit2A$d$fitted.values$id <- factor(seq(nrow(fit2A$d$fitted.values)))
# Hack: remove a bunch of zeros, to speed things up.
#fit2A$d <- fit2A$d[fit2A$d$hhnuef != '0' | fit2A$d$hhnumcf == '0',]
fit2A$d$fitted.values <-
    fit2A$d$fitted.values[fit2A$d$fitted.values$weight > 0,]

fit2A$d$fitted.values$id <- fit2A$d$fitted.values$id[, drop=TRUE]

# Assuming that hhnumcf='2+' means exactly two census families, we get a
# slight misprediction of the total number of families: the dwelling
# synthesis here predicts 904949 families, while the actual total is
# 906389. The different is presumably due to 3-family households.

remove(temp)



# # #
# INDIVIDUAL
# # #

# Fit 2A with just 1D constraints. Only here for experiments; not for
# regular use.
temp <- bst$cf86a04$ipf_a
temp$.data <- margin.table2(temp$.data, 'cfstat')
temp$.min <- NULL
temp$.max <- NULL
fit2A$i1 <- ipf_list(constraintList = list(
        # CF86A04 is a weird exception, missing all other margins due to the
        # effort required to get it to fit the universe and sample of the
        # other tables.
        temp,
        # Two-way constraints
        pt$sexinc_2b$margin$sexp_totincp,
        # One-way constraints
        bst$lf86b01$margin$lfact,
        bst$sc86b01$margin$hlosp,
        profile86i$adultnoninst_sexp,
        bst$sc86b01$margin$agep,
        bst$lf86b04$margin$occ81p,
        # Zero-way constraint
        profile86i$adultnoninst),
    popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                          'lfact', 'totincp')],
    priorAssoc = 'frompop',
    maxIterations = 100,
    tolerance = 1/100,
    statistics = TRUE)

# Fit without hierarchical margins
temp <- bst$cf86a04$ipf_a
temp$.data <- margin.table2(temp$.data, c('sexp', 'agep', 'cfstat'))
temp$.min <- NULL
temp$.max <- NULL
fit2A$i_nh <- ipf_list(constraintList = list(
        # CF86A04 is a weird exception, missing all other margins due to the
        # effort required to get it to fit the universe and sample of the
        # other tables.
        temp,

        bst$lf86b01$margin$lfact_sexp_agep,
        bst$lf86b03$margin$lfact_sexp_hlosp,
        bst$sc86b01$margin$hlosp_sexp_agep,
        # Two-way constraints
        pt$sexinc_2b$margin$sexp_totincp,
        bst$fake$margin$sexp_agep,
        bst$lf86b04$margin$sexp_occ81p,
        # One-way constraints
        profile86i$adultnoninst_sexp),
    popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                          'lfact', 'totincp')],
    priorAssoc = 'frompop',
    maxIterations = 100,
    tolerance = 1/100,
    statistics = TRUE)


temp <- bst$cf86a04$ipf_a
temp$.data <- margin.table2(temp$.data, c('sexp', 'agep', 'cfstat'))
temp$.min <- NULL
temp$.max <- NULL
fit2A$i <- ipf_list(constraintList = list(
        # CF86A04 is a weird exception, missing all other margins due to the
        # effort required to get it to fit the universe and sample of the
        # other tables.
        temp,

        bst$lf86b01$margin$lfact_sexp_agep,
        bst$lf86b03$margin$lfact_sexp_hlosp,
        bst$sc86b01$margin$hlosp_sexp_agep,
        # Two-way constraints
        pt$sexinc_2b$margin$sexp_totincp,
        bst$lf86b01$margin$lfact_agep,
        bst$lf86b01$margin$lfact_sexp,
        bst$lf86b03$margin$lfact_hlosp,
        bst$sc86b01$margin$hlosp_sexp,
        bst$sc86b01$margin$hlosp_agep,
        bst$fake$margin$sexp_agep,
        bst$lf86b04$margin$sexp_occ81p,
        # One-way constraints
        bst$lf86b01$margin$lfact,
        bst$sc86b01$margin$hlosp,
        profile86i$adultnoninst_sexp,
        bst$sc86b01$margin$agep,
        bst$lf86b04$margin$occ81p,
        # Zero-way constraint
        profile86i$adultnoninst),
    popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                          'lfact', 'totincp')],
    priorAssoc = 'frompop',
    maxIterations = 100,
    tolerance = 1/100,
    statistics = TRUE)
# For consistency with the other fits... add a "pum id" column.
fit2A$i$fitted.values$pumiId <- fit2A$i$fitted.values$id
fit2A$i$fitted.values$id <- fit2A$i$fitted.values$id[, drop=TRUE]



# # #
# FAMILY
# # #

# Build constraints against the individual table: get the correct
# education+age for husband/wife. This takes a little work, because we have
# to add an "N/A" category for both variables.

temp <- list()
temp$i <- margin.list(fit2A$i$fitted.values, c('sexp', 'lfact', 'agep', 'cfstat'))
# Convert individual cfstat to family's cfstruc
temp$i <- temp$i[,,,c('Partner', 'Lone parent', 'Lone parent')]
names(dimnames(temp$i)) <- c('sexp', 'lfact', 'agep', 'cfstruc')
dimnames(temp$i)$cfstruc <-
    c('Husband-wife', 'Lone female parent', 'Lone male parent')
temp$i['Male',,,'Lone female parent'] <- 0
temp$i['Female',,,'Lone male parent'] <- 0
assert(dimnames(temp$i)$cfstruc == levels(pum86$f$cfstruc))
# Add extra "NA" category for lfact and agep.
temp$dn <- dimnames(temp$i)
temp$dn$lfact <- c(temp$dn$lfact, 'NA')
temp$dn$agep <- c(temp$dn$agep, 'NA')
temp$i <- abind(temp$i, array(0, dim=c(dim(temp$i)[1], 1, dim(temp$i)[3:4])),
                along=2)
temp$i <- abind(temp$i, array(0, dim=c(dim(temp$i)[1:2], 1, dim(temp$i)[4])),
                along=3)
# abind loses the names and we have to bring them back.
dimnames(temp$i) <- temp$dn

temp$m <- temp$i['Male',,,]
names(dimnames(temp$m)) <- c('lfactm', 'agem', 'cfstruc')
temp$m['NA','NA','Lone female parent'] <-
    margin.table2(temp$i['Female',,,'Lone female parent'])
temp$f <- temp$i['Female',,,]
names(dimnames(temp$f)) <- c('lfactf', 'agef', 'cfstruc')
temp$f['NA','NA','Lone male parent'] <-
    margin.table2(temp$i['Male',,,'Lone male parent'])
temp$i <- NULL

temp$d <- margin.list(fit2A$d$fitted.values, c('hhnumcf', 'room', 'tenurh'))
# Note that this excludes the "0" level - it takes too much memory to
# include it. We have to be careful later, as a result.
temp$dcf <- abind(
    temp$d['1',,],
    temp$d['2+',,]*2,   # Treat 2+ as exactly 2.
    along = 0.5)
names(dimnames(temp$dcf)) <- c('hhnumcf', 'room', 'tenure')
dimnames(temp$dcf)$hhnumcf <- c('1', '2+')

fit2A$f <- ipf_list(constraintList = list(
        IpfConstraint(temp$f),
        IpfConstraint(temp$m),
        IpfConstraint(temp$dcf),
        bst$cf86a03$margin$cfstruc_childa_b_c_de,
        bst$cf86a02$margin$cfstruc_nuchild,
        bst$cf86a03$margin$cfstruc,
        bst$cf86a02$margin$nuchild,
        bst$cf86a03$margin$childa_b_c_de,
        bst$cf86a03$total),
    popFrame = pum86$f[,c('id', 'cfstruc', 'cfsize',
                     'nuchild', 'room', 'tenure',
                     'agef', 'lfactf', 'agem', 'lfactm',
                     'childa', 'childb', 'childc', 'childde')],
    priorAssoc = 'frompop',
    maxIterations = 500,
    tolerance = 1/100,
    statistics = TRUE)
remove(temp)

fit2A$f$fitted.values <- ipf_list_arrayDimToListDim(fit2A$f$fitted.values)
colnames(fit2A$f$fitted.values)[colnames(fit2A$f$fitted.values) == 'id'] <- 'pumfId'
fit2A$f$fitted.values$id <- factor(seq(nrow(fit2A$f$fitted.values)))
fit2A$f$fitted.values$id <- fit2A$f$fitted.values$id[, drop=TRUE]


system('mkdir ../results')
system('mkdir ../results/latest')
save.image(file=paste('../results/latest/prefitGeog-', cmaname, '.Rdata', sep=''))



########################################################################
print('Adding geography')

fitGeog <- list(cmacodes = cmacodes, cmaname = cmaname)

# TODO: support the "ctcode only" margin. (Requires improvements to
# ipf_list)
getBSTCtcodeMargins <- function(bstList) {
    c(lapply(bstList, function(x) { x$data }),
      unlist(lapply(bstList, function(bst) {
        bst$margin[grep('ctcode_', names(bst$margin))]
      }), recursive = FALSE))
}

bstList <- list(bst$sc86b01, bst$lf86b01, bst$lf86b03, bst$lf86b04)

# Now, just fit to the pre-fitted BSTs and the CMA total.
# Also add the sex x age table, so we get the correct 15-19 and 20-24 age
# split.
#
# Note the final constraint: this forces the total weight on each row
# (i.e., each PUM record after summing over zones) to exactly equal the
# total weight in fit2A.
# The obvious alternative (fitting to the tabular representation) isn't
# feasible when the tabular representation gets sufficiently big.
#
# The ideal alternative is to fit against the complete set of (say) 3-way
# crosstabs of fit2A... but it's very slow, and it's not clear that it
# would help much.


fitGeog$i <- ipf_list(
    constraintList = c(
        list(pt$sexinc_2b$data,
             bst$cf86a04$ipf_a),
        # For each of the BSTs, use both the full table and all margins
        # involving ctcode.
        getBSTCtcodeMargins(bstList),
        list(bst$fake$data,
             IpfConstraint(margin.list(fit2A$i$fitted.values, 'id')))
     ),
     popFrame = fit2A$i$fitted.values,
     priorAssoc = 'none',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
# So, we now have a fitted table of weights on the individuals... but we
# must be careful using this - it's massive, and R quickly blows up
# when using the table.


# TODO: we seem to be losing some records here - for example, record #3207
# (movable dwelling, 1 person, 5 rooms) seems to get clamped down from 134
# persons (fit2A) to 0 persons. This seems to be because the appropriate
# zones are area-suppressed to zero.
fitGeog$d <- ipf_list(
    constraintList = c(
        list(bst$dw86b02$ipf_a,
             bst$dw86b04$ipf_a,
             bst$hh86b01_b02$ipf_a),
        getBSTCtcodeMargins(list(bst$dw86a01, bst$dw86a02,
                                 bst$hh86a01, bst$hh86a02)),
        list(IpfConstraint(margin.list(fit2A$d$fitted.values, 'id')))
    ),
    popFrame = fit2A$d$fitted.values,
    priorAssoc = 'none',
    maxIterations = 100,
    tolerance = 1/100,
    statistics = TRUE)

temp <- list()
temp$i <- margin.list(fitGeog$i$fitted.values, c('sexp', 'lfact', 'agep', 'cfstat', 'ctcode'))
gc()
temp$i <- temp$i[,,,c('Partner', 'Lone parent', 'Lone parent'),]
names(dimnames(temp$i)) <- c('sexp', 'lfact', 'agep', 'cfstruc', 'ctcode')
# Convert individual cfstat to family's cfstruc
dimnames(temp$i)$cfstruc <-
    c('Husband-wife', 'Lone female parent', 'Lone male parent')
temp$i['Male',,,'Lone female parent',] <- 0
temp$i['Female',,,'Lone male parent',] <- 0
assert(dimnames(temp$i)$cfstruc == levels(pum86$f$cfstruc))
# Add extra "NA" category for lfact and agep.
temp$dn <- dimnames(temp$i)
temp$dn$lfact <- c(temp$dn$lfact, 'NA')
temp$dn$agep <- c(temp$dn$agep, 'NA')
temp$i <- abind(temp$i, array(0, dim=c(dim(temp$i)[1], 1, dim(temp$i)[3:5])),
                along=2)
temp$i <- abind(temp$i, array(0, dim=c(dim(temp$i)[1:2], 1, dim(temp$i)[4:5])),
                along=3)
# abind loses the names and we have to bring them back.
dimnames(temp$i) <- temp$dn

temp$m <- temp$i['Male',,,,]
names(dimnames(temp$m)) <- c('lfactm', 'agem', 'cfstruc', 'ctcode')
temp$m['NA','NA','Lone female parent',] <-
    margin.table2(temp$i['Female',,,'Lone female parent',], 'ctcode')
temp$f <- temp$i['Female',,,,]
names(dimnames(temp$f)) <- c('lfactf', 'agef', 'cfstruc', 'ctcode')
temp$f['NA','NA','Lone male parent',] <-
    margin.table2(temp$i['Male',,,'Lone male parent',], 'ctcode')
temp$i <- NULL
gc()

temp$d <- margin.list(fitGeog$d$fitted.values,
    c('hhnumcf', 'room', 'tenurh', 'ctcode'))
gc()
temp$dcf <- abind(
    temp$d['1',,,],
    temp$d['2+',,,]*2,   # Treat 2+ as exactly 2.
    along = 0.5)
names(dimnames(temp$dcf)) <- c('hhnumcf', 'room', 'tenure', 'ctcode')
dimnames(temp$dcf)$hhnumcf <- c('1', '2+')

fitGeog$f <- ipf_list(
    constraintList = c(
        list(IpfConstraint(temp$f),
             IpfConstraint(temp$m),
             IpfConstraint(temp$dcf)),
        getBSTCtcodeMargins(list(bst$cf86a02, bst$cf86a03)),
        list(IpfConstraint(margin.list(fit2A$f$fitted.values, 'id')))
    ),
    popFrame = fit2A$f$fitted.values,
    priorAssoc = 'none',
    maxIterations = 100,
    tolerance = 1/100,
    statistics = TRUE)

synthesis$elapsedTime <- proc.time() - synthesis$startTime
fitGeog$elapsedTime <- synthesis$elapsedTime
save(fit2A, file=paste('../results/latest/fit2A-', cmaname, '.Rdata', sep=''))
save(fitGeog, file=paste('../results/latest/fitGeog-', cmaname, '.Rdata', sep=''))
stop()
