#
# profile86.R
#
# Helpful for inspecting the raw 1986 census profile dataset, with appropriate
# labels. Used for population synthesis.
#
# This is probably the most chaotic part of the input data: the
# data was originally downloaded in household and individual blocks, but
# should have been divided differently: 100% and 20% sample data would be
# much more meaningful.
#
# It also hasn't been organized very well here.
#

source('utils.R')

raw <- inputData('profile86i_ct', '*', sortColumns=c('ctcode', 'cma'),
                 whereExpression = makeOrQuery(field='cma', values=cmacodes))
raw <- raw[raw$cma %in% cmacodes,]
#raw <- sqlQuery(channel, paste(
#    'SELECT * FROM profile86i_ct WHERE ',
#    makeOrQuery(field='cma', values=cmacodes),
#    ' ORDER BY ctcode,cma', sep=''))

assert(sum(raw$ctcode==0) == length(cmacodes))

profile86i <- list()

# Sum all ctcode=0 rows into a single first row.
if(length(cmacodes) > 0) {
    raw <- rbind(cbind(raw[1,1:3],
                       as.data.frame(lapply(raw[raw$ctcode==0,-(1:3)], sum))),
                 raw[raw$ctcode!=0,])
    raw$cma[1] <- NA;
    raw$cmaname[1] <- NA;
}

# It's a lie - actually numCT+1
numCT <- dim(raw)[1]

# We have a number of zones where area suppression took place due to a tiny
# population.
# We have an accurate total pop for all of these, and an age profile for
# half of them. Everything else is zero.
#
# For the moment, we just give them the CMA-wide average for all
# attributes, scaled to match the known population size.
# TODO: use available age profile for the few that have it.
profile86i$suppressed <- raw$lfstf[-1] == 0
names(profile86i$suppressed) <- raw$ctcode[-1]

# More accurate than total_pop, for some reason.
profile86i$ctcode <- errct(array(data = raw$total_pop1[-1],
    dim = numCT-1,
    dimnames = list(ctcode = raw$ctcode[-1])))
profile86i$total <- err4(raw$total_pop1[1])

raw <- adjustSuppressed(raw, profile86i$suppressed,
                    scale = profile86i$ctcode$.data / profile86i$total$.data,
                    ignoreFields = c('cmaname', 'cma', 'ctname', 'ctcode'))


# 
# VERIFY UNIVERSES
#
# These are just a series of checks to ensure that the universes for
# various profile variables match our expectations.
#

assert(censusEqual(raw$total_pop, raw$agem + raw$agef, 5, 10))

raw$noninst <- raw$total_pop2 - (raw$agem01 + raw$agem02 + raw$agem03 +
    raw$agef01 + raw$agef02 + raw$agef03)
assert(censusEqual(raw$edu, raw$lfstm + raw$lfstf, 5, 10))
# This shouldn't be larger than 10, but there seems to be one +15 value.
# Maybe due to a 9->0 rounding?
assert(censusEqual(raw$lf - raw$lf1, raw$occm + raw$occf, 15, 15))

raw$noninst <- NULL



# In general, the following two constraints should hold (subject to rounding)
#
# 1. LF <= NONINST
# 2. NONINST <= NONINST+CHILD
# 3. NONINST+CHILD <= TOTALPOP
#
# where LF is the size of the labour force (e.g., raw$lf),
# NONINST is the adult non-institutional population (e.g., raw$edu,
# raw$lfstm+raw$lfstf)
# CHILD is the child population (e.g., raw$agem01+agem02+agem03+...)
# TOTALPOP is the total population (raw$total_pop).
# Note that NONINST+CHILD is raw$total_pop2.
#
# Constraint #1 and #2 appear to hold.
# Constraint #3 does not appear to hold, with violations as large as 225.
# Even the weaker "NONINST <= TOTALPOP" does not appear to quite hold, with
# violations of up to 25.
#
# Why is this? Probably because of mismatches between 100% (census form 2A)
# and 20% (form 2B) sampled data. LF, NONINST and NONINST+CHILD
# (total_pop2) come from 20% data, while CHILD and TOTALPOP come from 100%
# data.
#
# The solution? Build my own table showing the geographical breakdown of
# the population into three categories:
#
# 1) child
# 2) adult non-institutional
# 3) adult institutional
#
# Item #1 is known from the 100% data (with some error); the others must be
# inferred from combined 100% and 20% data. My version of this table
# enforces all three constraints - in cases where a constraint is violated,
# any 20% data involved is assumed to be erroneous.
#
# This table is therefore the best estimate we have using 100% data. Note,
# however, that much of our synthesis is done using the 20% estimate of the
# non-institutional population, and then *later* adjusted to match this
# 100% estimate of the population.
#
z <- list()
z$childf <- raw$agef01 + raw$agef02 + raw$agef03
z$childm <- raw$agem01 + raw$agem02 + raw$agem03
z$adultf <- raw$agef - z$childf
z$adultm <- raw$agem - z$childm
profile86i$ctcode_adultinstp_sexp <- array(
    data = c(
        z$childf, pmin(raw$lfstf, z$adultf), pmax(0, z$adultf - raw$lfstf),
        z$childm, pmin(raw$lfstm, z$adultm), pmax(0, z$adultm - raw$lfstm)
    ),
    dim = c(numCT, 3, 2),
    dimnames = list(ctcode = raw$ctcode,
        adultinstp = c('0-14','15+ noninst', '15+ inst'),
        sexp = c('Female', 'Male'))
)
profile86i$adultinstp_sexp <- profile86i$ctcode_adultinstp_sexp[1,,]
profile86i$ctcode_adultinstp_sexp <- profile86i$ctcode_adultinstp_sexp[-1,,]


# Margins
profile86i$ctcode_adultinstp <- array(
    data = c(
        z$childf + z$childm,
        pmin(raw$edu, z$adultf + z$adultm),
        pmax(0, z$adultf + z$adultm - raw$edu)
    ),
    dim = c(numCT, 3),
    dimnames = list(ctcode = raw$ctcode,
        adultinstp = c('0-14', '15+ noninst', '15+ inst'))
)
profile86i$adultinstp <- array1(profile86i$ctcode_adultinstp)
profile86i$ctcode_adultinstp <- profile86i$ctcode_adultinstp[-1,]

profile86i$ctcode_sexp <- array(
    data = c(
        raw$agef, raw$agem
    ),
    dim = c(numCT, 2),
    dimnames = list(ctcode = raw$ctcode,
        sexp = c('Female', 'Male'))
)
profile86i$sexp <- array1(profile86i$ctcode_sexp)
profile86i$ctcode_sexp <- profile86i$ctcode_sexp[-1,]
remove(z)



# Create some err4/errct versions, using just the 15+ noninst data.
profile86i$adultnoninst_sexp <-
    err4(array1(profile86i$adultinstp_sexp, '15+ noninst'))
profile86i$ctcode_adultnoninst_sexp <-
    errct(profile86i$ctcode_adultinstp_sexp[,'15+ noninst',])
profile86i$adultnoninst <-
    err4(profile86i$adultinstp['15+ noninst'])
profile86i$ctcode_adultnoninst <-
    errct(array1(profile86i$ctcode_adultinstp, '15+ noninst', dim=2))

# TODO: apply err4/ct to adultinstp tables.




# The profile table breakdown by age and gender.
# * derived from 100% data (2A form)
# * includes institutional residents
# These are our best estimates for the 15-and-under population, but can't
# be compared with the SC86B04 estimates due to the above two issues.
profile86i$ctcode_sexp_agep <- array(
    data = c(raw$agef01, raw$agem01,
             raw$agef02, raw$agem02,
             raw$agef03, raw$agem03,
             raw$agef04, raw$agem04,
             raw$agef05, raw$agem05,
             raw$agef06, raw$agem06,
             raw$agef07, raw$agem07,
             raw$agef08, raw$agem08,
             raw$agef09, raw$agem09,
             raw$agef10, raw$agem10,
             raw$agef11, raw$agem11),
    dim = c(numCT, 2, 11),
    dimnames = list(ctcode = raw$ctcode,
        sexp = c('Female', 'Male'),
        agep = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-34', '35-44',
                 '45-54', '55-64', '65-74', '75+'))
)
profile86i$sexp_agep <- profile86i$ctcode_sexp_agep[1,,]
profile86i$ctcode_sexp_agep <- profile86i$ctcode_sexp_agep[-1,,]
profile86i$ctcode_agep <- margin.table2(profile86i$ctcode_sexp_agep,
                                        c('ctcode', 'agep'))
profile86i$agep <- margin.table2(profile86i$sexp_agep, c('agep'))



#
# This is the real future of these profile tables - dress them up to look
# like BSTs.
#

#
# Universe: 2B, full population
# Sex by income.
#
# A bit of interpretation: "with income" implies non-zero income, either
# positive or negative.
#
pt <- list()
pt$sexinc_2b <- list()
pt$sexinc_2b$data <- array(
    data = c(
             # This is income=0
             # Take difference from the 2B total by sex.
             # Force to non-negative number (due to 2B popn total errors)
             pmax(c(profile86i$adultnoninst_sexp$.data['Female'],
                    profile86i$ctcode_adultnoninst_sexp$.data[,'Female'])
                  - raw$incf, 0),
             pmax(c(profile86i$adultnoninst_sexp$.data['Male'],
                    profile86i$ctcode_adultnoninst_sexp$.data[,'Male'])
                  - raw$incm, 0),

             # This combines those with income <0 and income 0-999
             raw$incf01, raw$incm01,
             raw$incf02, raw$incm02,
             raw$incf03, raw$incm03,
             raw$incf04, raw$incm04,
             raw$incf05, raw$incm05,
             raw$incf06, raw$incm06,
             raw$incf07, raw$incm07,
             raw$incf08, raw$incm08,
             raw$incf09, raw$incm09,
             raw$incf10, raw$incm10,
             raw$incf11, raw$incm11),
    dim = c(numCT, 2, 12),
    dimnames = list(ctcode = raw$ctcode,
        sexp = c('Female', 'Male'),
        totincp = c('0', '<1.0k', '1.0-2.9k', '3.0-4.9k', '5.0-6.9k',
             '7.0-9.9k', '10.0-14.9k', '15.0-19.9k', '20.0-24.9k',
             '25.0-29.9k', '30.0-34.9k', '35.0k+'))
)
pt$sexinc_2b$margin <- list()
pt$sexinc_2b$margin$sexp_totincp <- err4(pt$sexinc_2b$data[1,,])
pt$sexinc_2b$data <- errct(pt$sexinc_2b$data[-1,,])
# TODO: we need to do an ipf_b to fit this to the 2B totals.

remove(raw)
