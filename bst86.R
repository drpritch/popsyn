#
# bst86.R
#
# Import the Basic Summary Tabulations from the 1986 census from the Postgres
# database.
#
# This is actually a fair bit of work, since the Postgres data (like the
# original raw CSV files) just has "r1c10" data. Each table needs to be
# organized into a coherent multi-dimensional table, arranged to allow easy
# connection with the PUMS.
#
# The actual level mapping / category collapsing to connect the two data
# sources is done in synthesize.R, however.
#

library('gdata')
library('abind')
source('ipf.R')
source('utils.R')

#########################################################
# Individual BSTs
#########################################################

makeBSTs <- function(profile86i, pum86, cmacodes) {

inputDataBST <- function(tablename) {
    result <- inputData(tablename, '*', sortColumns=c('ctcode', 'ca'),
        whereExpression = paste(makeOrQuery(field='ca', cmacodes),
        ' AND (ctcode>0 OR pca=0)', sep=''))
    result <- result[(result$ca %in% cmacodes) &
                     (result$ctcode>0 | result$pca==0),]
    # Sum all ctcode=0 rows into a single first row.
    if(length(cmacodes) > 1) {
        result <- rbind(cbind(result[1,1:6],
                   as.data.frame(lapply(result[result$ctcode==0,-(1:6)], sum))),
                   result[result$ctcode!=0,])
        result$ca[1] <- -1;
    }
    result
}

makeSC86B01 <- function() {
    # SC86B01
    #
    # Universe: non-institutional population (20% counts), ages 15+
    #

    # Notes:
    # - ctcode=0 indicates a total over several CTs. There are several
    #   different records with ctcode=0, each with a different pca representing
    #   the chunks of the CMA that are in different counties(?).
    # - ctcode=0 and pca=0 indicates the total for the entire CMA. For the
    #   moment, I'm keeping it as a valid census tract.
    raw <- inputDataBST('census.bst_ct1986_sc86b01_canada')
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))

    # Adjust area-suppressed regions, using 100% non-institutional population
    # totals.
    #
    # Note that CT 1020 gets wiped out by this... it's suppressed in the
    # profile tables, but not in some BSTs.
    raw <- adjustSuppressed(raw, profile86i$suppressed,
                    scale = profile86i$ctcode_adultnoninst$.data / raw$total[1],
                    ignoreFields = names(raw)[1:6])

    sc86b01 <- list(margin = list())
    sc86b01$data <- array(data = c(
            # Row 5 comes before row 4 to ensure female (1) is before male (2)
            raw$r5c1, raw$r5c2, raw$r5c3, raw$r5c4, raw$r5c5, raw$r5c6,
            raw$r4c1, raw$r4c2, raw$r4c3, raw$r4c4, raw$r4c5, raw$r4c6,
            raw$r8c1, raw$r8c2, raw$r8c3, raw$r8c4, raw$r8c5, raw$r8c6,
            raw$r7c1, raw$r7c2, raw$r7c3, raw$r7c4, raw$r7c5, raw$r7c6,
            raw$r11c1, raw$r11c2, raw$r11c3, raw$r11c4, raw$r11c5, raw$r11c6,
            raw$r10c1, raw$r10c2, raw$r10c3, raw$r10c4, raw$r10c5, raw$r10c6,
            raw$r14c1, raw$r14c2, raw$r14c3, raw$r14c4, raw$r14c5, raw$r14c6,
            raw$r13c1, raw$r13c2, raw$r13c3, raw$r13c4, raw$r13c5, raw$r13c6,
            raw$r17c1, raw$r17c2, raw$r17c3, raw$r17c4, raw$r17c5, raw$r17c6,
            raw$r16c1, raw$r16c2, raw$r16c3, raw$r16c4, raw$r16c5, raw$r16c6,
            raw$r20c1, raw$r20c2, raw$r20c3, raw$r20c4, raw$r20c5, raw$r20c6,
            raw$r19c1, raw$r19c2, raw$r19c3, raw$r19c4, raw$r19c5, raw$r19c6),
        dim = c(numCT, 6, 2, 6),
        dimnames = list(ctcode = raw$ctcode,
             hlosp = c('Less than grade 9', 'Grades 9-13', 'High school',
                       'Trades and non-uni', 'University w/o degree',
                       'University w/ degree'),
             sexp = levels(pum86$i$sexp),
             agep = c('15-24', levels(pum86$i$agep)[-1:-3]))
    )
    dn <- dimnames(sc86b01$data)
    sc86b01$margin$hlosp_sexp_agep <-
        err4(sc86b01$data[1,,,])
    sc86b01$data <-
        errct(sc86b01$data[-1,,,])

    # THREE-WAY MARGINS
    # Schooling by sex
    sc86b01$margin$ctcode_hlosp_sexp <- array(
        data = c(
            # Females
            raw$r2c1, raw$r2c2, raw$r2c3, raw$r2c4, raw$r2c5, raw$r2c6,
            # Males
            raw$r1c1, raw$r1c2, raw$r1c3, raw$r1c4, raw$r1c5, raw$r1c6),
        dim = c(numCT, 6, 2),
        dimnames = list(ctcode = dn$ctcode,
                        hlosp = dn$hlosp,
                        sexp = dn$sexp))
    sc86b01$margin$hlosp_sexp <-
        err4(sc86b01$margin$ctcode_hlosp_sexp[1,,])
    sc86b01$margin$ctcode_hlosp_sexp <-
        errct(sc86b01$margin$ctcode_hlosp_sexp[-1,,])

    # Schooling by age
    sc86b01$margin$ctcode_hlosp_agep <- array(
        data = c(
            raw$r3c1,  raw$r3c2,  raw$r3c3,  raw$r3c4,  raw$r3c5,  raw$r3c6,
            raw$r6c1,  raw$r6c2,  raw$r6c3,  raw$r6c4,  raw$r6c5,  raw$r6c6,
            raw$r9c1,  raw$r9c2,  raw$r9c3,  raw$r9c4,  raw$r9c5,  raw$r9c6,
            raw$r12c1, raw$r12c2, raw$r12c3, raw$r12c4, raw$r12c5, raw$r12c6,
            raw$r15c1, raw$r15c2, raw$r15c3, raw$r15c4, raw$r15c5, raw$r15c6,
            raw$r18c1, raw$r18c2, raw$r18c3, raw$r18c4, raw$r18c5, raw$r18c6),
        dim = c(numCT, 6, 6),
        dimnames = list(ctcode = dn$ctcode,
                        hlosp = dn$hlosp,
                        agep = dn$agep))
    sc86b01$margin$hlosp_agep <-
        err4(sc86b01$margin$ctcode_hlosp_agep[1,,])
    sc86b01$margin$ctcode_hlosp_agep <-
        errct(sc86b01$margin$ctcode_hlosp_agep[-1,,])

    # Sex by age
    sc86b01$margin$ctcode_sexp_agep <- array(
        data = c(
                raw$r5,  raw$r4,
                raw$r8,  raw$r7,
                raw$r11, raw$r10,
                raw$r14, raw$r13,
                raw$r17, raw$r16,
                raw$r20, raw$r19),
            dim = c(numCT, 2, 6),
        dimnames = list(ctcode = dn$ctcode,
                        sexp = dn$sexp,
                        agep = dn$agep))
    sc86b01$margin$sexp_agep <-
        err4(sc86b01$margin$ctcode_sexp_agep[1,,])
    sc86b01$margin$ctcode_sexp_agep <-
        errct(sc86b01$margin$ctcode_sexp_agep[-1,,])

    #
    # TWO-WAY MARGINALS
    #

    # Education
    sc86b01$margin$ctcode_hlosp <- array(
        data = c(raw$c1, raw$c2, raw$c3, raw$c4, raw$c5, raw$c6),
        dim = c(numCT, 6),
        dimnames = list(ctcode = dn$ctcode,
                        hlosp = dn$hlosp))
    sc86b01$margin$hlosp <-
        err4(array1(sc86b01$margin$ctcode_hlosp))
    sc86b01$margin$ctcode_hlosp <-
        err4(sc86b01$margin$ctcode_hlosp[-1,])
    assert(censusEqual(margin.table2(sc86b01$margin$hlosp_sexp_agep$.data,
                                     'hlosp'),
                       sc86b01$margin$hlosp$.data, (2*7)*5, 5))
    assert(censusEqual(margin.table2(sc86b01$data$.data, c('ctcode', 'hlosp')),
                       sc86b01$margin$ctcode_hlosp$.data, (2*7)*5, 5))
    #summary(abs(margin.table2(sc86b01$data$data, c('ctcode','hlosp')) -
    #               sc86b01$margin$ctcode_hlosp$data) - 2*7*5)


    # This is distinct from profile86i$adultnoninst_ctcode_sexp in that this is
    # based on the 20% noninstitutional universe, while profile is the 100%
    # estimate of the noninstitutional universe.
    sc86b01$margin$ctcode_sexp <- array(
        data = c(raw$r2, raw$r1),
        dim = c(numCT, 2),
        dimnames = list(ctcode = dn$ctcode,
                        sexp = dn$sexp))
    sc86b01$margin$sexp <-
        err4(array1(sc86b01$margin$ctcode_sexp))
    sc86b01$margin$ctcode_sexp <-
        errct(sc86b01$margin$ctcode_sexp[-1,])
    assert(censusEqual(margin.table2(sc86b01$margin$hlosp_sexp_agep$.data,
                                     'sexp'),
                       sc86b01$margin$sexp$.data, (6*7)*5, 5))
    assert(censusEqual(margin.table2(sc86b01$data$.data, c('ctcode','sexp')),
                       sc86b01$margin$ctcode_sexp$.data, (6*7)*5, 5))

    sc86b01$margin$ctcode_agep <- array(
        data = c(raw$r3, raw$r6, raw$r9, raw$r12, raw$r15, raw$r18),
        dim = c(numCT, 6),
        dimnames = list(ctcode = dn$ctcode,
                        agep = dn$agep))
    sc86b01$margin$agep <-
        err4(array1(sc86b01$margin$ctcode_agep))
    sc86b01$margin$ctcode_agep <-
        errct(sc86b01$margin$ctcode_agep[-1,])
    assert(censusEqual(margin.table2(sc86b01$margin$hlosp_sexp_agep$.data,
                                     'agep'),
                       sc86b01$margin$agep$.data, (6*2)*5, 5))
    assert(censusEqual(margin.table2(sc86b01$data$.data, c('ctcode','agep')),
                       sc86b01$margin$ctcode_agep$.data, (6*2)*5, 5))

    sc86b01$margin$ctcode <- array(raw$total)
    dimnames(sc86b01$margin$ctcode) <- list(ctcode = dn$ctcode)
    sc86b01$total <-
        err4(sc86b01$margin$ctcode[1])
    sc86b01$margin$ctcode <-
        errct(sc86b01$margin$ctcode[-1])

    # Tweak this BST so that it satisfies its marginal constraints
    # Surprisingly, this seems to require more than +/- 4 to some cells.
    # wtf?
    if(FALSE) {
    sc86b01$ipf <- IpfConstraint(ipf(constraintList = list(
            sc86b01$margin$ctcode_hlosp_sexp,
            sc86b01$margin$ctcode_hlosp_agep,
            sc86b01$margin$ctcode_sexp_agep,
            sc86b01$margin$hlosp_sexp_agep,
            sc86b01$margin$ctcode_hlosp,
            sc86b01$margin$ctcode_sexp,
            sc86b01$margin$ctcode_agep,
            sc86b01$margin$hlosp_sexp,
            sc86b01$margin$hlosp_agep,
            sc86b01$margin$sexp_agep,
            sc86b01$margin$ctcode,
            sc86b01$margin$hlosp,
            sc86b01$margin$sexp,
            sc86b01$margin$agep,
            sc86b01$total),
        priorArray = sc86b01$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    sc86b01
}

makeFake <- function(sc86b01) {
    # Disaggregate the 15-24 age group to 15-17, 18-19 and 20-24 groups as
    # best as possible.
    
    # Step one: split 15-24 into 15-19 and 20-24
    #
    # We do this using the total non-institutional 15-24
    # population (from SC86B01) and split it in half using the known ratio of
    # 15-19 to 20-24 year olds by gender by census tract. This assumes that the
    # institutional population is small relative to non-institutional, and
    # does not affect the ratio... a fair assumption.
    #
    # Ideally, we might replace with DM86A01... but it's defined on the 100%
    # institutional population.
    #
    # Step two: split 15-19 into 15-17 and 18-19 groups.
    # We have basically no data for this other than the PUM - very
    # unfortunate, since there's definitely a geographic trend here.
    frac1519 <- profile86i$sexp_agep[,'15-19'] /
        (profile86i$sexp_agep[,'15-19'] +
         profile86i$sexp_agep[,'20-24'])
    frac1519 <- ifelse(is.nan(frac1519), 0, frac1519)
    dn <- dimnames(sc86b01$margin$ctcode_sexp_agep$.data)
    dn$agep <- levels(pum86$i$agep)

    frac1517 <- sum(pum86$i$agep == '15-17')
    frac1517 <- frac1517 / (frac1517 + sum(pum86$i$agep == '18-19'))

    fake <- list(margin = list())
    fake$margin$sexp_agep <- abind(
        sc86b01$margin$sexp_agep$.data[,'15-24'] * frac1519 * frac1517,
        sc86b01$margin$sexp_agep$.data[,'15-24'] * frac1519 * (1-frac1517),
        sc86b01$margin$sexp_agep$.data[,'15-24'] * (1-frac1519),
        sc86b01$margin$sexp_agep$.data[,-1],
        along = 2)
    dimnames(fake$margin$sexp_agep) <- list(sexp = dn$sexp, agep = dn$agep)
    fake$margin$sexp_agep <- err4(fake$margin$sexp_agep)

    # Data: ct x sex x age
    frac1519 <- profile86i$ctcode_sexp_agep[,,'15-19'] /
        (profile86i$ctcode_sexp_agep[,,'15-19'] +
         profile86i$ctcode_sexp_agep[,,'20-24'])
    frac1519 <- ifelse(is.nan(frac1519), 0, frac1519)

    fake$data <- abind(
        sc86b01$margin$ctcode_sexp_agep$.data[,,'15-24']
            * frac1519 * frac1517,
        sc86b01$margin$ctcode_sexp_agep$.data[,,'15-24']
            * frac1519 * (1-frac1517),
        sc86b01$margin$ctcode_sexp_agep$.data[,,'15-24']
            * (1-frac1519),
        sc86b01$margin$ctcode_sexp_agep$.data[,,-1],
        along = 3)

    dimnames(fake$data) <- dn
    fake$data <- errct(fake$data)

    fake
}

makeLF86B01 <- function(sc86b01) {
    #
    # LF86B01
    #
    # Universe: population (20% counts), ages 15+

    raw <- inputDataBST('census.bst_ct1986_lf86b01_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))

    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B04 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])


    lf86b01 <- list(margin = list())
    lf86b01$data <- array(
        data = c(
            raw$r5c2,  raw$r5c3,  raw$r5c6,  raw$r4c2,  raw$r4c3,  raw$r4c6,
            raw$r8c2,  raw$r8c3,  raw$r8c6,  raw$r7c2,  raw$r7c3,  raw$r7c6,
            raw$r11c2, raw$r11c3, raw$r11c6, raw$r10c2, raw$r10c3, raw$r10c6,
            raw$r14c2, raw$r14c3, raw$r14c6, raw$r13c2, raw$r13c3, raw$r13c6,
            raw$r17c2, raw$r17c3, raw$r17c6, raw$r16c2, raw$r16c3, raw$r16c6,
            raw$r20c2, raw$r20c3, raw$r20c6, raw$r19c2, raw$r19c3, raw$r19c6
        ),
        dim = c(numCT, 3, 2, 6),
        dimnames = list(ctcode = raw$ctcode,
                        lfact = c('Employed', 'Unemployed', 'Not labour force'),
                        sexp = levels(pum86$i$sexp),
                        agep = c('15-24', levels(pum86$i$agep)[-1:-3]))
    )
    dn <- dimnames(lf86b01$data)
    lf86b01$margin$lfact_sexp_agep <- err4(lf86b01$data[1,,,])
    lf86b01$data <- errct(lf86b01$data[-1,,,])

    lf86b01$margin$ctcode_lfact_sexp <- array(
        data = c(raw$r2c2,  raw$r2c3,  raw$r2c6,
                 raw$r1c2,  raw$r1c3,  raw$r1c6),
        dim = c(numCT, 3, 2),
        dimnames = list(ctcode = dn$ctcode, lfact=dn$lfact, sexp=dn$sexp))
    lf86b01$margin$lfact_sexp <-
        err4(lf86b01$margin$ctcode_lfact_sexp[1,,])
    lf86b01$margin$ctcode_lfact_sexp <-
        errct(lf86b01$margin$ctcode_lfact_sexp[-1,,])

    lf86b01$margin$ctcode_lfact_agep <- array(
        data = c(raw$r3c2,  raw$r3c3,  raw$r3c6,
                 raw$r6c2,  raw$r6c3,  raw$r6c6,
                 raw$r9c2,  raw$r9c3,  raw$r9c6,
                 raw$r12c2, raw$r12c3, raw$r12c6,
                 raw$r15c2, raw$r15c3, raw$r15c6,
                 raw$r18c2, raw$r18c3, raw$r18c6),
        dim = c(numCT, 3, 6),
        dimnames = list(ctcode = dn$ctcode, lfact=dn$lfact, agep=dn$agep))
    lf86b01$margin$lfact_agep <-
        err4(lf86b01$margin$ctcode_lfact_agep[1,,])
    lf86b01$margin$ctcode_lfact_agep <-
        errct(lf86b01$margin$ctcode_lfact_agep[-1,,])

    lf86b01$margin$ctcode_sexp_agep <- array(
        data = c(raw$r5,  raw$r4,
                 raw$r8,  raw$r7,
                 raw$r11, raw$r10,
                 raw$r14, raw$r13,
                 raw$r17, raw$r16,
                 raw$r20, raw$r19),
        dim = c(numCT, 2, 6),
        dimnames = list(ctcode = dn$ctcode, sexp=dn$sexp, agep=dn$agep))
    lf86b01$margin$sexp_agep <-
        err4(lf86b01$margin$ctcode_sexp_agep[1,,])
    lf86b01$margin$ctcode_sexp_agep <-
        errct(lf86b01$margin$ctcode_sexp_agep[-1,,])

    lf86b01$margin$ctcode_lfact <- array(
        data = c(raw$c2,  raw$c3,  raw$c6),
        dim = c(numCT, 3),
        dimnames = list(ctcode = dn$ctcode, lfact=dn$lfact))
    lf86b01$margin$lfact <-
        err4(array1(lf86b01$margin$ctcode_lfact))
    lf86b01$margin$ctcode_lfact <-
        errct(lf86b01$margin$ctcode_lfact[-1,])

    lf86b01$margin$ctcode_sexp <- array(
        data = c(raw$r2,  raw$r1),
        dim = c(numCT, 2),
        dimnames = list(ctcode = dn$ctcode, sexp=dn$sexp))
    lf86b01$margin$sexp <-
        err4(array1(lf86b01$margin$ctcode_sexp))
    lf86b01$margin$ctcode_sexp <-
        errct(lf86b01$margin$ctcode_sexp[-1,])

    lf86b01$margin$ctcode_agep <- array(
        data = c(raw$r3,  raw$r6,  raw$r9,  raw$r12,
                 raw$r15, raw$r18),
        dim = c(numCT, 6),
        dimnames = list(ctcode = dn$ctcode, agep=dn$agep))
    lf86b01$margin$agep <-
        err4(array1(lf86b01$margin$ctcode_agep))
    lf86b01$margin$ctcode_agep <-
        errct(lf86b01$margin$ctcode_agep[-1,])

    lf86b01$margin$ctcode <- array(raw$total)
    dimnames(lf86b01$margin$ctcode) <- list(ctcode = dn$ctcode)
    lf86b01$total <-
        err4(lf86b01$margin$ctcode[1])
    lf86b01$margin$ctcode <-
        errct(lf86b01$margin$ctcode[-1])

    # Tweak this BST so that it satisfies its marginal constraints
    if(FALSE) {
    lf86b01$ipf <- IpfConstraint(ipf(constraintList = list(
            lf86b01$margin$ctcode_lfact_sexp,
            lf86b01$margin$ctcode_lfact_agep,
            lf86b01$margin$ctcode_lfact,
            lf86b01$margin$ctcode_sexp,
            lf86b01$margin$ctcode_agep,
            lf86b01$margin$lfact_sexp_agep,
            lf86b01$margin$lfact_sexp,
            lf86b01$margin$lfact_agep,
            lf86b01$margin$sexp_agep,
            lf86b01$margin$ctcode,
            lf86b01$margin$lfact,
            lf86b01$margin$sexp,
            lf86b01$margin$agep,
            lf86b01$total),
        priorArray = lf86b01$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    lf86b01
}

makeLF86B03 <- function(sc86b01) {
    #
    # LF86B03
    #
    # Universe: population (20% counts), ages 15+

    raw <- inputDataBST('census.bst_ct1986_lf86b03_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))

    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B04 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])


    lf86b03 <- list(margin = list())
    lf86b03$data <- array(
        data = c(
            # Rows 1 and 2 are totals by sex
            # Rows 3,6,9,... are totals for each hlosp
            raw$r5c2,  raw$r5c3,  raw$r5c6,  raw$r4c2,  raw$r4c3,  raw$r4c6,
            raw$r8c2,  raw$r8c3,  raw$r8c6,  raw$r7c2,  raw$r7c3,  raw$r7c6,
            raw$r11c2, raw$r11c3, raw$r11c6, raw$r10c2, raw$r10c3, raw$r10c6,
            raw$r14c2, raw$r14c3, raw$r14c6, raw$r13c2, raw$r13c3, raw$r13c6,
            raw$r17c2, raw$r17c3, raw$r17c6, raw$r16c2, raw$r16c3, raw$r16c6,
            raw$r20c2, raw$r20c3, raw$r20c6, raw$r19c2, raw$r19c3, raw$r19c6,
            raw$r23c2, raw$r23c3, raw$r23c6, raw$r22c2, raw$r22c3, raw$r22c6
        ),
        dim = c(numCT, 3, 2, 7),
        dimnames = list(ctcode = raw$ctcode,
                        lfact = c('Employed', 'Unemployed', 'Not labour force'),
                        sexp = levels(pum86$i$sexp),
                        hlosp = c('Less than grade 9', 'Grades 9-13',
                                  'High school',
                                  'Trade certificate/diploma/non-uni',
                                  'Uni/Non-uni w/o',
                                  'Uni/Non-uni w/ diploma',
                                  'University')))
    dn <- dimnames(lf86b03$data)
    lf86b03$margin$lfact_sexp_hlosp <- err4(lf86b03$data[1,,,])
    lf86b03$data <- errct(lf86b03$data[-1,,,])

    lf86b03$margin$ctcode_lfact_sexp <- array(
        data = c(raw$r2c2,  raw$r2c3,  raw$r2c6,
                 raw$r1c2,  raw$r1c3,  raw$r1c6),
        dim = c(numCT, 3, 2),
        dimnames = list(ctcode = dn$ctcode, lfact=dn$lfact, sexp=dn$sexp))
    lf86b03$margin$lfact_sexp <-
        err4(lf86b03$margin$ctcode_lfact_sexp[1,,])
    lf86b03$margin$ctcode_lfact_sexp <-
        errct(lf86b03$margin$ctcode_lfact_sexp[-1,,])

    lf86b03$margin$ctcode_lfact_hlosp <- array(
        data = c(raw$r3c2,  raw$r3c3,  raw$r3c6,
                 raw$r6c2,  raw$r6c3,  raw$r6c6,
                 raw$r9c2,  raw$r9c3,  raw$r9c6,
                 raw$r12c2, raw$r12c3, raw$r12c6,
                 raw$r15c2, raw$r15c3, raw$r15c6,
                 raw$r18c2, raw$r18c3, raw$r18c6,
                 raw$r21c2, raw$r21c3, raw$r21c6),
        dim = c(numCT, 3, 7),
        dimnames = list(ctcode = dn$ctcode, lfact=dn$lfact, hlosp=dn$hlosp))
    lf86b03$margin$lfact_hlosp <-
        err4(lf86b03$margin$ctcode_lfact_hlosp[1,,])
    lf86b03$margin$ctcode_lfact_hlosp <-
        errct(lf86b03$margin$ctcode_lfact_hlosp[-1,,])

    lf86b03$margin$ctcode_sexp_hlosp <- array(
        data = c(raw$r5,  raw$r4,
                 raw$r8,  raw$r7,
                 raw$r11, raw$r10,
                 raw$r14, raw$r13,
                 raw$r17, raw$r16,
                 raw$r20, raw$r19,
                 raw$r23, raw$r22),
        dim = c(numCT, 2, 7),
        dimnames = list(ctcode = dn$ctcode, sexp=dn$sexp, hlosp=dn$hlosp))
    lf86b03$margin$sexp_hlosp <-
        err4(lf86b03$margin$ctcode_sexp_hlosp[1,,])
    lf86b03$margin$ctcode_sexp_hlosp <-
        errct(lf86b03$margin$ctcode_sexp_hlosp[-1,,])

    lf86b03$margin$ctcode_lfact <- array(
        data = c(raw$c2,  raw$c3,  raw$c6),
        dim = c(numCT, 3),
        dimnames = list(ctcode = dn$ctcode, lfact=dn$lfact))
    lf86b03$margin$lfact <-
        err4(array1(lf86b03$margin$ctcode_lfact))
    lf86b03$margin$ctcode_lfact <-
        errct(lf86b03$margin$ctcode_lfact[-1,])

    lf86b03$margin$ctcode_sexp <- array(
        data = c(raw$r2,  raw$r1),
        dim = c(numCT, 2),
        dimnames = list(ctcode = dn$ctcode, sexp=dn$sexp))
    lf86b03$margin$sexp <-
        err4(array1(lf86b03$margin$ctcode_sexp))
    lf86b03$margin$ctcode_sexp <-
        errct(lf86b03$margin$ctcode_sexp[-1,])

    lf86b03$margin$ctcode_hlosp <- array(
        data = c(raw$r3,  raw$r6,  raw$r9,  raw$r12,
                 raw$r15, raw$r18, raw$r21),
        dim = c(numCT, 7),
        dimnames = list(ctcode = dn$ctcode, hlosp=dn$hlosp))
    lf86b03$margin$hlosp <-
        err4(array1(lf86b03$margin$ctcode_hlosp))
    lf86b03$margin$ctcode_hlosp <-
        errct(lf86b03$margin$ctcode_hlosp[-1,])

    lf86b03$margin$ctcode <- array(raw$total)
    dimnames(lf86b03$margin$ctcode) <- list(ctcode = dn$ctcode)
    lf86b03$total <-
        err4(lf86b03$margin$ctcode[1])
    lf86b03$margin$ctcode <-
        errct(lf86b03$margin$ctcode[-1])

    # Tweak this BST so that it satisfies its marginal constraints
    if(FALSE) {
    lf86b03$ipf <- IpfConstraint(ipf(constraintList = list(
            lf86b03$margin$ctcode_lfact_sexp,
            lf86b03$margin$ctcode_lfact_hlosp,
            lf86b03$margin$ctcode_lfact,
            lf86b03$margin$ctcode_sexp,
            lf86b03$margin$ctcode_hlosp,
            lf86b03$margin$lfact_sexp_hlosp,
            lf86b03$margin$lfact_sexp,
            lf86b03$margin$lfact_hlosp,
            lf86b03$margin$sexp_hlosp,
            lf86b03$margin$ctcode,
            lf86b03$margin$lfact,
            lf86b03$margin$sexp,
            lf86b03$margin$hlosp,
            lf86b03$total),
        priorArray = lf86b03$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    lf86b03
}

makeLF86B04 <- function(sc86b01) {
    #
    # LF86B04
    #
    # Universe: labour force (20% counts), ages 15+
    #
    # Basically the same as above, only easier - no weird age variable, and
    # only two variables per census tract.
    #
    # The only trick is that the population of LF86B04 is only the labour
    # force, while the target population is the non-institutional
    # adult population. To get the number of adults outside the labour force
    # (and outside institutions), I have to do use the profile table.

    raw <- inputDataBST('census.bst_ct1986_lf86b04_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))

    # Adjust area-suppressed regions.
    # Note that CT 1020 gets wiped out by this... it's suppressed in the
    # profile tables, but not in some BSTs.
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # Very careful here... the original table's universe is
            # the labour force (20% estimate), so we scale by its universe.
            # 
            # scale = (profile86i$ctcode_adultnoninst$.data / raw$total[1]) *
            #   (raw$total[1] / sc86b01$total$.data)
            #
            # line 1: noninst pop for this zone (100% estimate, only available)
            # line 2: expected ratio of labour force to noninst pop (20%
            #    estimate)
            #
            # (We later add an extra row to make the universe the
            # non-institutional population, 20% estimate.)
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    # Total non-institutional population by gender (20% estimates)
    raw$noninstf <- c(sc86b01$margin$sexp$.data['Female'],
                      sc86b01$margin$ctcode_sexp$.data[,'Female'])
    raw$noninstm <- c(sc86b01$margin$sexp$.data['Male'],
                      sc86b01$margin$ctcode_sexp$.data[,'Male'])

    lf86b04 <- list(margin = list())
    lf86b04$data <- array(
        data = c(
            # Column 2 comes before column 1 to ensure females come before
            # males, as in the PUMS data.

            # Row 3 - Management
            raw$r3c2,   raw$r3c1,
            # Row 4 - Nat sci, eng, math
            raw$r4c2,   raw$r4c1,
            # Row 5 - Soc sci
            raw$r5c2,   raw$r5c1,
            # Row 7 - Teaching
            raw$r7c2,   raw$r7c1,
            # Row 8 - Medicine
            raw$r8c2,   raw$r8c1,
            # Row 9 - Art
            raw$r9c2,   raw$r9c1,
            # Row 10 - Clerical
            raw$r10c2,  raw$r10c1,
            # Row 11 - Sales
            raw$r11c2,  raw$r11c1,
            # Row 12 - Service
            raw$r12c2,  raw$r12c1,
            # Rows 13-16 - Primary (Farming, Fishing, Forestry, Mining)
            raw$r13c2 + raw$r14c2 + raw$r15c2 + raw$r16c2,
            raw$r13c1 + raw$r14c1 + raw$r15c1 + raw$r16c1,
            # Row 17 - Processing
            raw$r17c2,  raw$r17c1,
            # Rows 18-19 - Machining, product fabrication, assembly, repair
            raw$r18c2 + raw$r19c2,
            raw$r18c1 + raw$r19c1,
            # Row 20 - Construction
            raw$r20c2,  raw$r20c1,
            # Row 21 - Transportation Equipment Operation
            raw$r21c2,  raw$r21c1,
            # Rows 6,22-24 - Other (Religion, material handling (nec),
            # other crafts and equipment, other not elsewhere covered)
            raw$r6c2 + raw$r22c2 + raw$r23c2 + raw$r24c2,
            raw$r6c1 + raw$r22c1 + raw$r23c1 + raw$r24c1,
            # Row 1:        N/A (inexp)
            # noninst - lf: N/A (not in labour force)
            raw$r1c2 + raw$noninstf - raw$c2,
            raw$r1c1 + raw$noninstm - raw$c1),
        dim = c(numCT, 2, 16),
        dimnames = list(ctcode = raw$ctcode, sexp = levels(pum86$i$sexp),
                        occ81p = c('Manag admin & related occns',
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
    dn <- dimnames(lf86b04$data)
    lf86b04$margin$sexp_occ81p <-
        err4(lf86b04$data[1,,])
    # Also test for entries less than zero, and set to zero - this can happen
    # due to rounding error.
    lf86b04$data <-
        errct(pmax(lf86b04$data[-1,,], 0))

    lf86b04$margin$ctcode_occ81p <- array(
        data = c(raw$r3,  raw$r4,  raw$r5,  raw$r7,  raw$r8,  raw$r9,
                 raw$r10, raw$r11, raw$r12,
                 raw$r13 + raw$r14 + raw$r15 + raw$r16,
                 raw$r17,
                 raw$r18 + raw$r19,
                 raw$r20, raw$r21,
                 raw$r6 + raw$r22 + raw$r23 + raw$r24,
                 raw$r1 + raw$noninstf + raw$noninstm - raw$total),
        dim = c(numCT, 16),
        dimnames = list(ctcode = dn$ctcode,
                        occ81p = dn$occ81p))
    lf86b04$margin$occ81p <-
        err4(array1(lf86b04$margin$ctcode_occ81p))
    lf86b04$margin$ctcode_occ81p <-
        errct(lf86b04$margin$ctcode_occ81p[-1,])
    assert(censusEqual(margin.table2(lf86b04$data$.data, c('ctcode','occ81p')),
                       lf86b04$margin$ctcode_occ81p$.data, 16*5, 4*5))

    # Tweak this BST so that it satisfies its marginal constraints
    if(FALSE) {
    lf86b04$ipf <- IpfConstraint(ipf(constraintList = list(
            sc86b01$margin$ctcode_sexp,
            lf86b04$margin$ctcode_occ81p,
            lf86b04$margin$sexp_occ81p,
            sc86b01$margin$ctcode,
            sc86b01$margin$sexp,
            lf86b04$margin$occ81p,
            sc86b01$total),
        priorArray = lf86b04$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    lf86b04
}

makeCF86A04 <- function(sc86b01) {
    #
    # CF86A04
    #
    # Universe: population in private dwellings (100% counts).
    #
    # Brutal. It would be nice to have separate totals for
    # the <=15 and >15 populations, but that's not possible right now.
    #
    # Note that the raw table uses the 100% estimates of the
    # non-institutional and non-collective population - and hence doesn't line
    # up well with SC86B01 and LF86B04.
    #
    # However, we
    # 1) add a row for the collective population, making the universe the
    #    non-institutional population.
    # 2) estimate the ctcode x sex x age breakdown for the collective
    #    population, using the deltas between SC86B01 and CF86A04 as a
    #    guide to the margins of this population.
    # 3) adjust the full population to match the known SC86B01 totals, to
    #    account for any remaining A/B differences.
    # Ultimately, it might be useful to use the 100% estimates - but
    # that'll be brutal.

    raw <- inputDataBST('census.bst_ct1986_cf86a04_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B04 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    cf86a04 <- list(margin = list())

    # A modest trick to help disaggregate the "child 25+" x gender into
    # multiple age groups. There's probably some way to do this with IPF... but
    # who cares.
    map_pum_cf86a04 <- 
        LevelMap(factor = 'agep',
                 map = c(1,1,1,2,2,3,3,4),
                 levels = 1:4,
                 labels = c('15-24','25-44','45-64','65+'))
    # 1) Use the PUM to get an age x sex x cfstat table, and keep only the
    # relevant levels of cfstat.
    childsplit <- xtabs(data = pum86$i, formula = ~agep+sexp+cfstat)[,,
        c('Child w/ parents', 'Child w/ lone parent')]
    # Verify that the level names haven't changed.
    assert(dim(childsplit)[3] == 2)
    # 2) Add the two cfstat levels, and then collapse the age levels to match
    # this table.
    childsplit <- collapse.array(margin.table2(childsplit, c('agep','sexp')),
                                 map_pum_cf86a04)
    # 3) Remove the first row (15-24) and normalise the remaining columns, so
    # that we have the fraction of each 25+ gender in each age bracket.
    #
    # We could at this stage keep separate columns for male/female, but I
    # think it's better to pool the data and get a larger sample size.
    # Especially since otherwise we get no 65+ female children.
    # Ideally, we'd use the Canada-wide PUMS to estimate this, actually.
    childsplit <- prop.table(margin.table2(childsplit[-1,], 'agep'))
    remove(map_pum_cf86a04)

    cf86a04$data <- array(
        data = c(
            # Column 2 comes before column 1 to ensure females come before
            # males, as in the PUMS data.

            # HUSBAND-WIFE
            raw$r1c2,  raw$r1c1,
            raw$r2c2,  raw$r2c1,
            raw$r3c2,  raw$r3c1,
            raw$r4c2,  raw$r4c1,
            # LONE PARENTS
            raw$r6c2,  raw$r6c1,
            raw$r7c2,  raw$r7c1,
            raw$r8c2,  raw$r8c1,
            raw$r9c2,  raw$r9c1,
            # CHILDREN
            raw$r13c2+raw$r14c2,         raw$r13c1+raw$r14c1,
            round(c(raw$r15c2, raw$r15c1) * childsplit['25-44']),
            round(c(raw$r15c2, raw$r15c1) * childsplit['45-64']),
            round(c(raw$r15c2, raw$r15c1) * childsplit['65+']),
            # NOT IN FAMILY
            raw$r19c2, raw$r19c1,
            raw$r20c2, raw$r20c1,
            raw$r21c2, raw$r21c1,
            raw$r22c2, raw$r22c1,
            # NA - to be filled in later
            rep(0, numCT * 8)
        ),
        dim = c(numCT, 2, 4, 5),
        dimnames = list(ctcode = raw$ctcode, sexp = levels(pum86$i$sexp),
                        agep = c('15-24','25-44','45-64', '65+'),
                        cfstat = c('Partner', 'Lone parent', 'Child',
                        'Non-CF person', 'NA'))
    )
    dn <- dimnames(cf86a04$data)
    cf86a04$margin$sexp_agep_cfstat <- cf86a04$data[1,,,]
    cf86a04$data <- cf86a04$data[-1,,,]


    #
    # MARGINS
    #
    # We only include the margins here that we have meaningful amounts of data
    # for. This BST is weird enough (and error-filled enough) that many margins
    # are useless compared to, say, SC86B01.
    #
    cf86a04$margin$ctcode_agep_cfstat <- array(
        data = c(
            # Column 2 comes before column 1 to ensure females come before
            # males, as in the PUMS data.

            # HUSBAND-WIFE
            raw$r1, raw$r2, raw$r3, raw$r4,
            # LONE PARENTS
            raw$r6, raw$r7, raw$r8, raw$r9,
            # CHILDREN
            raw$r13+raw$r14,
            round(raw$r15 * childsplit['25-44']),
            round(raw$r15 * childsplit['45-64']),
            round(raw$r15 * childsplit['65+']),
            # NOT IN FAMILY
            raw$r19, raw$r20, raw$r21, raw$r22,
            # NA - to be filled in later
            rep(0, numCT * 4)
        ),
        dim = c(numCT, 4, 5),
        dimnames = list(ctcode = dn$ctcode, agep = dn$agep, cfstat = dn$cfstat)
    )
    cf86a04$margin$agep_cfstat <- cf86a04$margin$ctcode_agep_cfstat[1,,]
    cf86a04$margin$ctcode_agep_cfstat <- cf86a04$margin$ctcode_agep_cfstat[-1,,]
    remove(childsplit)

    cf86a04$margin$ctcode_sexp_cfstat <- array(
        data = c(
            raw$r5c2,  raw$r5c1,
            raw$r10c2, raw$r10c1,
            raw$r16c2 - raw$r11c2 - raw$r12c2,
            raw$r16c1 - raw$r11c1 - raw$r12c1,
            raw$r23c2 - raw$r18c2,
            raw$r23c1 - raw$r18c1,
            # NA - to be filled in later
            rep(0, numCT * 2)
        ),
        dim = c(numCT, 2, 5),
        dimnames = list(ctcode = dn$ctcode, sexp = dn$sexp,
                        cfstat = dn$cfstat)
    )
    cf86a04$margin$sexp_cfstat <-
        cf86a04$margin$ctcode_sexp_cfstat[1,,]
    cf86a04$margin$ctcode_sexp_cfstat <-
        cf86a04$margin$ctcode_sexp_cfstat[-1,,]

    cf86a04$margin$ctcode_sexp <- array(
        # Remove ages 0-14
        data = c(raw$c2 - raw$r11c2 - raw$r12c2,
                 raw$c1 - raw$r11c1 - raw$r12c1),
        dim = c(numCT, 2),
        dimnames = list(ctcode = dn$ctcode, sexp = dn$sexp)
    )
    cf86a04$margin$sexp <- array1(cf86a04$margin$ctcode_sexp)
    cf86a04$margin$ctcode_sexp <- cf86a04$margin$ctcode_sexp[-1,]

    cf86a04$margin$ctcode_cfstat <- array(
        data = c(
            raw$r5,
            raw$r10,
            raw$r16 - raw$r11 - raw$r12,
            raw$r23 - raw$r18,
            # NA - to be filled in later
            rep(0, numCT * 2)
        ),
        dim = c(numCT, 5),
        dimnames = list(ctcode = dn$ctcode, cfstat = dn$cfstat)
    )
    cf86a04$margin$cfstat <- array1(cf86a04$margin$ctcode_cfstat)
    cf86a04$margin$ctcode_cfstat <- cf86a04$margin$ctcode_cfstat[-1,]

    a <- ipf(
        constraintList = list(
            errct(cf86a04$margin$ctcode_sexp),
            errct(margin.table2(cf86a04$margin$ctcode_agep_cfstat,
                                c('ctcode', 'agep'))),
            err4(cf86a04$margin$sexp),
            err4(margin.table2(cf86a04$margin$agep_cfstat, 'agep'))
        ),
        priorArray = margin.table2(cf86a04$data, c('ctcode', 'sexp', 'agep'))
    )
    map_sc86b01_cf86a04 <- 
        LevelMap(factor = 'agep',
                 map = c(1,2,2,3,3,4),
                 levels = 1:4,
                 labels = c('15-24','25-44','45-64','65+'))
    # "B" margins by zone, sex, age.
    b <- list()
    b$ctcode_sexp_agep <- 
        collapse.array(sc86b01$margin$ctcode_sexp_agep$.data,
                              map_sc86b01_cf86a04)
    b$sexp_agep <-
        collapse.array(sc86b01$margin$sexp_agep$.data,
                              map_sc86b01_cf86a04)
    b$ctcode_agep <- 
        collapse.array(sc86b01$margin$ctcode_agep$.data,
                              map_sc86b01_cf86a04)
    b$agep <- 
        collapse.array(sc86b01$margin$agep$.data,
                              map_sc86b01_cf86a04)
    b$ctcode_sexp <- 
        sc86b01$margin$ctcode_sexp$.data
    b$sexp <- 
        sc86b01$margin$sexp$.data

    remove(map_sc86b01_cf86a04)

    # We now have our best estimate of the ct x sex x age breakdown in this
    # table (a), and the ct x sex x age breakdown from SC86B01 (b).
    # These are defined on different populations:
    #   a: excludes collective residents, 100% sample
    #   b: includes collective residents, 20% sample
    # Therefore, max(b - a, 0) is our best guess of the ct x sex x age
    # breakdown of collective residents. We adjust that data to match the
    # margins of 

    na <- ipf(
        constraintList = list(
            IpfConstraint(
                pmax(b$ctcode_agep -
                    margin.table2(cf86a04$margin$ctcode_agep_cfstat,
                                  c('ctcode', 'agep')),
                    0)),
            IpfConstraint(
                pmax(b$ctcode_sexp -
                    margin.table2(cf86a04$margin$ctcode_sexp_cfstat,
                                  c('ctcode', 'sexp')),
                    0)),
            IpfConstraint(
                pmax(b$agep - margin.table2(cf86a04$margin$agep_cfstat, 'agep'),
                    0)),
            IpfConstraint(
                pmax(b$sexp - margin.table2(cf86a04$margin$sexp_cfstat, 'sexp'),
                    0))
        ),
        priorArray = pmax(b$ctcode_sexp_agep - a, 0)
    )
    cf86a04$data[,,,'NA'] <- na
    cf86a04$margin$sexp_agep_cfstat[,,'NA'] <-
        margin.table2(na, c('sexp', 'agep'))
    cf86a04$margin$ctcode_agep_cfstat[,,'NA'] <-
        margin.table2(na, c('ctcode', 'agep'))
    cf86a04$margin$agep_cfstat[,'NA'] <-
        margin.table2(na, c('agep'))
    cf86a04$margin$ctcode_sexp_cfstat[,,'NA'] <-
        margin.table2(na, c('ctcode', 'sexp'))
    cf86a04$margin$sexp_cfstat[,'NA'] <-
        margin.table2(na, c('sexp'))
    cf86a04$margin$ctcode_cfstat[,'NA'] <-
        margin.table2(na, c('ctcode'))
    cf86a04$margin$cfstat['NA'] <-
        margin.table2(na)

    # Kill off the margins. They no longer add any value. Only keep the
    # ctcode-less one, since that's generally expected to be around.
    cf86a04$margin_orig <- cf86a04$margin
    cf86a04$margin <- list(
        sexp_agep_cfstat = IpfConstraint(cf86a04$margin_orig$sexp_agep_cfstat)
    )
    cf86a04$data <- errct(cf86a04$data)


    # Tweak this BST so that it satisfies the 2B marginal constraints
    # We still call it "A" even though we're actually fitting to the "B"
    # distribution.
    cf86a04$ipf_a <- IpfConstraint(ipf(constraintList = list(
            IpfConstraint(margin.table2(cf86a04$data$.data,
                                        c('ctcode', 'cfstat'))),
            IpfConstraint(b$ctcode_sexp_agep),
            IpfConstraint(b$ctcode_agep),
            IpfConstraint(b$ctcode_sexp),
            IpfConstraint(margin.table2(cf86a04$data$.data,
                                        c('cfstat'))),
            IpfConstraint(b$sexp_agep),
            IpfConstraint(b$agep),
            IpfConstraint(b$sexp)),
        priorArray = cf86a04$data$.data,
        maxIterations = 100,
        tolerance = 1/100))

    cf86a04
}





#########################################################
# Family BSTs
#########################################################

makeCF86A02 <- function(sc86b01) {
    #
    # CF86A02
    #
    # Universe: census families in private households (100% sample)

    raw <- inputDataBST('census.bst_ct1986_cf86a02_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    cf86a02 <- list(margin = list())
    cf86a02$data <- array(
        data = c(
            raw$r10c1, raw$r10c3, raw$r10c2,
            raw$r1c1,  raw$r1c3,  raw$r1c2,
            raw$r2c1,  raw$r2c3,  raw$r2c2,
            raw$r3c1,  raw$r3c3,  raw$r3c2,
            raw$r4c1,  raw$r4c3,  raw$r4c2,
            raw$r5c1,  raw$r5c3,  raw$r5c2,
            raw$r6c1,  raw$r6c3,  raw$r6c2,
            raw$r7c1,  raw$r7c3,  raw$r7c2,
            raw$r8c1,  raw$r8c3,  raw$r8c2
        ),
        dim = c(numCT, 3, 9),
        dimnames = list(ctcode = raw$ctcode,
                        cfstruc = levels(pum86$f$cfstruc),
                        nuchild = levels(pum86$f$nuchild))
    )
    cf86a02$margin$cfstruc_nuchild <- err4(cf86a02$data[1,,])
    cf86a02$data <- errct(cf86a02$data[-1,,])

    #
    # MARGINS
    #

    cf86a02$margin$ctcode_cfstruc <- array(
        data = c(raw$c1, raw$c3, raw$c2),
        dim = c(numCT, 3),
        dimnames = list(ctcode = raw$ctcode,
                        cfstruc = levels(pum86$f$cfstruc))
    )
    cf86a02$margin$cfstruc <-
        err4(array1(cf86a02$margin$ctcode_cfstruc))
    cf86a02$margin$ctcode_cfstruc <-
        errct(cf86a02$margin$ctcode_cfstruc[-1,])

    cf86a02$margin$ctcode_nuchild <- array(
        data = c(raw$r10, raw$r1, raw$r2, raw$r3, raw$r4, raw$r5, raw$r6,
                 raw$r7,  raw$r8),
        dim = c(numCT, 9),
        dimnames = list(ctcode = raw$ctcode,
                        nuchild = levels(pum86$f$nuchild))
    )
    cf86a02$margin$nuchild <-
        err4(array1(cf86a02$margin$ctcode_nuchild))
    cf86a02$margin$ctcode_nuchild <-
        errct(cf86a02$margin$ctcode_nuchild[-1,])

    cf86a02$margin$ctcode <- array(
        data = c(raw$total[-1]),
        dim = c(numCT - 1),
        dimnames = list(ctcode = raw$ctcode[-1])
    )
    cf86a02$margin$ctcode <- errct(cf86a02$margin$ctcode)
    cf86a02$total <- err4(raw$total[1])

    if(FALSE) {
    cf86a02$ipf <- IpfConstraint(ipf(constraintList = list(
            cf86a02$margin$ctcode_nuchild,
            cf86a02$margin$ctcode,
            cf86a02$margin$nuchild,
            cf86a02$total),
        priorArray = cf86a02$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    cf86a02
}

makeCF86A03 <- function(sc86b01) {
    #
    # CF86A03
    #
    # Universe: census families in private households (100% sample)

    raw <- inputDataBST('census.bst_ct1986_cf86a03_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    cf86a03 <- list(margin = list())

    # Figure out how to split up the households with 18+ children and some
    # children 0-17. Use the PUM ratio.
    # TODO: should no longer be necessary, now that IPF can handle NAs...
    # right?
    pum86f_nz = data.frame(
        childa = factor(pum86$f$childa != '0', labels=c('None', 'Some')),
        childb = factor(pum86$f$childb != '0', labels=c('None', 'Some')),
        childc = factor(pum86$f$childc != '0', labels=c('None', 'Some')),
        childde = factor(pum86$f$childde != '0,0', labels=c('None', 'Some')))
    pum86f_nz_tab <- xtabs(formula = ~childa + childb + childc + childde,
                           data = pum86f_nz)
    r10split <- as.vector(pum86f_nz_tab[,,,'Some'])[-1]
    # For Toronto CMA, evalutes to:
    #   c(19,242,8,371,5,113,4)
    r10split <- prop.table(r10split)

    cf86a03$data <- array(
        data = c(
            # A/B/C/DE = 0/0/0/0
            raw$r12c1, raw$r12c3, raw$r12c2,
            # A/B/C/DE = 1/0/0/0
            raw$r1c1,  raw$r1c3,  raw$r1c2,
            # A/B/C/DE = 0/1/0/0
            raw$r2c1,  raw$r2c3,  raw$r2c2,
            # A/B/C/DE = 1/1/0/0
            raw$r4c1,  raw$r4c3,  raw$r4c2,

            # A/B/C/DE = 0/0/1/0
            raw$r3c1,  raw$r3c3,  raw$r3c2,
            # A/B/C/DE = 1/0/1/0
            raw$r5c1,  raw$r5c3,  raw$r5c2,
            # A/B/C/DE = 0/1/1/0
            raw$r6c1,  raw$r6c3,  raw$r6c2,
            # A/B/C/DE = 1/1/1/0
            raw$r7c1,  raw$r7c3,  raw$r7c2,

            # A/B/C/DE = 0/0/0/1
            raw$r9c1,  raw$r9c3,  raw$r9c2,
            # A/B/C/DE = 1/0/0/1
            c(raw$r10c1, raw$r10c3, raw$r10c2) * r10split[1],
            # A/B/C/DE = 0/1/0/1
            c(raw$r10c1, raw$r10c3, raw$r10c2) * r10split[2],
            # A/B/C/DE = 1/1/0/1
            c(raw$r10c1, raw$r10c3, raw$r10c2) * r10split[3],
            # A/B/C/DE = 0/0/1/1
            c(raw$r10c1, raw$r10c3, raw$r10c2) * r10split[4],
            # A/B/C/DE = 1/0/1/1
            c(raw$r10c1, raw$r10c3, raw$r10c2) * r10split[5],
            # A/B/C/DE = 0/1/1/1
            c(raw$r10c1, raw$r10c3, raw$r10c2) * r10split[6],
            # A/B/C/DE = 1/1/1/1
            c(raw$r10c1, raw$r10c3, raw$r10c2) * r10split[7]
        ),
        dim = c(numCT, 3, 2, 2, 2, 2),
        dimnames = list(ctcode = raw$ctcode,
                        cfstruc = levels(pum86$f$cfstruc),
                        childa = c('None 0-6', 'Some 0-6'),
                        childb = c('None 6-14', 'Some 6-14'),
                        childc = c('None 15-17', 'Some 15-17'),
                        childde = c('None 18+', 'Some 18+'))
    )
    cf86a03$margin$cfstruc_childa_b_c_de <- err4(cf86a03$data[1,,,,,])
    cf86a03$data <- errct(cf86a03$data[-1,,,,,])

    #
    # MARGINS
    #

    cf86a03$margin$ctcode_childa_b_c_de <- array(
        data = c(
            # A/B/C/DE = 0/0/0/0
            raw$r12,
            # A/B/C/DE = 1/0/0/0
            raw$r1,
            # A/B/C/DE = 0/1/0/0
            raw$r2,
            # A/B/C/DE = 1/1/0/0
            raw$r4,

            # A/B/C/DE = 0/0/1/0
            raw$r3,
            # A/B/C/DE = 1/0/1/0
            raw$r5,
            # A/B/C/DE = 0/1/1/0
            raw$r6,
            # A/B/C/DE = 1/1/1/0
            raw$r7,

            # A/B/C/DE = 0/0/0/1
            raw$r9,
            # A/B/C/DE = nz///1
            raw$r10 * r10split[1],
            raw$r10 * r10split[2],
            raw$r10 * r10split[3],
            raw$r10 * r10split[4],
            raw$r10 * r10split[5],
            raw$r10 * r10split[6],
            raw$r10 * r10split[7]
        ),
        dim = c(numCT, 2, 2, 2, 2),
        dimnames = list(ctcode = raw$ctcode,
                        childa = c('None 0-6', 'Some 0-6'),
                        childb = c('None 6-14', 'Some 6-14'),
                        childc = c('None 15-17', 'Some 15-17'),
                        childde = c('None 18+', 'Some 18+'))
    )
    cf86a03$margin$childa_b_c_de <-
        err4(cf86a03$margin$ctcode_childa_b_c_de[1,,,,])
    cf86a03$margin$ctcode_childa_b_c_de <-
        errct(cf86a03$margin$ctcode_childa_b_c_de[-1,,,,])

    cf86a03$margin$ctcode_cfstruc <- array(
        data = c(raw$c1, raw$c3, raw$c2),
        dim = c(numCT, 3),
        dimnames = list(ctcode = raw$ctcode,
                        cfstruc = levels(pum86$f$cfstruc))
    )
    cf86a03$margin$cfstruc <-
        err4(array1(cf86a03$margin$ctcode_cfstruc))
    cf86a03$margin$ctcode_cfstruc <-
        errct(cf86a03$margin$ctcode_cfstruc[-1,])


    cf86a03$margin$ctcode <- array(
        data = c(raw$total[-1]),
        dim = c(numCT - 1),
        dimnames = list(ctcode = raw$ctcode[-1])
    )
    cf86a03$margin$ctcode <- errct(cf86a03$margin$ctcode)
    cf86a03$total <- err4(raw$total[1])

    if(FALSE) {
    cf86a03$ipf <- IpfConstraint(ipf(constraintList = list(
            cf86a03$margin$ctcode_childa_b_c_de,
            cf86a03$margin$ctcode_cfstruc,
            cf86a03$margin$childa_b_c_de,
            cf86a03$margin$cfstruc,
            cf86a03$total),
        priorArray = cf86a03$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }
    cf86a03
}

makeDW86A01 <- function(sc86b01) {
    #
    # DW86A01
    #
    # Universe: occupied private dwellings (i.e., households excluding
    # temporary/collective dwellers etc.)
    #
    # We ignore one dimension from the source table: household maintainer's
    # age.

    raw <- inputDataBST('census.bst_ct1986_dw86a01_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    dw86a01 <- list(margin = list())

    dw86a01$data <- array(
        data = c(
            raw$r5,  raw$r6,  raw$r7,
            raw$r9,  raw$r10, raw$r11,
            raw$r13, raw$r14, raw$r15,
            raw$r17, raw$r18, raw$r19
        ),
        dim = c(numCT, 3, 4),
        dimnames = list(ctcode = raw$ctcode,
                        tenurh = c('Owned', 'Rented', 'On reserve'),
                        dtypeh = c('Single-detached',
                                   'Apartment, 5+ storeys',
                                   'Movable',
                                   'Other'))
    )
    dn <- dimnames(dw86a01$data)
    dw86a01$margin$tenurh_dtypeh <- err4(dw86a01$data[1,,])
    dw86a01$data <- errct(dw86a01$data[-1,,])

    #
    # MARGINS
    #

    dw86a01$margin$ctcode_tenurh <- array(
        data = c(
            raw$r1, raw$r2, raw$r3
        ),
        dim = c(numCT, 3),
        dimnames = list(ctcode = raw$ctcode,
                        tenurh = dn$tenurh)
    )
    dw86a01$margin$tenurh <- err4(array1(dw86a01$margin$ctcode_tenurh))
    dw86a01$margin$ctcode_tenurh <- errct(dw86a01$margin$ctcode_tenurh[-1,])

    dw86a01$margin$ctcode_dtypeh <- array(
        data = c(
            raw$r4, raw$r8, raw$r12, raw$r16
        ),
        dim = c(numCT, 4),
        dimnames = list(ctcode = raw$ctcode,
                        dtypeh = dn$dtypeh)
    )
    dw86a01$margin$dtypeh <- err4(array1(dw86a01$margin$ctcode_dtypeh))
    dw86a01$margin$ctcode_dtypeh <- errct(dw86a01$margin$ctcode_dtypeh[-1,])

    dw86a01$margin$ctcode <- array(raw$total)
    dimnames(dw86a01$margin$ctcode) <- list(ctcode = dn$ctcode)
    dw86a01$total <- err4(dw86a01$margin$ctcode[1])
    dw86a01$margin$ctcode <- errct(dw86a01$margin$ctcode[-1])

    if(FALSE) {
    dw86a01$ipf <- IpfConstraint(ipf(constraintList = list(
            dw86a01$margin$ctcode_tenurh,
            dw86a01$margin$ctcode_dtypeh,
            dw86a01$margin$tenurh_dtypeh,
            dw86a01$margin$ctcode,
            dw86a01$margin$tenurh,
            dw86a01$margin$dtypeh,
            dw86a01$total),
        priorArray = dw86a01$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    dw86a01
}

makeDW86A02 <- function(sc86b01) {
    #
    # DW86A02
    #
    # Universe: occupied private dwellings (i.e., households excluding
    # temporary/collective dwellers etc.)

    raw <- inputDataBST('census.bst_ct1986_dw86a02_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    dw86a02 <- list(margin = list())

    dw86a02$data <- array(
        data = c(
            raw$r1c1, raw$r1c2, raw$r1c3, raw$r1c4, raw$r1c5,
            raw$r1c6, raw$r1c7, raw$r1c8, raw$r1c9, raw$r1c10,
            
            raw$r2c1, raw$r2c2, raw$r2c3, raw$r2c4, raw$r2c5,
            raw$r2c6, raw$r2c7, raw$r2c8, raw$r2c9, raw$r2c10,
            
            raw$r3c1, raw$r3c2, raw$r3c3, raw$r3c4, raw$r3c5,
            raw$r3c6, raw$r3c7, raw$r3c8, raw$r3c9, raw$r3c10,
            
            raw$r4c1, raw$r4c2, raw$r4c3, raw$r4c4, raw$r4c5,
            raw$r4c6, raw$r4c7, raw$r4c8, raw$r4c9, raw$r4c10
        ),
        dim = c(numCT, 10, 4),
        dimnames = list(ctcode = raw$ctcode,
                        hhsize = c('1', '2', '3', '4', '5', '6', '7', '8',
                                   '9', '10+'),
                        dtypeh = c('Single-detached',
                                   'Apartment, 5+ storeys',
                                   'Movable',
                                   'Other'))
    )
    dn <- dimnames(dw86a02$data)
    dw86a02$margin$hhsize_dtypeh <- err4(dw86a02$data[1,,])
    dw86a02$data <- errct(dw86a02$data[-1,,])

    #
    # MARGINS
    #

    dw86a02$margin$ctcode_hhsize <- array(
        data = c(
            raw$c1, raw$c2, raw$c3, raw$c4, raw$c5,
            raw$c6, raw$c7, raw$c8, raw$c9, raw$c10
        ),
        dim = c(numCT, 10),
        dimnames = list(ctcode = raw$ctcode,
                        hhsize = dn$hhsize)
    )
    dw86a02$margin$hhsize <- err4(array1(dw86a02$margin$ctcode_hhsize))
    dw86a02$margin$ctcode_hhsize <- errct(dw86a02$margin$ctcode_hhsize[-1,])

    dw86a02$margin$ctcode_dtypeh <- array(
        data = c(
            raw$r1, raw$r2, raw$r3, raw$r4
        ),
        dim = c(numCT, 4),
        dimnames = list(ctcode = raw$ctcode,
                        dtypeh = dn$dtypeh)
    )
    dw86a02$margin$dtypeh <- err4(array1(dw86a02$margin$ctcode_dtypeh))
    dw86a02$margin$ctcode_dtypeh <- errct(dw86a02$margin$ctcode_dtypeh[-1,])

    dw86a02$margin$ctcode <- array(raw$total)
    dimnames(dw86a02$margin$ctcode) <- list(ctcode = dn$ctcode)
    dw86a02$total <- err4(dw86a02$margin$ctcode[1])
    dw86a02$margin$ctcode <- errct(dw86a02$margin$ctcode[-1])

    if(FALSE) {
    dw86a02$ipf <- IpfConstraint(ipf(constraintList = list(
            dw86a02$margin$ctcode_hhsize,
            dw86a02$margin$ctcode_dtypeh,
            dw86a02$margin$hhsize_dtypeh,
            dw86a02$margin$ctcode,
            dw86a02$margin$hhsize,
            dw86a02$margin$dtypeh,
            dw86a02$total),
        priorArray = dw86a02$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    dw86a02
}

makeDW86B02 <- function(dw86a01, sc86b01) {
    #
    # DW86B02
    #
    # Universe: occupied private dwellings (i.e., households excluding
    # temporary/collective dwellers etc.)

    raw <- inputDataBST('census.bst_ct1986_dw86b02_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    dw86b02 <- list(margin = list())

    dw86b02$data <- array(
        data = c(
            raw$r1c1, raw$r1c2, raw$r1c3, raw$r1c4,
            raw$r2c1, raw$r2c2, raw$r2c3, raw$r2c4,
            raw$r3c1, raw$r3c2, raw$r3c3, raw$r3c4,
            raw$r4c1, raw$r4c2, raw$r4c3, raw$r4c4,
            raw$r5c1, raw$r5c2, raw$r5c3, raw$r5c4,
            raw$r6c1, raw$r6c2, raw$r6c3, raw$r6c4,
            raw$r7c1, raw$r7c2, raw$r7c3, raw$r7c4,
            raw$r8c1, raw$r8c2, raw$r8c3, raw$r8c4
        ),
        dim = c(numCT, 4, 8),
        dimnames = list(ctcode = raw$ctcode,
                        dtypeh = c('Single-detached',
                                   'Apartment, 5+ storeys',
                                   'Movable',
                                   'Other'),
                        builth = c('-1920', '1921-1945', '1946-1960',
                                   '1961-1970', '1971-1975', '1976-1980',
                                   '1981-1985', '1986'))
    )
    dn <- dimnames(dw86b02$data)
    dw86b02$margin$dtypeh_builth <- err4(dw86b02$data[1,,])
    dw86b02$data <- errct(dw86b02$data[-1,,])

    #
    # MARGINS
    #

    dw86b02$margin$ctcode_builth <- array(
        data = c(
            raw$r1, raw$r2, raw$r3, raw$r4,
            raw$r5, raw$r6, raw$r7, raw$r8
        ),
        dim = c(numCT, 8),
        dimnames = list(ctcode = raw$ctcode,
                        builth = dn$builth)
    )
    dw86b02$margin$builth <- err4(array1(dw86b02$margin$ctcode_builth))
    dw86b02$margin$ctcode_builth <- errct(dw86b02$margin$ctcode_builth[-1,])

    dw86b02$margin$ctcode_dtypeh <- array(
        data = c(
            raw$c1, raw$c2, raw$c3, raw$c4
        ),
        dim = c(numCT, 4),
        dimnames = list(ctcode = raw$ctcode,
                        dtypeh = dn$dtypeh)
    )
    dw86b02$margin$dtypeh <- err4(array1(dw86b02$margin$ctcode_dtypeh))
    dw86b02$margin$ctcode_dtypeh <- errct(dw86b02$margin$ctcode_dtypeh[-1,])

    dw86b02$margin$ctcode <- array(raw$total)
    dimnames(dw86b02$margin$ctcode) <- list(ctcode = dn$ctcode)
    dw86b02$total <- err4(dw86b02$margin$ctcode[1])
    dw86b02$margin$ctcode <- errct(dw86b02$margin$ctcode[-1])

    dw86b02$ipf <- IpfConstraint(ipf(constraintList = list(
            dw86b02$margin$ctcode_builth,
            dw86b02$margin$ctcode_dtypeh,
            dw86b02$margin$dtypeh_builth,
            dw86b02$margin$ctcode,
            dw86b02$margin$builth,
            dw86b02$margin$dtypeh,
            dw86b02$total),
        priorArray = dw86b02$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    dw86b02$ipf_a <- IpfConstraint(ipf(
        constraintList = list(
            dw86a01$margin$ctcode_dtypeh,
            dw86a01$margin$dtypeh,
            dw86a01$margin$ctcode,
            dw86a01$total
        ),
        priorArray = dw86b02$ipf$.data,
        maxIterations = 100,
        tolerance = 1/100
    ))
    dw86b02$ipf_a_noctcode <- IpfConstraint(
        margin.table2(dw86b02$ipf_a$.data, c('dtypeh', 'builth'))
    )

    dw86b02
}

makeDW86B04 <- function(dw86a01, sc86b01) {
    #
    # DW86B04
    #
    # Universe: occupied private dwellings (i.e., households excluding
    # temporary/collective dwellers etc.)

    raw <- inputDataBST('census.bst_ct1986_dw86b04_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    dw86b04 <- list(margin = list())

    dw86b04$data <- array(
        data = c(
            raw$r1c1, raw$r1c2, raw$r1c3, raw$r1c4,
            raw$r2c1, raw$r2c2, raw$r2c3, raw$r2c4,
            raw$r3c1, raw$r3c2, raw$r3c3, raw$r3c4,
            raw$r4c1, raw$r4c2, raw$r4c3, raw$r4c4,
            raw$r5c1, raw$r5c2, raw$r5c3, raw$r5c4
        ),
        dim = c(numCT, 4, 5),
        dimnames = list(ctcode = raw$ctcode,
                        dtypeh = c('Single-detached',
                                   'Apartment, 5+ storeys',
                                   'Movable',
                                   'Other'),
                        pperroom = levels(pum86$h$pperroom))
    )
    dn <- dimnames(dw86b04$data)
    dw86b04$margin$dtypeh_pperroom <- err4(dw86b04$data[1,,])
    dw86b04$data <- errct(dw86b04$data[-1,,])

    #
    # MARGINS
    #

    dw86b04$margin$ctcode_pperroom <- array(
        data = c(
            raw$r1, raw$r2, raw$r3, raw$r4, raw$r5
        ),
        dim = c(numCT, 5),
        dimnames = list(ctcode = raw$ctcode,
                        pperroom = dn$pperroom)
    )
    dw86b04$margin$pperroom <- err4(array1(dw86b04$margin$ctcode_pperroom))
    dw86b04$margin$ctcode_pperroom <- errct(dw86b04$margin$ctcode_pperroom[-1,])

    dw86b04$margin$ctcode_dtypeh <- array(
        data = c(
            raw$c1, raw$c2, raw$c3, raw$c4
        ),
        dim = c(numCT, 4),
        dimnames = list(ctcode = raw$ctcode,
                        dtypeh = dn$dtypeh)
    )
    dw86b04$margin$dtypeh <- err4(array1(dw86b04$margin$ctcode_dtypeh))
    dw86b04$margin$ctcode_dtypeh <- errct(dw86b04$margin$ctcode_dtypeh[-1,])

    dw86b04$margin$ctcode <- array(raw$total)
    dimnames(dw86b04$margin$ctcode) <- list(ctcode = dn$ctcode)
    dw86b04$total <- err4(dw86b04$margin$ctcode[1])
    dw86b04$margin$ctcode <- errct(dw86b04$margin$ctcode[-1])

    dw86b04$ipf <- IpfConstraint(ipf(constraintList = list(
            dw86b04$margin$ctcode_pperroom,
            dw86b04$margin$ctcode_dtypeh,
            dw86b04$margin$dtypeh_pperroom,
            dw86b04$margin$ctcode,
            dw86b04$margin$pperroom,
            dw86b04$margin$dtypeh,
            dw86b04$total),
        priorArray = dw86b04$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    dw86b04$ipf_a <- IpfConstraint(ipf(
        constraintList = list(
            dw86a01$margin$ctcode_dtypeh,
            dw86a01$margin$dtypeh,
            dw86a01$margin$ctcode,
            dw86a01$total
        ),
        priorArray = dw86b04$ipf$.data,
        maxIterations = 100,
        tolerance = 1/100
    ))
    dw86b04$ipf_a_noctcode <- IpfConstraint(
        margin.table2(dw86b04$ipf_a$.data, c('dtypeh', 'pperroom'))
    )

    dw86b04
}


makeHH86A01 <- function(sc86b01) {
    #
    # HH86A01
    #
    # Universe: occupied private dwellings (i.e., households excluding
    # temporary/collective dwellers etc.)
    #
    # Although none of the documentation says so explicitly, this is indeed
    # a count of *census* families, not economic families. It matches the
    # profile table census family breakdowns.

    raw <- inputDataBST('census.bst_ct1986_hh86a01_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    hh86a01 <- list(margin = list())

    hh86a01$data <- array(
        data = c(
            raw$r22c1, raw$r22c2,
            raw$r17c1, raw$r17c2,
            raw$r18c1, raw$r18c2
        ),
        dim = c(numCT, 2, 3),
        dimnames = list(ctcode = raw$ctcode,
                        tenurh = c('Owned', 'Rented'),
                        hhnumcf = c('0', '1', '2+'))
    )
    dn <- dimnames(hh86a01$data)
    hh86a01$margin$tenurh_hhnumcf <- err4(hh86a01$data[1,,])
    hh86a01$data <- errct(hh86a01$data[-1,,])

    #
    # MARGINS
    #

    hh86a01$margin$ctcode_hhnumcf <- array(
        data = c(
            raw$r22, raw$r17, raw$r18
        ),
        dim = c(numCT, 3),
        dimnames = list(ctcode = raw$ctcode,
                        hhnumcf = dn$hhnumcf)
    )
    hh86a01$margin$hhnumcf <- err4(array1(hh86a01$margin$ctcode_hhnumcf))
    hh86a01$margin$ctcode_hhnumcf <- errct(hh86a01$margin$ctcode_hhnumcf[-1,])

    hh86a01$margin$ctcode_tenurh <- array(
        data = c(
            raw$c1, raw$c2
        ),
        dim = c(numCT, 2),
        dimnames = list(ctcode = raw$ctcode,
                        tenurh = dn$tenurh)
    )
    hh86a01$margin$tenurh <- err4(array1(hh86a01$margin$ctcode_tenurh))
    hh86a01$margin$ctcode_tenurh <- errct(hh86a01$margin$ctcode_tenurh[-1,])

    hh86a01$margin$ctcode <- array(raw$total)
    dimnames(hh86a01$margin$ctcode) <- list(ctcode = dn$ctcode)
    hh86a01$total <- err4(hh86a01$margin$ctcode[1])
    hh86a01$margin$ctcode <- errct(hh86a01$margin$ctcode[-1])

    if(FALSE) {
    hh86a01$ipf <- IpfConstraint(ipf(constraintList = list(
            hh86a01$margin$ctcode_tenurh,
            hh86a01$margin$ctcode_hhnumcf,
            hh86a01$margin$tenurh_hhnumcf,
            hh86a01$margin$ctcode,
            hh86a01$margin$tenurh,
            hh86a01$margin$hhnumcf,
            hh86a01$total),
        priorArray = hh86a01$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    hh86a01
}

makeHH86A02 <- function(hh86a02, sc86b01) {
    #
    # HH86A02
    #
    # Universe: occupied private dwellings (i.e., households excluding
    # temporary/collective dwellers etc.)
    #
    # Although none of the documentation says so explicitly, this is indeed
    # a count of *census* families, not economic families. It matches the
    # profile table census family breakdowns.

    raw <- inputDataBST('census.bst_ct1986_hh86a02_canada')
    assert(dim(raw)[1] == numCT)
    assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw <- adjustSuppressed(raw, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw)[1:6])

    hh86a02 <- list(margin = list())

    hh86a02$data <- array(
        data = c(
            raw$r11c1, raw$r11c2, raw$r11c3, raw$r11c4, raw$r11c5,
            raw$r11c6, raw$r11c7, raw$r11c8, raw$r11c9, raw$r11c10,
            raw$r7c1,  raw$r7c2,  raw$r7c3,  raw$r7c4,  raw$r7c5,
            raw$r7c6,  raw$r7c7,  raw$r7c8,  raw$r7c9,  raw$r7c10,
            raw$r8c1,  raw$r8c2,  raw$r8c3,  raw$r8c4,  raw$r8c5,
            raw$r8c6,  raw$r8c7,  raw$r8c8,  raw$r8c9,  raw$r8c10
        ),
        dim = c(numCT, 10, 3),
        dimnames = list(ctcode = raw$ctcode,
                        hhsize = c('1', '2', '3', '4', '5', '6', '7', '8',
                                   '9', '10+'),
                        hhnumcf = c('0', '1', '2+'))
    )
    dn <- dimnames(hh86a02$data)
    hh86a02$margin$hhsize_hhnumcf <- err4(hh86a02$data[1,,])
    hh86a02$data <- errct(hh86a02$data[-1,,])

    #
    # MARGINS
    #

    hh86a02$margin$ctcode_hhnumcf <- array(
        data = c(
            raw$r11, raw$r7, raw$r8
        ),
        dim = c(numCT, 3),
        dimnames = list(ctcode = raw$ctcode,
                        hhnumcf = dn$hhnumcf)
    )
    hh86a02$margin$hhnumcf <- err4(array1(hh86a02$margin$ctcode_hhnumcf))
    hh86a02$margin$ctcode_hhnumcf <- errct(hh86a02$margin$ctcode_hhnumcf[-1,])

    hh86a02$margin$ctcode_hhsize <- array(
        data = c(
            raw$c1, raw$c2, raw$c3, raw$c4, raw$c5,
            raw$c6, raw$c7, raw$c8, raw$c9, raw$c10
        ),
        dim = c(numCT, 10),
        dimnames = list(ctcode = raw$ctcode,
                        hhsize = dn$hhsize)
    )
    hh86a02$margin$hhsize <- err4(array1(hh86a02$margin$ctcode_hhsize))
    hh86a02$margin$ctcode_hhsize <- errct(hh86a02$margin$ctcode_hhsize[-1,])

    hh86a02$margin$ctcode <- array(raw$total)
    dimnames(hh86a02$margin$ctcode) <- list(ctcode = dn$ctcode)
    hh86a02$total <- err4(hh86a02$margin$ctcode[1])
    hh86a02$margin$ctcode <- errct(hh86a02$margin$ctcode[-1])

    if(FALSE) {
    hh86a02$ipf <- IpfConstraint(ipf(constraintList = list(
            hh86a02$margin$ctcode_hhsize,
            hh86a02$margin$ctcode_hhnumcf,
            hh86a02$margin$hhsize_hhnumcf,
            hh86a02$margin$ctcode,
            hh86a02$margin$hhsize,
            hh86a02$margin$hhnumcf,
            hh86a02$total),
        priorArray = hh86a02$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    }

    hh86a02
}

makeHH86B01_B02 <- function(hh86a01, hh86a02, sc86b01) {
    #
    # HH86B01 & B02
    #
    # Universe: occupied private owner-occupied dwellings (B01)
    #           occupied private tenant-occupied dwellings (B02)
    #         = occupied private dwellings (non-farm, non-reserve)
    raw_b01 <- inputDataBST('census.bst_ct1986_hh86b01_canada')
    raw_b02 <- inputDataBST('census.bst_ct1986_hh86b02_canada')
    assert(dim(raw_b01)[1] == numCT)
    assert(dim(raw_b02)[1] == numCT)
    assert(raw_b01$ctcode[-1] == names(profile86i$ctcode$.data))
    assert(raw_b02$ctcode[-1] == names(profile86i$ctcode$.data))
    raw_b01 <- adjustSuppressed(raw_b01, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw_b01)[1:6])
    raw_b02 <- adjustSuppressed(raw_b02, profile86i$suppressed,
            # TODO: what is correct scale for this universe?
            # Using same formula as LF86B01 for now.
            scale = profile86i$ctcode_adultnoninst$.data / sc86b01$total$.data,
            ignoreFields = names(raw_b02)[1:6])

    hh86b01_b02 <- list(margin = list())

    hh86b01_b02$data <- array(
        data = c(
            raw_b01$r9c1, raw_b01$r9c2, raw_b01$r9c3, raw_b01$r9c4,raw_b01$r9c5,
            raw_b01$r1c1, raw_b01$r1c2, raw_b01$r1c3, raw_b01$r1c4,raw_b01$r1c5,
            raw_b01$r8c1, raw_b01$r8c2, raw_b01$r8c3, raw_b01$r8c4,raw_b01$r8c5,
            raw_b02$r9c1, raw_b02$r9c2, raw_b02$r9c3, raw_b02$r9c4,raw_b02$r9c5,
            raw_b02$r1c1, raw_b02$r1c2, raw_b02$r1c3, raw_b02$r1c4,raw_b02$r1c5,
            raw_b02$r8c1, raw_b02$r8c2, raw_b02$r8c3, raw_b02$r8c4,raw_b02$r8c5
        ),
        dim = c(numCT, 5, 3, 2),
        dimnames = list(ctcode = raw_b01$ctcode,
                        payh = c('0-199', '200-399', '400-699', '700-999',
                                 '1000+'),
                        hhnumcf = c('0', '1', '2+'),
                        tenurh = c('Owned', 'Rented'))
    )
    dn <- dimnames(hh86b01_b02$data)
    hh86b01_b02$margin$payh_hhnumcf_tenurh <- err4(hh86b01_b02$data[1,,,])
    hh86b01_b02$data <- errct(hh86b01_b02$data[-1,,,])

    #
    # MARGINS
    #

    hh86b01_b02$margin$ctcode_hhnumcf_tenurh <- array(
        data = c(
            raw_b01$r9, raw_b01$r1, raw_b01$r8,
            raw_b02$r9, raw_b02$r1, raw_b02$r8
        ),
        dim = c(numCT, 3, 2),
        dimnames = list(ctcode = raw_b01$ctcode,
                        hhnumcf = dn$hhnumcf,
                        tenurh = dn$tenurh)
    )
    hh86b01_b02$margin$hhnumcf_tenurh <-
        err4(hh86b01_b02$margin$ctcode_hhnumcf_tenurh[1,,])
    hh86b01_b02$margin$ctcode_hhnumcf_tenurh <-
        errct(hh86b01_b02$margin$ctcode_hhnumcf_tenurh[-1,,])

    hh86b01_b02$margin$ctcode_payh_tenurh <- array(
        data = c(
            raw_b01$c1, raw_b01$c2, raw_b01$c3, raw_b01$c4, raw_b01$c5,
            raw_b02$c1, raw_b02$c2, raw_b02$c3, raw_b02$c4, raw_b02$c5
        ),
        dim = c(numCT, 5, 2),
        dimnames = list(ctcode = raw_b01$ctcode,
                        payh = dn$payh,
                        tenurh = dn$tenurh)
    )
    hh86b01_b02$margin$payh_tenurh <-
        err4(hh86b01_b02$margin$ctcode_payh_tenurh[1,,])
    hh86b01_b02$margin$ctcode_payh_tenurh <-
        errct(hh86b01_b02$margin$ctcode_payh_tenurh[-1,,])

    hh86b01_b02$margin$ctcode_tenurh <- array(
        data = c(
            raw_b01$total,
            raw_b02$total
        ),
        dim = c(numCT, 2),
        dimnames = list(ctcode = raw_b01$ctcode,
                        tenurh = dn$tenurh))
    hh86b01_b02$margin$tenurh <-
        err4(array1(hh86b01_b02$margin$ctcode_tenurh))
    hh86b01_b02$margin$ctcode_tenurh <-
        errct(hh86b01_b02$margin$ctcode_tenurh[-1,])

    hh86b01_b02$ipf <- IpfConstraint(ipf(constraintList = list(
            hh86b01_b02$margin$ctcode_payh_tenurh,
            hh86b01_b02$margin$ctcode_hhnumcf_tenurh,
            hh86b01_b02$margin$payh_hhnumcf_tenurh,
            hh86b01_b02$margin$ctcode_tenurh,
            hh86b01_b02$margin$payh_tenurh,
            hh86b01_b02$margin$hhnumcf_tenurh,
            hh86b01_b02$margin$tenurh),
        priorArray = hh86b01_b02$data$.data,
        maxIterations = 100,
        tolerance = 1/100))
    hh86b01_b02$ipf_a <- IpfConstraint(ipf(
        constraintList = list(
            hh86a01$margin$ctcode_tenurh,
            hh86a02$margin$ctcode_hhnumcf,
            hh86a01$margin$tenurh,
            hh86a02$margin$hhnumcf,
            hh86a02$margin$ctcode,
            hh86a02$total
        ),
        priorArray = hh86b01_b02$ipf$.data,
        maxIterations = 100,
        tolerance = 1/100
    ))
    hh86b01_b02$ipf_a_noctcode <- IpfConstraint(
        margin.table2(hh86b01_b02$ipf_a$.data, c('payh', 'hhnumcf', 'tenurh'))
    )

    hh86b01_b02
}

numCT <- length(profile86i$ctcode$.data) + 1
channel <- odbcConnect("PostgreSQL census", uid="david", case="tolower")

bst <- list()
# Individual BSTs
#print('SC86B01')
bst$sc86b01 <- makeSC86B01()
#print('Fake')
bst$fake <- makeFake(bst$sc86b01)
#print('LF86B01')
bst$lf86b01 <- makeLF86B01(bst$sc86b01)
#print('LF86B03')
bst$lf86b03 <- makeLF86B03(bst$sc86b01)
#print('LF86B04')
bst$lf86b04 <- makeLF86B04(bst$sc86b01)
#print('CF86A04')
bst$cf86a04 <- makeCF86A04(bst$sc86b01)
# Family BSTs
#print('CF86A02')
bst$cf86a02 <- makeCF86A02(bst$sc86b01)
#print('CF86A03')
bst$cf86a03 <- makeCF86A03(bst$sc86b01)

#print('DW86A01')
# TODO: 100+ iterations... why?
bst$dw86a01 <- makeDW86A01(bst$sc86b01)
#print('DW86A02')
# TODO: 100+ iterations... why?
bst$dw86a02 <- makeDW86A02(bst$sc86b01)
#print('DW86B02')
bst$dw86b02 <- makeDW86B02(bst$dw86a01, bst$sc86b01)
# TODO: 100+ iterations... why?
#print('DW86B04')
bst$dw86b04 <- makeDW86B04(bst$dw86a01, bst$sc86b01)

#print('HH86A01')
bst$hh86a01 <- makeHH86A01(bst$sc86b01)
#print('HH86A02')
bst$hh86a02 <- makeHH86A02(bst$hh86a02, bst$sc86b01)
#print('HH86B01_B02')
# TODO: 100+ iterations... why?
bst$hh86b01_b02 <- makeHH86B01_B02(bst$hh86a01, bst$hh86a02, bst$sc86b01)

odbcClose(channel)

bst
}
