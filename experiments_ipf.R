#
# Do the "IPF" series of experiments from the thesis.
#   C1 here = I1 in thesis, etc.
# 
# Should be run after synthesize2.R

source('validate_setup.R')
exper <- list()



# Trial: C1
temp <- ipf_list(
    constraintList = list(
        bst$sc86b01$margin$ctcode_sexp,
        pt$sexinc_2b$margin$sexp_totincp,
        margin.bst(bst$cf86a04$ipf_a, 'cfstat'),
        bst$sc86b01$margin$sexp,
        bst$sc86b01$margin$agep,
        bst$sc86b01$margin$hlosp,
        bst$lf86b01$margin$lfact,
        bst$lf86b04$margin$occ81p
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'none',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)

# Replace TAE with evaluation against desired margins.
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c1 = temp))

# Trial: C2
temp <- ipf_list(
    constraintList = list(
        bst$sc86b01$margin$ctcode_sexp,
        pt$sexinc_2b$margin$sexp_totincp,
        margin.bst(bst$cf86a04$ipf_a, 'cfstat'),
        bst$sc86b01$margin$sexp,
        bst$sc86b01$margin$agep,
        bst$sc86b01$margin$hlosp,
        bst$lf86b01$margin$lfact,
        bst$lf86b04$margin$occ81p
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'frompop',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)

# Replace TAE with evaluation against desired margins.
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c2 = temp))



# Trial: C3
temp <- ipf_list(
    constraintList = c(
        list(bst$sc86b01$margin$ctcode_sexp,
             pt$sexinc_2b$margin$sexp_totincp,
             margin.bst(bst$cf86a04$ipf_a, c('cfstat', 'agep', 'sexp'))),
        getBSTNoCtcodeMargins(bstList)
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'none',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)

# Replace TAE with evaluation against desired margins.
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c3 = temp))

# Trial: C4
temp <- ipf_list(
    constraintList = c(
        list(bst$sc86b01$margin$ctcode_sexp,
             pt$sexinc_2b$margin$sexp_totincp,
             margin.bst(bst$cf86a04$ipf_a, c('cfstat', 'agep', 'sexp'))),
        getBSTNoCtcodeMargins(bstList)
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'frompop',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)

# Replace TAE with evaluation against desired margins.
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c4 = temp))




# Trial: C5
temp <- ipf_list(
    constraintList = list(
        #IpfConstraint(margin.table2(pt$sexinc_2b$data$.data, c('totincp'))),
        pt$sexinc_2b$data,
        margin.bst(bst$cf86a04$ipf_a, c('ctcode', 'cfstat')),
        bst$sc86b01$margin$ctcode_sexp,
        bst$sc86b01$margin$ctcode_agep,
        bst$sc86b01$margin$ctcode_hlosp,
        bst$lf86b01$margin$ctcode_lfact,
        bst$lf86b04$margin$ctcode_occ81p
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'none',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c5 = temp))

# Trial: C6
temp <- ipf_list(
    constraintList = list(
        #IpfConstraint(margin.table2(pt$sexinc_2b$data$.data, c('totincp'))),
        pt$sexinc_2b$data,
        margin.bst(bst$cf86a04$ipf_a, c('ctcode', 'cfstat')),
        bst$sc86b01$margin$ctcode_sexp,
        bst$sc86b01$margin$ctcode_agep,
        bst$sc86b01$margin$ctcode_hlosp,
        bst$lf86b01$margin$ctcode_lfact,
        bst$lf86b04$margin$ctcode_occ81p
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'frompop',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c6 = temp))




# Trial: C7
temp <- ipf_list(
    constraintList = c(
        #IpfConstraint(margin.list(fit2A$i$fitted.values, 'id')))
        list(pt$sexinc_2b$data,
            bst$cf86a04$ipf_a),
        # For each of the BSTs, use both the full table and all margins
        # involving ctcode.
        getBSTCtcodeMargins(bstList),
        list(bst$fake$data)
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'none',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c7 = temp))

# Trial: C8
temp <- ipf_list(
    constraintList = c(
        #IpfConstraint(margin.list(fit2A$i$fitted.values, 'id')))
        list(pt$sexinc_2b$data,
            bst$cf86a04$ipf_a),
        # For each of the BSTs, use both the full table and all margins
        # involving ctcode.
        getBSTCtcodeMargins(bstList),
        list(bst$fake$data)
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'frompop',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c8 = temp))



# Trial: C9
temp <- ipf_list(
    constraintList = list(
        #IpfConstraint(margin.table2(pt$sexinc_2b$data$.data, c('totincp'))),
        pt$sexinc_2b$data,
        margin.bst(bst$cf86a04$ipf_a, c('ctcode', 'cfstat')),
        bst$sc86b01$margin$ctcode_sexp,
        bst$sc86b01$margin$ctcode_agep,
        bst$sc86b01$margin$ctcode_hlosp,
        bst$lf86b01$margin$ctcode_lfact,
        bst$lf86b04$margin$ctcode_occ81p,
        IpfConstraint(margin.list(fit2A$i1$fitted.values, 'id'))
     ),
     popFrame = fit2A$i1$fitted.values,
     priorAssoc = 'none',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c9 = temp))

# Trial: C10
temp <- ipf_list(
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
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(c10 = temp))


# Trial: R2 (=C8 w/o hierarchy)
temp <- ipf_list(
    constraintList = c(
        #IpfConstraint(margin.list(fit2A$i$fitted.values, 'id')))
        list(pt$sexinc_2b$data,
            bst$cf86a04$ipf_a),
        # For each of the BSTs, use *just* the full table
        lapply(bstList, function(x) { x$data }),
        list(bst$fake$data)
     ),
     popFrame = pum86$i[,c('id', 'cfstat', 'hlosp', 'sexp', 'agep', 'occ81p',
                           'lfact', 'totincp')],
     priorAssoc = 'frompop',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(r2 = temp))

# Trial: R3 (=C10 w/o hierarchy)
temp <- ipf_list(
    constraintList = c(
        list(pt$sexinc_2b$data,
            bst$cf86a04$ipf_a),
        # For each of the BSTs, use *just* the full table
        lapply(bstList, function(x) { x$data }),
        list(bst$fake$data,
             IpfConstraint(margin.list(fit2A$i_nh$fitted.values, 'id')))
     ),
     popFrame = fit2A$i_nh$fitted.values,
     priorAssoc = 'none',
     maxIterations = 100,
     tolerance = 1/100,
     statistics = TRUE)
temp$errorstats_eval <-
    ipf_errorstats_list(temp$fitted.values, taeConstraintList)
temp$fitted.values <- NULL
gc()
exper <- c(exper, list(r3 = temp))

result <- sortExperI(exper, statistic='srmse', scale=1000)

#result <- sortExperI(exper, statistic='mdi', scale=1/50)
#result[result$type == '2+D PUMS only',] <-
#    result[result$type == '2+D PUMS only',] / 10

#print(aggregate(result, by=list(foo$type), FUN=mean))

save(exper, file='../results/latest/exper_ipf.Rdata')
write.csv(result, file='../results/latest/exper_ipf.csv')
