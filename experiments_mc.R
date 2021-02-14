#
# Do the "Monte Carlo" series of experiments from the thesis.
# 
# Should be run after experiments_mc_prep.sh, which will synthesize 30
# populations using synthesize2.R
#

# Create the validation table list.
source('validate_setup.R')

# but also kill off some data that's not needed here.
remove(bst)
remove(fit2A)
remove(pt)
remove(profile86i)
remove(pum86)
gc()

set.seed(0)
popseeds <- round(runif(30)*1000)

load('../results/latest/fitGeog-Toronto.Rdata')
fitGeog$d <- NULL
fitGeog$f <- NULL
gc()

print('Evaluating full results (Monte Carlo conditional)')
exper <- list()
for(idx in seq(popseeds)) {
    print(paste('Seed: ', popseeds[idx]))

    pop <- list()
    pop$d <- read.csv(paste('../results/latest/pop_d-Toronto-s',
                            formatC(popseeds[idx], width=3, flag='0'),
                            '.csv', sep=''),
                      na.strings=c('-1'))
    pop$f <- read.csv(paste('../results/latest/pop_f-Toronto-s',
                            formatC(popseeds[idx], width=3, flag='0'),
                            '.csv', sep=''),
                      na.strings=c('-1'))
    pop$i <- read.csv(paste('../results/latest/pop_i-Toronto-s',
                            formatC(popseeds[idx], width=3, flag='0'),
                            '.csv', sep=''),
                      na.strings=c('-1'))
    pop$i_collective <-
             read.csv(paste('../results/latest/pop_ic-Toronto-s',
                            formatC(popseeds[idx], width=3, flag='0'),
                            '.csv', sep=''),
                      na.strings=c('-1'))
    pop$i$householdId[!is.na(pop$i$familyId)] <-
        pop$f$householdId[match(pop$i[!is.na(pop$i$familyId), 'familyId'],
                                pop$f$id)]
    pop$i$ctcode <- pop$d$ctcode[match(pop$i$householdId, pop$d$id)]
    pop$i <- pop$i[,c('id','pumiId','ctcode')]
    pop$i <- rbind(pop$i, pop$i_collective)

    iweight <- xtabs(~pumiId + ctcode, pop$i)
    remove(pop)
    temp <- list()
    temp$rowMap <- match(levels(fitGeog$i$fitted.values$pumiId)[
                            fitGeog$i$fitted.values$pumiId],
                         rownames(iweight))
    temp$colMap <- match(colnames(fitGeog$i$fitted.values$weight),
                         colnames(iweight))
    temp$dimNames <- dimnames(fitGeog$i$fitted.values$weight)
    # Add dummy row/col of zeroes
    iweight <- rbind(iweight, 0)
    iweight <- cbind(iweight, 0)
    gc()
    # Set NAs to point to the zero row/col.
    temp$rowMap[is.na(temp$rowMap)] <- nrow(iweight)
    temp$colMap[is.na(temp$colMap)] <- ncol(iweight)
    # Lookup appropriate rows/cols from result, set them in weight matrix.
    fitGeog$i$fitted.values$weight <- iweight[temp$rowMap, temp$colMap]
    remove(iweight)
    gc()
    dimnames(fitGeog$i$fitted.values$weight) <- temp$dimNames
    remove(temp)
    gc()
    temp <- list(
        list(errorstats_eval =
            ipf_errorstats_list(fitGeog$i$fitted.values, taeConstraintList))
    )
    names(temp) <- popseeds[idx]

    exper <- c(exper, temp)
    save(exper, file='../results/latest/exper_mccond.Rdata')
    gc()
}

save(exper, file='../results/latest/exper_mccond.Rdata')
remove(fitGeog)
gc()



print('Evaluating Monte Carlo only')

exper <- list()
for(idx in seq(popseeds)) {
    print(paste('Seed: ', popseeds[idx]))
    load('../results/latest/fitGeog-Toronto.Rdata')
    fitGeog$d <- NULL
    fitGeog$f <- NULL
    gc()
    #
    # Create an individual table *without* families/dwellings. This is to test
    # the effect of the multi-agent relationship stage of synthesis.
    #
    # This is intended to be done after the intro to synthesize2.R
    #
    # Add id dimension to table, for montecarlo
    names(dimnames(fitGeog$i$fitted.values$weight))[[1]] <- 'id'
    dimnames(fitGeog$i$fitted.values$weight)$id <- fitGeog$i$fitted.values$id
    gc()
    pop <- list()
    set.seed(popseeds[idx])
    pop$i <- montecarlo(fitGeog$i$fitted.values$weight)
    gc()
    # Copy all columns over to pop - including id.
    pop$i <- cbind(pop$i,
        pumiId = fitGeog$i$fitted.values[match(pop$i$id,fitGeog$i$fitted.values$id),
                                         'pumiId'])
    # Now get rid of the useless id. (pumhId and hhnucf contain all the
    # necessary information.)
    pop$i$id <- NULL
    # Kill off id-based rownames.
    rownames(pop$i) <- NULL
    # And add a real id.
    pop$i$id <- seq(nrow(pop$i))

    # Now repeat a bunch of the last section:
    iweight <- xtabs(~pumiId + ctcode, pop$i)
    remove(pop)
    gc()
    temp <- list()
    temp$rowMap <- match(levels(fitGeog$i$fitted.values$pumiId)[
                            fitGeog$i$fitted.values$pumiId],
                         rownames(iweight))
    temp$colMap <- match(colnames(fitGeog$i$fitted.values$weight),
                         colnames(iweight))
    temp$dimNames <- dimnames(fitGeog$i$fitted.values$weight)
    # Add dummy row/col of zeroes
    iweight <- rbind(iweight, 0)
    iweight <- cbind(iweight, 0)
    gc()
    # Set NAs to point to the zero row/col.
    temp$rowMap[is.na(temp$rowMap)] <- nrow(iweight)
    temp$colMap[is.na(temp$colMap)] <- ncol(iweight)
    # Lookup appropriate rows/cols from result, set them in weight matrix.
    fitGeog$i$fitted.values$weight <- iweight[temp$rowMap, temp$colMap]
    remove(iweight)
    gc()
    dimnames(fitGeog$i$fitted.values$weight) <- temp$dimNames
    remove(temp)
    gc()
    temp <- list(list(errorstats_eval =
        ipf_errorstats_list(fitGeog$i$fitted.values, taeConstraintList)))
    names(temp) <- popseeds[idx]
    fitGeog$i$fitted.values <- NULL

    exper <- c(exper, temp)
    remove(fitGeog)
    gc()
    save(exper, file='../results/latest/exper_mc.Rdata')
}

save(exper, file='../results/latest/exper_mc.Rdata')
#result <- sortExper(exper)
#write.csv(result, file='../results/latest/exper_mc.csv')







if(FALSE) {
source('pum86.R')
# Pull in all of the attribute values.
pop$d <- cbind(
    pop$d, pum86$h[match(pop$d$pumhId, pum86$h$id),
        c('dtypeh', 'builth', 'room', 'hhsize')])
# Find number of families per dwelling.
pop$f_d <- xtabs(~householdId, pop$f)
pop$d$hhnumcf <- pop$f_d[match(pop$d$id, rownames(pop$f_d))]
pop$d$hhnumcf[is.na(pop$d$hhnumcf)] <- 0
pop$d$hhnumcf <- factor(pop$d$hhnumcf)
pop$f_d <- NULL
}
