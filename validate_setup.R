#
# Set up for experiments and validation of results.
#

load('../results/latest/prefitGeog-Toronto.Rdata')
source('ipf_list.R')

# TODO: include straight "ctcode"
getBSTCtcodeMargins <- function(bstList) {
    c(lapply(bstList, function(x) { x$data }),
      unlist(lapply(bstList, function(bst) {
        bst$margin[grep('ctcode_', names(bst$margin))]
      }), recursive = FALSE))
}

getBSTNoCtcodeMargins <- function(bstList) {
    c(unlist(lapply(bstList, function(bst) {
        bst$margin[seq(bst$margin)[-grep('ctcode', names(bst$margin))]]
      }), recursive = FALSE))
}

bstList <- list(bst$sc86b01, bst$lf86b01, bst$lf86b03, bst$lf86b04)
margin.bst <- function(x, dn) {
    result <- x
    # ... and replace the data with just one margin.
    result$.data <- margin.table2(result$.data, dn)
    result$.min <- NULL
    result$.max <- NULL
    result
}

taeConstraintList <- c(
    list(pt$sexinc_2b$data),
    list(bst$fake$data),
    list(bst$cf86a04$ipf_a,
         margin.bst(bst$cf86a04$ipf_a, c('ctcode', 'cfstat')),
         margin.bst(bst$cf86a04$ipf_a, c('ctcode', 'cfstat', 'agep')),
         margin.bst(bst$cf86a04$ipf_a, c('ctcode', 'cfstat', 'sexp'))),
    # For each of the BSTs, use both the full table and all margins
    # involving ctcode.
    getBSTCtcodeMargins(bstList),

    list(pt$sexinc_2b$margin$sexp_totincp,
         bst$sc86b01$margin$ctcode),
    list(margin.bst(bst$cf86a04$ipf_a, c('cfstat')),
         margin.bst(bst$cf86a04$ipf_a, c('cfstat', 'agep')),
         margin.bst(bst$cf86a04$ipf_a, c('cfstat', 'sexp')),
         margin.bst(bst$cf86a04$ipf_a, c('cfstat', 'sexp', 'agep'))),
    getBSTNoCtcodeMargins(bstList),

    # Throw in a bunch of fit2A tables that aren't covered anywhere else
    list(
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('agep', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('cfstat', 'hlosp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('cfstat', 'occ81p'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('cfstat', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('hlosp', 'occ81p'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('hlosp', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('occ81p', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('agep', 'cfstat', 'hlosp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('agep', 'cfstat', 'occ81p'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('agep', 'cfstat', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('agep', 'hlosp', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('cfstat', 'hlosp', 'occ81p'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('cfstat', 'hlosp', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('cfstat', 'occ81p', 'totincp'))),
        IpfConstraint(margin.list(fit2A$i$fitted.values,
            c('hlosp', 'occ81p', 'totincp')))
        # ... and more 3-ways involving sex, lfstat, etc. ...
    )
)

#
# Take a list of experiments and arrange them into a data frame, sorted by
# type etc.
#

sortExper <- function(exper, statistic='srmse', scale = 1000) {
    result <- lapply(exper, function(x) { x$errorstats_eval[,statistic] })
    result <- round(data.frame(result) * scale, 0)
    colnames(result) <- names(exper)
    rownames(result) <- rownames(exper[[1]]$errorstats_eval)

    # Add fields about the type of each row
    result$hasct <- FALSE
    result$hasct[grep('^ctcode:',rownames(result))] <- TRUE
    result$oned <- TRUE
    result$oned[intersect(grep(':',rownames(result)),
                          which(!result$hasct))] <- FALSE
    result$oned[intersect(grep('^ctcode:.*:',rownames(result)),
                          which(result$hasct))] <- FALSE
    result$type <- factor(1 + result$hasct*1 + (!result$oned)*2,
        levels=1:5,
        labels=c('1D', '1D x ct', '2+D', '2+D x ct', '2+D PUMS only'))
    result$type[grep('^agep:totincp', rownames(result)):nrow(result)] <-
        '2+D PUMS only'
    result$hasct <- NULL
    result$oned <- NULL
    rownames(result) <- unlist(lapply(rownames(result), function(rowname) {
        split1 <- strsplit(rowname, ' ')[[1]]
        split2 <- strsplit(split1[1], ':')[[1]]
        if('ctcode' %in% split2) {
            split2 <- c('ctcode', sort(split2[split2 != 'ctcode']))
        } else { 
            split2 <- sort(split2)
        }
        split1[1] <- paste(split2, collapse=':')
        paste(split1, collapse=' ')
    }))

    # Sort: by type, then by 2D/3D (for pums only), then by name

    result$three <- FALSE
    result$three[result$type=='2+D PUMS only'][grep(':.*:', rownames(result)[result$type=='2+D PUMS only'])] <- TRUE
    result <- result[order(result$type, result$three, rownames(result)),]
    result$three <- NULL
    
    result$numCells <- as.integer(gsub(' \\)', '', gsub('.* \\( ', '', rownames(result))))

    result
}

sortExperI <- function(exper, statistic='srmse', scale = 1000) {
    result <- sortExper(exper, statistic, scale)
    # Kill off duplicated variables
    result <- result[-match(c('hlosp ( 7 )',
                              'ctcode:hlosp ( 5117 )',
                              'hlosp:sexp ( 14 )',
                              'ctcode:agep:sexp ( 8772 )',
                              'ctcode:hlosp:sexp ( 10234 )'),
                            rownames(result)),]
    rownames(result) <- gsub(' \\( .* \\)', '', rownames(result))
    result
}
