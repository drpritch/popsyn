#
# A quick hack to assemble the log-linear models of the SC86B01 table for
# the Data chapter of my thesis.
#
#
# Pulled together from synthesize.R, mostly.



library('RODBC')
library('lattice')
library('vcd')
library('gtools')
library('gdata')

source('ipf.R')
source('ipf_list.R')
source('utils.R')

# HSK = Hamilton/St. Catharines/Niagara, which are grouped together.
cmaname <- 'Toronto'
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

# A simplified version of this routine, but operating on either CSDs and CDs,
# both from the CSD tabulation.
makeSC86B01 <- function(geo = 'csdname') {
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
    raw <- sqlQuery(channel, paste(
        'SELECT *,rtrim(geoname) as csdname FROM census.bst_csd1986_sc86b01_canada WHERE ',
        makeOrQuery(field='cmaca', cmacodes),
        ' AND (csd>0 OR pcmaca=0)
         ORDER BY cd,csd,cmaca', sep=''))

    # CSD hacks
    #assert(raw$ctcode[-1] == names(profile86i$ctcode$.data))
    raw$geo <- raw[,geo]

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
        dim = c(nrow(raw), 6, 2, 6),
        dimnames = list(geo = raw$geo,
             hlosp = c('Less than grade 9', 'Grades 9-13', 'High school',
                       'Trades and non-uni', 'University w/o degree',
                       'University w/ degree'),
             sexp = levels(pum86$i$sexp),
             agep = c('15-24', levels(pum86$i$agep)[-1:-3]))
    )
    dn <- dimnames(sc86b01$data)
    sc86b01$margin$hlosp_sexp_agep <- sc86b01$data[1,,,]
    sc86b01$data <- sc86b01$data[-1,,,]
    names(dimnames(sc86b01$data))[1] <- geo

    if(geo == 'cd') {
        # Group by CDs
        cdnames <- sort(unique(raw$cd[-1]))
        sc86b01$data <- collapse.array(sc86b01$data,
            LevelMap(factor = 'cd', map = match(raw$cd[-1], cdnames),
                levels = seq(cdnames), labels = cdnames))
    }
    sc86b01
}
channel <- odbcConnect("PostgreSQL census", uid="david", case="tolower")
bst <- list()
bst$sc86b01_csd <- makeSC86B01('csdname')
bst$sc86b01_cd <- makeSC86B01('cd')
foo <- abind(bst$sc86b01_cd$data[-3,,,],
             bst$sc86b01_csd$data[c('SCARBOROUGH','TORONTO','EAST YORK',
                           'NORTH YORK', 'YORK', 'ETOBICOKE'),,,],
             along=1)
names(dimnames(foo)) <- names(dimnames(bst$sc86b01_cd$data))
bar <- xtabs(~hlosp + sexp + agep, pum86$i)
# Do we need to divide bar by dim(foo)[1] to account for spread across 10
# CDs? I don't *think* so... but I'm not sure.
bar <- array(rep(bar, each = dim(foo)[1]),
             dim = c(dim(foo)[1], dim(bar)),
             dimnames = c(list(cd = dimnames(foo)$cd), dimnames(bar)))
bar <- collapse.array(bar,
    LevelMap(factor = 'hlosp',
             map = c(1:4,4,4,5,5,6),
             levels = 1:6,
             labels = dimnames(bst$sc86b01_cd$data)$hlosp))
bar <- collapse.array(bar,
     LevelMap(factor = 'agep',
              map = c(1, 1, 1:6),
              levels = 1:6,
              labels = dimnames(bst$sc86b01_cd$data)$agep))

# w/o PUMS
a <- glm(formula = Freq / 5 ~ sexp + agep + hlosp + cd
                    + sexp*agep + sexp*hlosp + agep*hlosp
                    + sexp*agep*hlosp
                    + sexp*cd + agep*cd + hlosp*cd
                    + sexp*agep*cd + sexp*hlosp*cd + agep*hlosp*cd,
              family = poisson,
              data = as.data.frame.table(foo))

a0 <- glm(formula = Freq / 5 ~ sexp + agep + hlosp
                    + sexp*agep + sexp*hlosp + agep*hlosp
                    + sexp*agep*hlosp,
              family = poisson,
              data = as.data.frame.table(foo))

a1 <- glm(formula = Freq / 5 ~ sexp + agep + hlosp + cd
                    + sexp*agep + sexp*hlosp + agep*hlosp
                    + sexp*agep*hlosp
                    + sexp*cd + agep*cd + hlosp*cd
                    + sexp*agep*cd + sexp*hlosp*cd + agep*hlosp*cd
                    + sexp*agep*hlosp*cd,
              family = poisson,
              data = as.data.frame.table(foo))

# w/ PUMS
assert(dim(foo) == dim(bar))
frame <- as.data.frame.table(foo)
frame$Freq <- frame$Freq / 5
frame$pumFreq <- as.data.frame.table(bar)$Freq
b <- glm(formula = Freq ~ sexp + agep + hlosp + cd
                    + sexp*agep + sexp*hlosp + agep*hlosp
                    + sexp*agep*hlosp
                    + sexp*cd + agep*cd + hlosp*cd
                    + sexp*agep*cd + sexp*hlosp*cd + agep*hlosp*cd
                    + sexp*agep*hlosp*cd,
              family = poisson,
              data = frame,
              offset = log(pumFreq))

dimnames(foo)$hlosp <- c('< Gr. 9', 'Gr. 9-13', 'High school',
    'Trades & non-uni.', 'Uni. w/o deg.', 'Uni. w/ deg.')
postscript(file='../figure_sc86b01_mosaic.eps',
    #family='ComputerModern',
    family='Times',
    horizontal = FALSE, pointsize = 10, width=5, height=5)
mosaic(formula = ~sexp + agep + hlosp, data=foo,
    labeling_args = list(
        gp_labels = gpar(fontsize = 10),
        set_varnames = list(sexp = 'Gender', agep = 'Age',
                            hlosp = 'Highest Level of Schooling'),
        rot_labels=c(0, 0, 0, 90),
        just_labels=c('center', 'left', 'center', 'center'),
        offset_labels=c(0, -0.5, 0, 0),
        offset_varnames=c(0, 4.5, 0, 0)),
    highlighting = 3,
    margins=c(2.5, 7.5, 2.5, 2.5))
dev.off()

print(anova(a, test='Chisq'))
print(anova(b, test='Chisq'))
stop()

#                 Df Deviance Resid. Df Resid. Dev  P(>|Chi|)
#NULL                               863     628862
#sexp              1      568       862     628294 1.362e-125
#agep              5    40637       857     587657          0
#hlosp             5    63315       852     524342          0
#cd               11   381164       841     143178          0
#sexp:agep         5     1537       836     141641          0
#sexp:hlosp        5     4338       831     137303          0
#agep:hlosp       25   102424       806      34878          0
#sexp:cd          11      207       795      34671  2.218e-38
#agep:cd          55    11130       740      23541          0
#hlosp:cd         55    15950       685       7591          0
#sexp:agep:hlosp  25     3520       660       4071          0
#sexp:agep:cd     55      304       605       3767  3.963e-36
#sexp:hlosp:cd    55      733       550       3034 2.724e-119
#agep:hlosp:cd   275     2573       275        461          0

stop()
