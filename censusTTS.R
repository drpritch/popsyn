# Build a table providing a mapping between Ctcode (1986 census tract id)
# and Tts96 (1996 TTS zone id). The result is a list with three columns:
# 1) ctcode
# 2) tts96
# 3) weight
# Each row represents a pair of zones that overlap, and the ``weight''
# (sort of) represents the joint probability that a household resides in
# that pair of zones. In practice, it is actually the *overlapping area*
# of the two zones, with some corrections for TTS zones with zero
# residential development. Ideally, it would be a true probability
# distribution; but that hasn't been done yet, due to limited time and
# challenging data.
#
# Call with count of households in each ctcode
# (e.g., bst$sc86b01$margin$ctcode$.data)
makeCensusTTSMap <- function(ctcodes) {
    # Count of how many households TTS thinks there are in each 2001 tz
    count_tts01<- read.csv('../rawdata/tts_hhcount_1986_tz2001/drsout8456.csv')
    count_tts01 <- data.frame(tts01 = count_tts01[,1], count = count_tts01[,3])
    count_tts01 <- count_tts01[-nrow(count_tts01),]
    rownames(count_tts01) <- NULL

    # Each tuple here represents a set of 2001 zones. The first entry in the
    # tuple is the original 1996 zone, which has been split into multiple zones
    # in 2001.
    tts01_tts96_map <- list(
        list(42,466),
        list(98,477),
        list(103,467),
        list(147,481),
        list(242,480),
        list(253,478,479),
        list(267,475),
        list(268,476),
        list(306,468),
        list(391,471),
        list(392,472),
        list(404,473),
        list(405,474),
        list(427,465),
        list(436,470),
        list(437,469),
        list(440,464),
        list(1609,1716), # 2001 geography is bad here - 1609 doesn't show as being split.
        list(1689,1753),
        list(1696,1751),
        list(1708,1752),
        list(2047,2184,2185),
        list(2048,2182,2183),
        list(2049,2180,2181),
        list(2081,2190),
        # 2082/2083 boundary seems to have moved...
        # 2088/2090 boundary seems to have moved...
        list(2094,2189),
        list(2095,2188), # 2188 shows up in the 2001 map, but no data... zero pop?
        list(2098,2191),
        list(2162,2194),
        list(2165,2195),
        list(2177,2196),
        list(2178,2197)
    )
    # Apply the map to get counts in TTS96 zones.
    count_tts96 <- data.frame(tts96 = count_tts01$tts01, count = count_tts01$count)
    remove(count_tts01)
    for(i in tts01_tts96_map) {
        dest <- which(count_tts96$tts96 == i[[1]])
        src <- match(i[-1], count_tts96$tts96)
        src <- src[!is.na(src)]
        if(length(src) > 0) {
            count_tts96$count[dest] <- count_tts96$count[dest] +
                sum(count_tts96$count[src])
            count_tts96 <- count_tts96[-src,]
        }
    }



    # Mapping between 96 TZs and 86 CTs, using area of overlap to decide how to
    # (say) split a large CT into multiple TZs.
    ctcode_tts96_map <- inputData('ilute.zones_ct86_tts96_area', '*')
    # Only keep codes for our current CMA
    ctcode_tts96_map$ctcode <-
        factor(match(ctcode_tts96_map$ct86, names(ctcodes)),
               levels = seq(ctcodes),
               labels = names(ctcodes))
    ctcode_tts96_map <- ctcode_tts96_map[!is.na(ctcode_tts96_map$ctcode),]
    ctcode_tts96_map$tts96 <- factor(ctcode_tts96_map$tts96)
    ctcode_tts96_map$ct86 <- NULL

    # Several zones do not show up in the TTS96 counts table. These zones 
    # have zero residential population.
    count_tts96 <- rbind(count_tts96,
        data.frame(tts96 = setdiff(levels(ctcode_tts96_map$tts96),
                                   count_tts96$tts96),
                   count = 0))
    # count_tts96 has no data on zones with codes over 2670 - Bradford/West
    # Gwillumbury, Orangeville and Grimsby, in particular.
    # These correspond to ctcodes 4560-4567, 3015-3018, and a tiny part of
    # ctcode 3045.
    # Get rid of the zeros for these zones, since they're just unknown.
    count_tts96 <- count_tts96[as.integer(levels(count_tts96$tts96)[as.integer(count_tts96$tts96)]) <= 2670,]



    # Ideally, we'd like to do an IPF here: fit the map (prior table) to
    # two margins:
    # 1) the tts96 zone total
    # 2) the census code zone total
    # (giving preference to the census counts)
    #
    # However, that's challenging since we're missing totals for many TTS
    # zones. Instead, we just use the sparsity information - force some TTS
    # zones to have zero population, by modifying the map where they show up.
    #
    # The one exception is TTS zone 147, which has a tiny population (too small
    # for TTS to capture).
    count_tts96$count[count_tts96$tts96 == 147] <- 1
    ctcode_tts96_map <- ctcode_tts96_map[
        is.na(match(ctcode_tts96_map$tts96,
                    count_tts96$tts96[count_tts96$count==0])),]

    rownames(ctcode_tts96_map) <- NULL
    ctcode_tts96_map <- ctcode_tts96_map[order(ctcode_tts96_map$ctcode,
                                               ctcode_tts96_map$tts96),
                                         c('ctcode', 'tts96', 'area')]
    colnames(ctcode_tts96_map)[[3]] <- 'weight'

    return(ctcode_tts96_map)
}

