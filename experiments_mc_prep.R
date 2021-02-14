#
# Run synthesize2 ~30 times, with different random number seeds
#

# [1] 897 266 372 573 908 202 898 945 661 629  62 206 177 687 384 770 498 718
#[19] 992 380 777 935 212 652 126 267 386  13 382 870

set.seed(0)
popseeds <- round(runif(30)*1000)
cmaname <- 'Toronto'
for(i in seq(popseeds)) {
    popseed <- popseeds[i]
    if(!file.exists(paste('../results/latest/pop_d-', cmaname,
                         '-s', formatC(popseed, width=3, flag='0'),
                         '.csv', sep='')) &&
       !file.exists(paste('../results/latest/pop_d-', cmaname,
                         '-s', formatC(popseed, width=3, flag='0'),
                         '.csv.bz2', sep=''))) {
        print(paste('Using seed', popseed))
        source('synthesize2.R')
        quit(save='no')
    }
}
