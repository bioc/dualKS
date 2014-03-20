`dksToGeneSet` <-
function(data, n=50) {
    .rank <- function(x) { rank(-x, ties.method="first") }
    .dup <- function(x, n) { sum(x <= n) > 1 }
    .match <- function(x, n) { sum(x <= n) == 1 }
    .class <- function(x, n) { which(x <= n) }
    uc <- 0
    ix <- 1:nrow(data)
    while(uc < (n*ncol(data))) {
        ranks <- apply(data[ix,], 2, .rank)
        dups <- apply(ranks, 1, .dup, n)
        if(sum(dups)) {
            ix <- ix[-which(dups)]
            ranks <- ranks[-which(dups),]
        }
        if(length(ix) < (n*ncol(data))) {
            stop("Error: could not find adequate number of unique 
                classifiers in data set")
        }
        unique <- apply(ranks, 1, .match, n)
        uc <- length(which(unique))
    }
    ranks <- ranks[which(unique),]
    ix <- ix[which(unique)]
    cl <- apply(ranks, 1, .class, n)
    return(list(ids=rownames(data)[ix], class=factor(colnames(data)[cl]), 
        index=ix))
}

