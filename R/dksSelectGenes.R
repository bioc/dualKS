`dksSelectGenes` <-
function(data, n) {
    if(!class(data)=="DKSGeneScores") {
        stop("Error in dksSelectGenes: data must be of class DKSGeneScores")
    }
    ggU <- new("DKSGeneSet", genes=as.vector(NA), classes=factor(NA))
    ggD <- new("DKSGeneSet", genes=as.vector(NA), classes=factor(NA))
    if(!is.na(data@gscores.up[1])) {
        up <- dksToGeneSet(data@gscores.up, n)
        ggU@genes <- up$ids
        ggU@classes <- up$class
    }
    if(!is.na(data@gscores.down[1])) {
        down <- dksToGeneSet(data@gscores.down, n)
        ggD@genes <- down$ids
        ggD@classes <- down$class
    }
    return(new("DKSClassifier", genes.up=ggU, genes.down=ggD))
}

