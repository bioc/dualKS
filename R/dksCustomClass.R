`dksCustomClass` <-
function(upgenes=NULL, upclass=NULL, downgenes=NULL, downclass=NULL) {
    ggU <- new("DKSGeneSet", genes=as.vector(NA), classes=factor(NA))
    ggD <- new("DKSGeneSet", genes=as.vector(NA), classes=factor(NA))

    if(!is.null(upgenes)) {
        ggU@genes <- upgenes
        ggU@classes <- factor(upclass)
    }
    if(!is.null(downgenes)) {
        ggD@genes <- downgenes
        ggD@classes <- factor(downclass)
    }
    return(new("DKSClassifier", genes.up=ggU, genes.down=ggD))
}

