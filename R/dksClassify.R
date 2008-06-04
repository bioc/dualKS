`dksClassify` <-
function(eset, classifier, rescale=FALSE, method="kort") {
    if(!class(classifier) == "DKSClassifier") {
        stop("classifier must be of class DKSClassifier.")
    }
    if(class(eset) == "ExpressionSet" || class(eset) == "exprSet") {
        data <- exprs(eset)
    } else if (class(eset) == "matrix") {
        data <- eset
    } else {
        stop("'eset' must be of class 'ExpressionSet' or 'matrix'")
    }

    for(i in 1:ncol(data)) {
        if(i == 1) {
            if(!is.na(classifier@genes.up@genes[1])) {
                scores <- KS(data[, i], classifier@genes.up, method=method)$ksScores
            }
            if(!is.na(classifier@genes.up@genes[1]) && 
                !is.na(classifier@genes.down@genes[1])) {        
                scores <- scores + KS(data[, i], classifier@genes.down, decreasing=FALSE, method=method)$ksScores    
            } else if(!is.na(classifier@genes.down@genes[1])) {
                scores <- KS(data[, i], classifier@genes.down, decreasing=FALSE, method=method)$ksScores
            }            
        } else {
            if(!is.na(classifier@genes.up@genes[1])) {
                sc <- KS(data[, i], classifier@genes.up, method=method)$ksScores
            } 
            if(!is.na(classifier@genes.up@genes[1]) && 
                !is.na(classifier@genes.down@genes[1])) {        
                sc <- sc + KS(data[, i], classifier@genes.down, decreasing=FALSE, method=method)$ksScores
            } else if(!is.na(classifier@genes.down@genes[1])) {
                sc <- KS(data[, i], classifier@genes.down, decreasing=FALSE, method=method)$ksScores
            }
            scores <- rbind(scores, sc)
        }
    }
    rownames(scores) <- colnames(data)
    if(rescale) {
        sc.mn <- apply(scores, 2, min, na.rm=TRUE)
        scores <- sweep(scores, 2, sc.mn)
        sc.mx <- apply(scores, 2, max, na.rm=TRUE)
        scores <- sweep(scores, 2, sc.mx, "/")    
    }
    pr <- predictClass(scores)
    return(new("DKSPredicted", samples=colnames(data), 
        predictedClass=pr$class, predictedScore=pr$score, scoreMatrix=scores))
}
