`dksTrain` <-
function(eset, class, type="up", verbose=FALSE, weights=FALSE, logweights=TRUE, method="kort") {
    TYPES <- c("up", "down", "both")
    type <- pmatch(type, TYPES)
    if (is.na(type)) 
        stop("invalid type (must be 1 of 'up', 'down', or 'both')")
    if (type == -1) 
        stop("ambiguous type (must be 1 of 'up', 'down', or 'both')")
    type <- TYPES[type]
    if(class(eset) == "ExpressionSet" || class(eset) == "exprSet") {
        data <- exprs(eset)
    } else if (class(eset) == "matrix") {
        data <- eset
    } else {
        stop("'eset' must be of class 'ExpressionSet' or 'matrix'")
    }
    if(length(class) == 1) {
        class=pData(eset)[, class]
    } else if(!length(class) == ncol(data)) {
        stop("'class' must either be a column index for pData(eset) or a factor
                of length ncol(exprs(eset))")
    }
    class <- factor(class)
    up <- matrix()
    down <- matrix()
    if(type %in% c("both", "up")) {
        if(verbose) 
            print("Scoring upregulated genes")
        up <- rKSScan(data, class, decreasing=TRUE, verbose=verbose, weights=weights, logweights=logweights, method=method)
    }
    if(type %in% c("both", "down")) {
        if(verbose) 
            print("Scoring downregulated genes")
        down <- rKSScan(data, class, decreasing=FALSE, verbose=verbose, weights=weights, logweights=logweights, method=method)
    }
    return(new("DKSGeneScores", gscores.up=up, gscores.down=down))
}

