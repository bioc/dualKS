`dksWeights` <- 
function(eset, class) {
 
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
	ag <- function(x, ...) aggregate(x, ...)$x
	wt <- apply(data, 1, ag, list(class), mean, na.rm=TRUE)        
	wt <- apply(wt, 1, rank, ties.method="average", na.last="keep")
    wt <- wt / nrow(wt)
	wt
}
