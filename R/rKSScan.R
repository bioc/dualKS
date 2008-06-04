`rKSScan` <-
function(data, class, decreasing, verbose=FALSE, weights=FALSE, logweights=TRUE, method="kort") {
    class <- factor(class)
    sumna <- function(x) sum(is.na(x))
    if(class(weights) != "matrix" && !is.logical(weights))
        stop("argument 'weights' must be either a matrix or a logical")
    if(class(weights)=="matrix") {
        if(nrow(weights) != nrow(data) || ncol(weights) != length(levels(class))) 
            stop("weights must have nrow(data) rows and length(levels(class)) columns")
        wt <- weights
		if(!decreasing) 
			wt <- 1 - wt
		if(logweights) 
			wt <- log10(wt)
        nna <- apply(wt, 1, sumna)
        ixrm <- which(nna > 0)
        if(length(ixrm > 0)) {
            wt <- wt[-ixrm,]
            data <- data[-ixrm,]
        }
        r <- matrix(nrow=nrow(data), ncol=length(levels(class)))
		if(verbose) {
			for(i in 1:nrow(data)) {
				r[i,] <- rKS(data[i,], class, decreasing, weights=wt[i,], method=method)
			}
			if(i %% 1000 == 0) 
				cat("-->processed", i, "out of", nrow(data), "genes\n")
		} else {
			for(i in 1:nrow(data)) {
				r[i,] <- rKS(data[i,], class, decreasing, weights=wt[i,], method=method)
			}
		}
    } else if(weights) {
		wt <- dksWeights(data,class)
		if(!decreasing) 
			wt <- 1 - wt
		if(logweights) 
			wt <- log10(wt)
        nna <- apply(wt, 1, sumna)
        ixrm <- which(nna > 0)
        if(length(ixrm > 0)) {
            wt <- wt[-ixrm,]
            data <- data[-ixrm,]
        }
			r <- matrix(nrow=nrow(data), ncol=length(levels(class)))
		if(verbose) {
			for(i in 1:nrow(data)) {
				r[i,] <- rKS(data[i,], class, decreasing, weights=wt[i,], method=method)
			}
			if(i %% 1000 == 0) 
				cat("-->processed", i, "out of", nrow(data), "genes\n")
		} else {
			for(i in 1:nrow(data)) {
				r[i,] <- rKS(data[i,], class, decreasing, weights=wt[i,], method=method)
			}
		}
	} else {
		r <- matrix(nrow=nrow(data), ncol=length(levels(class)))
		if(verbose) {
			for(i in 1:nrow(data)) {
				r[i,] <- rKS(data[i,], class, decreasing, method=method)
			}
			if(i %% 1000 == 0) 
				cat("-->processed", i, "out of", nrow(data), "genes\n")
		} else {
			for(i in 1:nrow(data)) {
				r[i,] <- rKS(data[i,], class, decreasing, method=method)
			}
		}
    }
    rownames(r) <- rownames(data)
    colnames(r) <- levels(class)
    r
}

