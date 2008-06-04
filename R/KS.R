`KS` <-
function(data, geneset, decreasing=TRUE, method="kort") {
  if(!method %in% c("kort", "yang")) {
    stop("Method must be one of 'kort' or 'yang'")
  }

  if(sum(is.na(data))) {
        ixrm <- which(is.na(data))
        data <- data[-ixrm]
  }
  ix <- sort(data, index.return=TRUE, decreasing=decreasing)$ix
  r <- matrix(nrow=length(data), ncol=length(levels(geneset@classes)))
  for(coi in levels(geneset@classes)) {
    gs.coi <- length(which(geneset@classes %in% coi))
    coi.ix <- which(geneset@classes %in% coi)
    gs.ix <- which(names(data[ix]) %in% geneset@genes[coi.ix])
    sc <- length(data) / gs.coi
    pen <- length(data) / (length(data) - gs.coi)
    ks <- rep(-pen, length(data))
    ks[gs.ix] <- sc
    cs <- cumsum(ks)
    r[, which(levels(geneset@classes) %in% coi)] <- cs
  }
  colnames(r) <- levels(geneset@classes)
  rownames(r) <- names(data)[ix]
  if(method == "kort") { 	 
	return(list(runningSums=r, ksScores=apply(r, 2, max)))
  } else {
    dif <- function(x)
      return(max(x) + min(x))
    return(list(runningSums=r, ksScores=apply(r, 2, dif)))
  }  
}

