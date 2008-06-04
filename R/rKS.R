`rKS` <-
function(data, class, decreasing=TRUE, weights=NULL, method="kort") {
  if(!method %in% c("kort", "yang")) {
    stop("Method must be one of 'kort' or 'yang'")
  }
  class <- factor(class)
  nlv <- length(levels(class))
  ix.na <- which(is.na(data))
  if(length(ix.na)) {
    cn <- unique(class[-ix.na])
    if(length(cn) < nlv)
        return(NA)
  }
  if(is.null(weights)) weights <- rep(1, nlv)
  ix <- c(sort(data, index.return=TRUE, decreasing=decreasing)$ix, ix.na)
  class <- as.numeric(class[ix])
  n <- length(data)
  r <- matrix(nrow=n, ncol=nlv)
  for(coi in unique(class)) {
    ixx <- which(class == coi)
    nc <- length(ixx)
    sc <- n / nc
    pen <- n / (n - nc)
    ks <- rep(-pen, n)
    ks[ixx] <- weights[coi] * (n / nc)
    cs <- cumsum(ks)
    r[, coi] <- cs
  }
  colnames(r) <- unique(class)
  if(method == "kort") {    
    return(apply(r, 2, max))
  } else {
    dif <- function(x)
      return(max(x) + min(x))
    return(apply(r, 2, dif))
  }
}


