`dksPerm` <-
function(eset, class, n=100, samples=100, type="up", rescale=FALSE, verbose=FALSE, 
		method="kort") {
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
    
    it <- ceiling(samples/(ncol(data)*length(levels(class))))
    total <- 0
    scores <- matrix(0, ncol=length(levels(class)))
    for(i in 1:it) {
        if(verbose) 
            cat("-->Completed", i, "of", it, "resamplings\n")
        cl <- sample(class, length(class))
        tr <- dksTrain(eset, cl, type, method=method)
        cl <- dksSelectGenes(tr, 100)
        pr <- dksClassify(eset, cl, method=method)
        scores <- rbind(scores, pr@scoreMatrix)
    }        

 if(rescale) {
        gammaLL <- function(params) {
            -sum(dgamma(data, shape=exp(params[1]), rate=exp(params[2]), log=TRUE))
        }
    } else {
        gammaLL <- function(params) {
            -sum(dgamma(data, shape=exp(params[1]), rate=exp(params[2]), log=TRUE))
        }
    }

    data <- as.vector(scores[-1,])
    if(rescale) {
        data <- ((data-min(data))/max(data)) + 0.0001
    } else {
        data <- data+abs(min(data))+1
    }
    shape <- mean(data)^2/var(data)
    scale <- var(data)/mean(data)
    rate <- 1/scale
    if (rescale) {
        opt <- optim(c(log(shape), log(rate)), gammaLL)
        cmd <- paste("f <- function(x) { 1 - pgamma(x, shape=", exp(opt$par[1]),
            ", rate=", exp(opt$par[2]), ")}", sep="")
    } else {
        opt <- optim(c(log(shape), log(rate)), gammaLL)
        cmd <- paste("f <- function(x) { 1 - pgamma(x, shape=", exp(opt$par[1]),
            ", rate=", exp(opt$par[2]), ")}", sep="")
    }
    return(eval(parse(text=cmd)))

}
