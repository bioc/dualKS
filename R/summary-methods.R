setMethod("summary", "DKSPredicted", 
    function(object, ...) {
        pred <- object
        arg <- as.list(substitute(list(...)))
        if(length(arg) > 1) {
          actual <- eval(parse(text=arg[2]))
        } else {
          actual <- NULL
        }
    
        cat("\n\nDual KS Classification Summary:\n\n")
        cat("Predicted class frequencies:\n")
        print(table(pred@predictedClass))
        if (!is.null(actual)) {
            actual <- factor(actual)
        cat("\n\nConcordance rate (predicted==actual): ", sum(as.vector(pred@predictedClass) == 
            as.vector(actual))/length(actual) * 100, "%\n\n")
    }
})

 