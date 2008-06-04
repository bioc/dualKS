setMethod("show", "DKSPredicted", 
    function(object) {
    pred <- object
    tab <- cbind(pred@samples, as.vector(pred@predictedClass), 
        round(as.vector(pred@predictedScore), 3))
    colnames(tab) <- c("sample", "predicted class", "prediction score")
    rownames(tab) <- 1:nrow(tab)
    tab <- as.data.frame(tab)
    print(tab)  
})
