setClass("DKSGeneScores", representation(gscores.up="matrix", 
    gscores.down="matrix"))
setClass("DKSGeneSet", representation(genes="vector", classes="factor"))
setClass("DKSClassifier", representation(genes.up="DKSGeneSet", 
    genes.down="DKSGeneSet"))
setClass("DKSPredicted", representation(samples="vector", 
    predictedClass="factor", predictedScore="vector", scoreMatrix="matrix"))
    