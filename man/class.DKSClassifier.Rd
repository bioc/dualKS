\name{DKSClassifier}
\docType{class}
\alias{class:DKSClassifier}
\alias{DKSClassifier}

\alias{DKSClassifier-class}

\title{
  Class to contain gene set signatures.
}

\description{
  The function dksSelectGenes returns an object of class DKSClassifier
  that contains two \code{\link{DKSGeneSet}} objects: one for each 
  signature (upregulated and downregulated), needed  
  for subsequent classification. This way, the gene scoring does 
  not need to be repeated for classifiers based on different 
  numbers of genes (see \code{\link{DKSGeneScores}} and 
  \code{\link{dksTrain}}).
}

\author{Eric J. Kort}

\seealso{
  \code{\link{DKSGeneScores-class}}, \code{\link{dksTrain}}.
}
\keyword{classes}
