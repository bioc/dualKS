\name{DKSGeneSet}
\docType{class}
\alias{class:DKSGeneSet}
\alias{DKSGeneSet}
\alias{DKSGeneSet-class}

\title{
  Class to contain gene signatures.
}

\description{
  This class contains a vector of genes, and a corresponding factor of 
  classes that indicates to which signature each gene belongs.  
  The function \code{\link{dksSelectGenes}} extracts gene signatures of a 
  specified size from an object of class \code{\link{DKSGeneScores}}.  
 
  calculates a score for each possible 
  class (as determined by the \code{DKSGeneSet} passed to it) for each 
  sample in the test set passed to the function.  It then determines which 
  class each sample most likely belongs to based on which of those scores is 
  the largest.  All this information is useful after the analysis, and so it is 
  retained in the return object of class \code{DKSPredicted}.
}

\section{Slots}{
	\describe{
		\item{\code{genes}:}{A vector of gene identifiers.  Order is not important.}
		\item{\code{classes}:}{A factor specifying which class (signatuer) each gene 
			belongs to.}
	}
}


\author{Eric J. Kort}

\seealso{
  \code{\link{DKSGeneScores-class}}, \code{\link{DKSClassifier-class}}, \code{\link{dksSelectGenes}}.
}


\keyword{classes}
