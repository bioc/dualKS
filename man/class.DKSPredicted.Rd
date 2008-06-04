\name{DKSPredicted}
\docType{class}
\alias{class:DKSPredicted}
\alias{DKSPredicted}
\alias{DKSPredicted-class}
\alias{plot,DKSPredicted-method}
\alias{plot,DKSPredicted,missing-method}
\alias{show,DKSPredicted-method}
\alias{summary,DKSPredicted-method}

\title{
  Class to contain classification results from dksClassify
}

\description{
  The function \code{\link{dksClassify}} calculates a score for each possible 
  class (as determined by the \code{DKSGeneSet} passed to it) for each 
  sample in the test set passed to the function.  It then determines which 
  class each sample most likely belongs to based on which of those scores is 
  the largest.  All this information is useful after the analysis, and so it is 
  retained in the return object of class \code{DKSPredicted}.
}
\section{Creating Objects}{

  \code{new("DKSPredicted", 
	samples=new("vector"),
	predictedClass=new("factor"),
	predictedScore=new("vector"),
	scoreMatrix=new("matrix"))}

}
\section{Slots}{
	\describe{
		\item{\code{samples}:}{A vector of sample names}
		\item{\code{predictedClass}:}{A factor specifying which class each sample 
			belongs to based on the classification.}
		\item{\code{predictedScore}:}{A vector giving the score corresponding to 
			the assigned class for each sample.}
		\item{\code{scoreMatrix}:}{The complete score matrix for future reference.}
	}
}

\section{Methods}{
  Standard generic methods:
  \describe{
     \item{\code{plot(DKSPredicted)}:}{Plot scores for each sample and class sorted, 
		in turn, by each class.}
     \item{\code{plot(DKSPredicted,missing)}:}{Plot scores for each sample and class sorted, 
		in turn, by each class.}
     \item{\code{summary(DKSPredicted)}:}{Display summary information for the classification.}
     \item{\code{summary(DKSPredicted, factor)}:}{Display summary information for the 
		classification including the percent correctly classified (as specified by the factor).}
     \item{\code{show(DKSPredicted)}:}{Displays a table of scores and predicted classes for each 
		sample.}

	}
}


\author{Eric J. Kort}

\seealso{
  \code{\link{DKSGeneScores-class}}, \code{\link{DKSClassifier-class}}, \code{\link{dksClassify}}.
}


\keyword{classes}
