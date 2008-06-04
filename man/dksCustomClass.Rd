\name{dksCustomClass}
\alias{dksCustomClass}
\title{ Create a classification object from predefined gene signature. }
\description{
	This utility function will build a \code{\link{DKSClassifier}} object 
	from your own list of gene ids for use by \code{\link{dksClassify}}. 
	This is useful if you want to use the classification funtionality of 
	this package, but already have gene signatures you want to use (as 
	opposed to generating them with \code{\link{dksTrain}}.
}
\usage{
dksCustomClass(upgenes=NULL, upclass=NULL, downgenes=NULL, downclass=NULL)
}
\arguments{
  \item{upgenes}{A vector of gene ids that are upregulated in the classes 
		of intereset.  Note that upgenes and downgenes need not both be 
		specified, but at least one must be.}
  \item{upclass}{ A vector or factor of classes specifying which signature 
		each gene in upgenes belongs to.  (Note that upgenes can describe 
		an arbitrary number of classes, and the order of upgenes is not 
		important so long as the position of ids in upgenes and the position 
		of classes in upclass correspond to one another).}
  \item{downgenes}{A vector of gene ids that are downregulated in the classes 
		of intereset.  Note that upgenes and downgenes need not both be 
		specified, but at least one must be.}
  \item{downclass}{ A vector or factor of classes specifying which signature 
		each gene in downgenes belongs to.  (Note that downgenes can describe 
		an arbitrary number of classes, and the order of downgenes is not 
		important so long as the position of ids in downgenes and the position 
		of classes in downclass correspond to one another).}
}
\value{
  An object of class \code{\link{DKSClassifier}}.
}
\author{Eric J. Kort, Yarong Yang}
\seealso{\code{\link{dksTrain}}, \code{\link{dksSelectGenes}},
	\code{\link{dksClassify}}, \code{\link{DKSGeneScores}}, \code{\link{DKSPredicted}}, 
	\code{\link{DKSClassifier}}}

\examples{

	data("dks")
	up <- rownames(exprs(eset))[1:300]
	cls <- factor(rep(c(1,2,3), 100))
	classifier <- dksCustomClass(upgenes=up, upclass=cls)
}

\keyword{classif}
