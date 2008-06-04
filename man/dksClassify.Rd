\name{dksClassify}
\alias{dksClassify}
\title{ Predict classes for gene expression sets. }
\description{
	Kolmogorov-Smirnov rank sum scoring will be used to assign 
	one or more samples to one of two or more classes based on 
	previously defined gene signatures (see \code{\link{dksTrain}}).
}
\usage{
dksClassify(eset, classifier, rescale=FALSE, method="kort")
}
\arguments{
  \item{eset}{An \code{ExpressionSet} or \code{matrix} containing the gene 
		expression data for the samples to be classified.}
  \item{classifier}{ An \code{\link{DKSClassifier}} produced by 
		\code{\link{dksSelectGenes}} describing the gene expression 
		signature for each class.}
  \item{rescale}{ If TRUE, scores for each class will be mean centered 
		and normalized to remove arbitrary differences in scale and baseline 
		value between signatures for different classes.}
  \item{method}{ Two methods are supported.  The 'kort' method returns 
				the maximum of the running sum.  The 'yang' method 
				returns the sum of the maximum and the minimum of the 
				running sum, thereby penalizing classes that are highly enriched
				in a subset of genes of a given signature, but highly 
				down regulated in another subset of that same signature.}
}
\value{
  An object of class \code{\link{DKSPredicted}} containing the 
  class to which each sample in the \code{eset} was assigned as 
  well as other information.  This object has its own \code{summary} 
  and \code{show} functions useful for displaying this information 
  in a user friendly format.
}
\author{Eric J. Kort, Yarong Yang}
\seealso{\code{\link{dksTrain}}, \code{\link{dksSelectGenes}},
	\code{\link{dksClassify}}, \code{\link{DKSGeneScores}}, \code{\link{DKSPredicted}}, 
	\code{\link{DKSClassifier}}}

\examples{

	data("dks")
	tr <- dksTrain(eset, 1, "up")
	cl <- dksSelectGenes(tr, 100)
	pr <- dksClassify(eset, cl,rescale=FALSE)
	summary(pr, pData(eset)[,1])
	show(pr)
	plot(pr, actual=pData(eset)[,1])	
}

\keyword{classif}
