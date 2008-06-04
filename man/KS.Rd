\name{KS}
\alias{KS}
\title{ Calculate Kolmogorov Smirnov rank sum scores. }
\description{
  This function calculates the degree to which a subset of genes (i.e. 
  a "signature") is biased in the ordered list of all genes.  The 
  function is typically used internally by \code{\link{dksClassify}}, 
  but the user may want to call it directly to inspect the running 
  sums. 
}
\usage{
KS(data, geneset, decreasing=TRUE, method="kort")
}
\arguments{
  \item{data}{A vector of gene expression data.  The data need not 
		be sorted, as the function will sort it itself.}
  \item{geneset}{ A \code{\link{DKSGeneSet}} object, such as one of 
		the slots of the \code{\link{DKSClassifier}} returned by 
		\code{link{dksClassify}}.}
  \item{decreasing}{ Indicates which way \code{data} should be sorted. 
		If TRUE, the degree of upregulation will be scored.  If FALSE, 
		the degree of down regulation will be scored.}
  \item{method}{ Two methods are supported.  The 'kort' method returns 
				the maximum of the running sum.  The 'yang' method 
				returns the sum of the maximum and the minimum of the 
				running sum, thereby penalizing classes that are highly enriched
				in a subset of genes of a given signature, but highly 
				down regulated in another subset of that same signature.}
}

\value{
  \item{runningSums }{A matrix with 1 row per gene and 1 column per 
		signature. The value is the running sum of the KS metric 
		at each point along the sorted list of genes.  The maximum 
		of this column vector corresponds to the KS score for the 
		corresponding signature.}
  \item{ksScores}{A named vector giving the KS score for each signature.}

}
\author{Eric J. Kort, Yarong Yang}
\seealso{\code{\link{dksTrain}}, \code{\link{dksSelectGenes}},
	\code{\link{dksClassify}}, \code{\link{DKSGeneScores}}, \code{\link{DKSPredicted}}, 
	\code{\link{DKSClassifier}}}

\examples{

	data("dks")
	tr <- dksTrain(eset, 1, "both")
	cl <- dksSelectGenes(tr, 100)
	sc <- KS(exprs(eset)[,1], cl@genes.up)
	plot(sc$runningSums[,1], type='l')
}

\keyword{classif}
