\name{dksWeights}
\alias{dksWeights}
\title{ Calculate gene weights based on average expression. }
\description{
	Prior to selecting genes it may be desirable to calculate weights 
	for each genes so that some genes are more likely than others to be
	included in the gene signature all other things being equal.  This 
	function will calculate an N x M weight matrix for N genes in \code{data} 
	and M unique classes in \code{class}.  The weights are based on 
	mean expression of each gene in each class such that genes that are 
	highly expressed on average in a given class will be weighted more 
	highly when scoring genes for that class.
	
	The resulting weight matrix can be bassed to \code{\link{dksTrain}} as 
	the \code{weights} argument.
}
\usage{
dksWeights(eset, class)
}
\arguments{
  \item{eset}{An \code{ExpressionSet} or \code{matrix} containing the gene 
		expression data to be used for bootstrapping.}
  \item{class}{A factor with two or more levels indicating which 
				class each sample in the expression set belongs OR 
				an integer indicating which column of pData(eset) 
				contains this information.}
}
\note{ There are many metrics the user might want to use for weighting.  This 
	convenience function just implements one of the most obvious ones.  
	The user can provide his/her own N x M weight matrix to \code{\link{dksTrain}}.
	The weight matrix calculated by this function will be calculated on the fly 
	if the \code{weights} is set to TRUE when calling \code{\link{dksTrain}}.  
	However, it multiple calls to dksTrain are being made (for example when 
	performing some type of optimization or validation), it will save a lot of 
	time if the weight matrix is pre-calculated by a call to this function and the 
	resulting matrix supplied directly to \code{\link{dksTrain}} rather than having 
	it re-calculate the weight matrix every time.
 }
\value{
	An N x M matrix containing the weights for each gene and each class.
}
\author{Eric J. Kort}
\seealso{\code{\link{dksTrain}}, \code{\link{dksSelectGenes}},
	\code{\link{dksClassify}}, \code{\link{DKSGeneScores}}, 
	\code{\link{DKSPredicted}}, 
	\code{\link{DKSClassifier}}}

\examples{

	data("dks")
	wt <- dksWeights(eset, 1)
	str(wt)
}

\keyword{classif}
