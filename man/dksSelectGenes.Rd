\name{dksSelectGenes}
\alias{dksSelectGenes}
\title{ Extract gene signatures from a DKSGeneScores object. }
\description{
	The \code{DKSGeneScores} returned by \code{\link{dksTrain}} holds 
	the rank data for all the genes in the original \code{ExpressioSet}. 
	However, generally only the top n genes for each class are desired 
	for classification.  Rather than needing to re-run \code{dksTrain} 
	every time a signature of different size (n) is desired, you 
	simply extract that top n genes from this object using dksSelectGenes.
}
\usage{
dksSelectGenes(data, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ An object of class \code{DKSGeneScores}, typically generated
		by \code{dksTrain}}
  \item{n}{ The number of genes, per class, to include in the classification 
		signature.}
}
\value{
	An object of class \code{\link{DKSGeneScores}}
}
\author{Eric J. Kort, Yarong Yang}
\seealso{\code{\link{dksTrain}}, \code{\link{dksSelectGenes}},
	\code{\link{dksClassify}}, \code{\link{DKSGeneScores}}, \code{\link{DKSPredicted}}, 
	\code{\link{DKSClassifier}}}
	
\examples{
	data("dks")
	tr <- dksTrain(eset, 1, "up")
	cl <- dksSelectGenes(tr, 100)
	pr <- dksClassify(eset, cl)
	summary(pr, pData(eset)[,1])
	show(pr)
	plot(pr, actual=pData(eset)[,1])	
}

\keyword{classif}
