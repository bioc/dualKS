\name{dksTrain}
\alias{dksTrain}
\title{ Perform Dual KS Discriminant Analysis }
\description{
	This function will perform dual KS discriminant analysis on a 
	training set of gene expression data (in the form of an 
	ExpressionSet) and a vector of classes describing which of 
	(two or more) classes each column of data corresponds to.  Genes 
	will be be ranked based on the degree to which they are 
	upregulated or downregulated in each class, or both.  
	Discriminant gene signatures are then extracted using 
	dksSelectGenes and applied to new samples with dksClassify.
}

\usage{
	dksTrain(eset, class, type = "up", verbose=FALSE, weights=FALSE, logweights=TRUE, method='kort')
}

\arguments{
  \item{eset}{Gene expression data in the form of an 
				\code{ExpressionSet} or \code{matrix}}
  \item{class}{A factor with two or more levels indicating which 
				class each sample in the expression set belongs OR 
				an integer indicating which column of pData(eset) 
				contains this information.}
  \item{type}{One of "up", "down", or "both" indicating whether you 
			  want to analyze and classify based on up or down 
			  regulated genes, or both (note that classification of 
			  samples based on down regulated genes from single 
			  color experiments should be expected to work well due 
			  to the noise at low expression levels.  Therefore, 
			  'down', or 'both' should only be used for two color 
			  experiments or one color data that has been converted 
			  to ratios based on some reference sample(s).)}
  \item{verbose}{Set to TRUE if you want more evidence of progress
				while data is being processed.  Set to FALSE if you 
				want your CPU cycles to be used on analysis and not 
				printing messages.}
  \item{weights}{ Value determines whether and how genes are weighted 
			      when building the signatures.  See details.}
  \item{logweights}{ Should the weights be log10 transformed prior to applying?}
  \item{method}{ Two methods are supported.  The 'kort' method returns 
				the maximum of the running sum.  The 'yang' method 
				returns the sum of the maximum and the minimum of the 
				running sum, thereby penalizing genes that are highly enriched
				in a subset of samples of a given class, but highly 
				down regulated in another subset of that same class.}
}
\details{
This function calculates the Kolmogorov-Smirnov rank sum statistic for 
each gene and each level of 'class'.  The highest scoring genes can 
then be extracted for use in classification.

If weights=FALSE, signatures are defined based on the ranks of members 
of each class when sorted on each gene.  Those genes for which a given 
class has the highest rank when sorting samples by those genes will 
be included in the classifier, with no regard to the absolute expression 
level of those genes.  This is the classic KS statistic.

Very discriminant genes identified in this way may or may not be the 
highest expressed genes.  The result is that signatures identified 
in this way have arbitrary "baseline" values.  This may lead to 
misclassification when comparing two signatures (using, for example, 
\code{\link{dksClassify}}).  Therefore, one may wish to weight genes 
based on absolute expression level, or some other metric.

Setting \code{weights = TRUE} causes the genes to be weighted according 
to the log (base 10) of the relative rank of the mean expression of 
each gene in each class.  Alternatively, you may provide your own weight 
matrix as the argument to \code{weights}.  This matrix must have one 
column for each possible value of \code{class}, and one row for each 
gene in \code{eset}.  Note that for \code{type='down'} or the down 
component of \code{type='both'}, the weight matrix will be inverted 
as \code{1-matrix}, so the range of weights should be 0 - 1 for each 
class.  NAs are handled "gracefully" by discarding any 
genes for which any column of the corresponding row of \code{weights} 
is NA.  Our experience has been that weights that are a linear function 
of some feature of the gene expression (like mean) can be too subtle.  The 
effect of the weights can be increased by setting \code{logweights=TRUE} 
(which is the default).

}

\value{
	An object of class \code{\link{DKSGeneScores}}.
}

\author{Eric J. Kort, Yarong Yang}
\seealso{\code{\link{dksTrain}}, \code{\link{dksSelectGenes}},
	\code{\link{dksClassify}}, \code{\link{DKSGeneScores}}, 
	\code{\link{DKSPredicted}}, 
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