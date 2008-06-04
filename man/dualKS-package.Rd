\name{dualKS-package}
\alias{dualKS-package}
\alias{dualKS}
\docType{package}
\title{ Dual KS Discriminant Analysis and Classification}
\description{
	This package implements a Kolmogorov Smirnov rank-sum based 
	algorithm for training (i.e. discriminant analysis--identification 
	of genes that discriminate between classes) and classification 
	of gene expression data sets.  One of the chief strengths of 
	this approach is that it is amenable to the "multiclass" problem.
	That is, it can discriminate between more than 2 classes.
}
\details{
\tabular{ll}{
Package: \tab dualKS\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-04-18\cr
License: \tab GPL-3\cr
}

	Use of this package requires an \code{ExpressionSet}, pre-processed 
	to your liking, containing gene expression data you wish to train on 
	to identify discriminant genes.  Optionally, a second \code{ExpressionSet}
	may be employed for subsequent testing or "diagnosis".  
	
	If the \code{ExpressionSet} does not contain the appropriate phenoData 
	specifying the classes for each sample, a factor containing this information 
	will also be required. 
	
	See the examples for further details.
 
}
\author{
Eric J. Kort
Maintainer: <Eric.Kort@vai.org>
}
\keyword{ package }


