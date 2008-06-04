\name{DKSGeneScores}
\docType{class}
\alias{class:DKSGeneScores}
\alias{DKSGeneScores}

\alias{DKSGeneScores-class}

\title{
  Class to contain dual Kolmogorov Smirnov rank sum discriminant
  analysis results for gene expression data.
}

\description{
  The function dksTrain returns on object of class DKSGeneScore 
  to hold the analysis results for subsequent classifier extraction 
  and classification.  This way, the gene scoring does not need to 
  be repeated for classifiers based on different numbers of genes.
}
\section{Creating Objects}{

  \code{new("DKSGeneScore", 
	gscores.up=new("matrix"), 
	gscores.down=new("matrix"))}
}
\section{Slots}{
  \item{\code{gscores.up}:}{A matrix of gene scores, one column per 
	class}
  \item{\code{gscores.down}:}{A matrix of gene scores, one column per 
	class}
}

\author{Eric J. Kort}

\seealso{
  \code{\link{DKSPredicted-class}}, \code{\link{dksTrain}}.
}

\keyword{classes}
