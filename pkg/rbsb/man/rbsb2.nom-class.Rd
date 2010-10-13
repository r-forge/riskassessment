\name{nom-class}
\docType{class}
\alias{nom-class}
\alias{print,nom-method}
\alias{length,nom-method}

\title{Class "nom" to describe the node and variable names of a /bn/ }

\description{	 list of variable names of each node, the component names are the node names when there is no variable name, must be "" NOT character(0) for the length giving giving the number of variables in any cases...}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("nom",,x=list(N1=c("a","b","c"),N2=""))}.

}

\section{Slots}{
 \describe{
  \item{\code{x}:}{Object of class \code{"list"} The name of each component of the list is associated to a node; when a node is multivariate the component is the vector of its variable names. If not, it must be "": character(0) is not accepted. }
  }
}

\section{Methods}{
 \describe{
  \item{print}{ see \code{print8nom} for the details }
  \item{length}{ see \code{length8nom} for the details }
  }
}

\author{ J.-B. Denis }


\examples{
showClass("nom")
}
\keyword{classes}
