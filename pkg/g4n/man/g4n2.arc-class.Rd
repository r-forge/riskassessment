\name{arc-class}
\docType{class}
\alias{arc-class}
\alias{print,arc-method}

\title{Class "arc" ~~~ }
\description{	To define the structure (DAG) of a Bayesian network as
  its lower level (no names, only the arcs on the numbered nodes/variables.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("arc",nbn=3,fle=matrix(c(1,2,2,3,100,100),2))}.
}
\section{Slots}{
	 \describe{
    \item{\code{nbn}:}{Object of class \code{"numeric"}: number of nodes
    in the /bn/.}
    \item{\code{fle}:}{Object of class \code{"matrix"}: a matrix
      indicating the arcs of the /bn/, each row defines an arc (parent
      node, child node, arc decoration).
    }
  }
}
\section{Methods}{
  \describe{
    \item{print}{see \code{print8arc} for the details.}
    }
}

\author{ J.-B. Denis }
\note{ ~~further notes~~ }

\examples{
showClass("arc")
}
\keyword{classes}
