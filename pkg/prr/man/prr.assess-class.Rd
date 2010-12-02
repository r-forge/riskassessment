\name{assess-class}
\docType{class}
\alias{assess-class}
\alias{print,assess-method}

\title{Class /assess/ to specify the assessment of an expert }
\description{
 Objects of class /assess/ store in a standardized way, different pieces of information about the expert's assessement of the values of a node. This a basic object not to be handle by end-users.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("assess",x="1.7",cV=28,cU=10)}.
}
\section{Slots}{
	 \describe{
    \item{\code{x}:}{Object of class \code{"character"} giving the central value(s) for the node (can be an easyprogramming expression, the reason why it is a character). }
    \item{\code{cV}:}{Object of class \code{"numeric"} providing the
      variability value(s) associated to the node, must be in the 0...100 interval. }
    \item{\code{cU}:}{Object of class \code{"numeric"} providing the
      uncertainty value(s) associated to the node, must be in the 0...100 interval. }
  }
}
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8assess} for the details. }
         }
}

\author{ Jean-Baptiste Denis }

\examples{
showClass("assess");
}
\keyword{classes}
