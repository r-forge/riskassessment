\name{ion-class}
\docType{class}
\alias{ion-class}
\alias{length,ion-method}
\alias{print,ion-method}

\title{Class "ion" to define from different viewpoints a subset of nodes and/or variables of a /nom/. }
\description{ nodes and/or variables can be identified as characters or as numbers. The key function is \code{nv2ion}.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ion", ,nn=LETTERS[1:3])}.
	
}
\section{Slots}{
	 \describe{
    \item{\code{nn}:}{Object of class \code{"character"}: node names }
    \item{\code{vn}:}{Object of class \code{"character"}: variable names }
    \item{\code{nvn}:}{Object of class \code{"character"} node[variable] names }
    \item{\code{nk}:}{Object of class \code{"numeric"} node numbers }
    \item{\code{ij}:}{Object of class \code{"numeric"} variable numbers within nodes }
    \item{\code{vk}:}{Object of class \code{"numeric"} variable numbers}
    \item{\code{iden}:}{Object of class \code{"character"} identication of the way the subset of node/variable names (in inoput) is described. Can be "nn","nv" or "v". }
  }
}
\section{Methods}{
  \describe{
    \item{length}{\code{signature(x = "ion")}: see \code{length8ion} for the details. }
    \item{print}{\code{signature(x = "ion")}:  see \code{print8ion} for the details. }
	 }
}

\author{Jean-Baptiste Denis }
\note{ ~~further notes~~ }

\seealso{
	See Also the function \code{\link{nv2ion}},
}
\examples{
showClass("ion")
}
\keyword{classes}
