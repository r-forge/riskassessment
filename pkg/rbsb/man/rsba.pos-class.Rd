\name{pos-class}
\docType{class}
\alias{pos-class}
\alias{print,pos-method}

\title{Class "pos" to define the projection into $R^2$ of objects into $R^3$ }

\description{	As display of /gn/s is performed from a $R^3$ perspective, then /pos/ was issued to commonly define the projection into $R^2$ (paper, screen).}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("pos")} and
  modified with \code{rsba3k} function.
}
	

\section{Slots}{
	 \describe{
    \item{\code{posi}:}{Object of class \code{"matrix"}: each row is
    associated to a node; columns are the three coordinates and a
    decoration code for the node; then a nbnx4 matrix. }
    \item{\code{view}:}{Object of class \code{"numeric"}: the two
    angles (in grades ?) for the projection into $R^2$. (0,0) means the
    use of (x,y).  }
    \item{\code{zoom}:}{Object of class \code{"numeric"}: four values:
    center point after normalization on [-1,+1] instead of [min,MAX]
    and the the magnifying factor for both coordinates. (0,0,0,1)
    implies the standard fitted view.}
  }
}
\section{Methods}{
  \describe{
    \item{print}{ See \code{print8pos} for the details. }
	 }
}

\author{ J.-B. Denis }
\note{ ~~further notes~~ }

\examples{
showClass("pos")
}
\keyword{classes}
