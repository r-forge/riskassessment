\name{pgr-class}
\docType{class}
\alias{pgr-class}
\alias{print,pgr-method}

\title{Class "pgr" to format the plot of /gn/ (mainly DAG associated
to Bayesian networks).}
\description{	parameter defining the style of the arrows, padding,
zooming, ... are incorporated in this constant.}

\section{Objects from the Class}{
 The constant can be modified by calling
 \code{rsba3k("pgr","nval",value)} where value is a valid /pgr/.
}
\section{Slots}{
	 \describe{
    \item{\code{unitscale}:}{Object of class \code{"numeric"}: ???}
    \item{\code{padding}:}{Object of class \code{"numeric"}: To obtain
 some margins around the representation.}
    \item{\code{cexscale}:}{Object of class \code{"numeric"}: ??? }
    \item{\code{arrowlength}:}{Object of class \code{"numeric"}: ??? }
    \item{\code{sscale}:}{Object of class \code{"numeric"}: ??? }
    \item{\code{kzoom}:}{Object of class \code{"numeric"}:  The
 magnifying coefficient of the graph (if greater than one, only a part
 of the graph is plotted).}
    \item{\code{diar}:}{Object of class \code{"numeric"}: the distance
 between the arrow heads and the center of the nodes.}
  }
}
\section{Methods}{
  \describe{
    \item{print}{ See \code{print8pgr} for the details. }
	 }
}

\author{ J.-B. Denis }
\note{ ~~further notes~~ }

\examples{
showClass("pgr")
}
\keyword{classes}
