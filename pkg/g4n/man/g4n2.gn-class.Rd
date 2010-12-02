\name{gn-class}
\docType{class}
\alias{gn-class}
\alias{plot,gn-method}
\alias{print,gn-method}

\title{Class for graphs associated to Bayesian networks }
\description{	Rebastaba object describing directed graphs. These are usually associated to DAGs of some Bayesian network but the constraint of no cycle is not applied.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gn", ...)}.
 See the descriptions for the slots below. 
}
\section{Slots}{
	 \describe{
    \item{\code{description}:}{Object of class \code{"des"} Just the description identifying the object. }
    \item{\code{nom}:}{Object of class \code{"nom"} To define the node names (and variables names) associated to the items of the /gn/. }
    \item{\code{item}:}{Object of class \code{"character"} Either \code{n} or \code{y} to indicate which level was choosen (nodes or variables). }
    \item{\code{pos}:}{Object of class \code{"pos"} The position of the node into $R^3$ by means of a four column matrix. As many rows as items. The first three columns for the positions and the last one for the decoration style of nodes. }
    \item{\code{arc}:}{Object of class \code{"arc"} The relationships between items (see the description of the class for details. }
    \item{\code{pgr}:}{Object of class \code{"pgr"} Formatting parameters for the plotting of the /gn/. }
  }
}
\section{Methods}{
  \describe{
    \item{print}{  see \code{print8gn} for the details.}
    \item{plot}{  see \code{plot8gn} for the details.}
	 }
}
\author{ J.-B. Denis}

\examples{
showClass("gn")
}
\keyword{classes}
