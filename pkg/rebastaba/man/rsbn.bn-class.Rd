\name{bn-class}
\docType{class}
\alias{bn-class}
\alias{plot,bn-method}
\alias{print,bn-method}

\title{Class for Bayesian Networks }
\description{	rebastaba objects describing Bayesian networks with a great variety of nodes and distributions.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("bn", ...)}.
	See the slot definitions below.
}
\section{Slots}{
	 \describe{
    \item{\code{description}:}{Object of class \code{"des"} Just the description identifying the object. }
    \item{\code{nom}:}{Object of class \code{"nom"} defines the node names (and variables names) associated to the items of the /bn/. }
    \item{\code{ntype}:}{Object of class \code{"character"} defines the different node types of the /bn/. }
    \item{\code{ndes}:}{Object of class \code{"list"} provides the descriptions of every node. }
    \item{\code{npara}:}{Object of class \code{"list"} contains the parameter definitions for each node }
    \item{\code{nrep}:}{Object of class \code{"numeric"} numbers of repetitions of the nodes (0 means no repetition). }
    \item{\code{ntransfo}:}{Object of class \code{"character"} The definition of the transformation to apply to all variates of each node. }
    \item{\code{ndaf}:}{Object of class \code{"list"} contains the definition of the associated data frame to the node if any. }
    \item{\code{nwin}:}{Object of class \code{"list"} contains the definition of the associated window for empirical drawing to the node if any. }
    \item{\code{nfun}:}{Object of class \code{"list"}  if necessary. }
    \item{\code{nfug}:}{Object of class \code{"list"} contains the generated function (computed by rebastaba) to the nodes }
    \item{\code{vnat}:}{Object of class \code{"character"} natures of all variates of the /bn/. }
    \item{\code{vpod}:}{Object of class \code{"list"} possible domains for all variates of the /bn/. }
    \item{\code{vred}:}{Object of class \code{"list"} representation domains for all variates of the /bn/. }
    \item{\code{vcod}:}{Object of class \code{"list"} common domains for all variates of the /bn/. }
    \item{\code{vparent}:}{Object of class \code{"list"} parents of each variate of the /bn/. }
  }
}
\section{Methods}{
  \describe{
    \item{print}{  see \code{print8bn} for the details.}
    \item{plot}{  see \code{plot8bn} for the details.}
	 }
}
\author{ J.-B. Denis}

\examples{
showClass("bn")
}
\keyword{classes}
