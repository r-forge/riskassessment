\name{nod-class}
\docType{class}
\alias{nod-class}
\alias{print,nod-method}

\title{Class /nod/ to specify the different nodes of a chain }
\description{
 Objects of class /nod/ gives the possibility to store in a standardized way, all pieces of information about a node. This a basic object. Contrary to /rebastaba/ package, all nodes are supposed associated to continuous (numeric) random variables.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("nod",name="myobject",...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character(1)"}  giving the name of the node.}
    \item{\code{probab}:}{Object of class \code{"probab"}  giving the essential characteristics of the node distribution.
      See
      the series of \code{prr.probab?} objects for examples. }
    \item{\code{assess}:}{Object of class \code{"assess"} giving the three values assessed by the expert: \code{x,cV,cU}. The last two ones are \code{numeric(1)} and must be comprised between 0 and 100. The first one is \code{character(1)} since either a numeric value or a character expression can be involved. The character expression is necessary to indicate relationship with other nodes. For instance \code{1+{{A}}*{{B}}} will indicate that the value to give to the node for \code{x} is the the product of the values of node \code{A} and \code{B} summed to one. Examples of \code{/assess/} are given by the objects \code{prr.assess?}. }
    \item{\code{format}:}{Object of class \code{"character(3)"} giving the format to apply when computing the three subnode of that node, respectively to \code{x,m,CV}; more precisely, it is the number of decimals to be kept, and for the two last if they must be stored (a negative value indicates that not).}
  }
}
 
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8nod} for the details. }
         }
}

\author{ Jean-Baptiste Denis }

\examples{showClass("nod");}
\keyword{classes}
