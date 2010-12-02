\name{distri-class}
\docType{class}
\alias{distri-class}
\alias{print,distri-method}

\title{Class /distri/ to numerically specify a probability distribution to be simulated }

\description{ Objects of class /distri/ indicate the main
  characteristics of the distribution with the help of a
  \code{/proba/} object, but also the number of elementary number of
  simulations, and the numerical values to attribute to the necessary
  parameters.  }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("distri",...)}
  where \dots will be replaced by its slots when default values are not
  convenient.}

\section{Slots}{
	 \describe{
    \item{\code{probab}:}{Object of class \code{"probab"}  to define
  the family and the fixed parameters.}
    \item{\code{nbdraw}:}{Object of class \code{"numeric(1)"}  to define
  the number of simulations when it is not given by the number of rows
  of the slot \code{parand} and these are constant over all the simulations.}
    \item{\code{parand}:}{Object of class \code{"matrix"} and
  \code{numeric} giving the necessary additional parameters for the
  simulation. Each row of the matrix correspond to a simulation. This
  matrix structure was implemented to allow different value between
  simulations. For the moment, the matrix has got only two columns for
  the central value and the variation coefficient. This gives a unified
  parameterization of all distributions and encompass the degenerate
  cases for extreme values of the variation coefficient. No need to
  give this matrix some dimnames. }
  }
}

\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8distri} for the details. }
         }
}

\author{ Jean-Baptiste Denis }

\examples{
showClass("distri");
}
\keyword{classes}
