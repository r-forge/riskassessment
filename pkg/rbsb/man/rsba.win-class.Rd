\name{win-class}
\docType{class}
\alias{win-class}
\alias{print,win-method}

\title{Class "win" to define the window for a non-parametric regression}
\description{	The windows to select a subset of prediction individuals
  is based on some covariables. For each of them, a relative weight
 is attributed (@wgt). @k indicate the norm on which the distance is
 based. @di indicates the maximum distance for selection. @nb indicates
 the (minimum,maximum) numbers of individuals to include in the
 subset. @ty indicates the type of prediction performed on the selected
 subset: at present, only 'mean' and 'median' are implemented.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("win",wgt=1:4,k=2,di=5,nb=c(1,40),ty="mean")}.
}
\section{Slots}{
	 \describe{
    \item{\code{wgt}:}{Object of class \code{"numeric"}: the non
      negative weights for each variables }
    \item{\code{k}:}{Object of class \code{"numeric"}: the power coefficient }
    \item{\code{di}:}{Object of class \code{"numeric"}: the minimum and 
      the seires of maximum distances of individuals in the candidate subset.
      Must be of same length as slot \code{nb}. }
    \item{\code{nb}:}{Object of class \code{"numeric"}: the minimum and
      the series of maximum numbers of individuals in the candidate subset.
      Must be of same length as slot \code{di}. }
    \item{\code{ty}:}{Object of class \code{"character"}: the type of
      statistical summary. }
  }
}
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8win} for the details. }
         }
}

\author{ J.-B. Denis }
\note{ ~~further notes~~ }

\examples{
showClass("win")
}
\keyword{classes}
