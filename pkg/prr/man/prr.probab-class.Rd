\name{probab-class}
\docType{class}
\alias{probab-class}
\alias{print,probab-method}

\title{Class /probab/ to specify the essential part of a probability distribution }
\description{
 Objects of class /probab/ gives the possibility to store in a
 standardized way, important pieces of information about a probability
 distribution: the family, the support, and possibly the truncation
 interval. This a basic object not to be handle by end-users. Notice 
 that no slot is devoted to the name of the described node. It is used
 in /distri/ and /nod/ objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("probab")}.
}
\section{Slots}{
	 \describe{
    \item{\code{family}:}{A \code{character(1)} giving the
      the type of distribution like 'beta' or 'normal'. The possible
      families are stored into the constant \code{prr.tdis}. }
    \item{\code{pafixe}:}{Object of class \code{"numeric"} giving the
      truncation interval for the \code{normal} family, the support and
      the truncation intervals for the \code{lognormal} and \code{beta}
      families, and the limits defining the categories for the
      \code{categorical} family.}
  }
}
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8probab} for the details. }
         }
}

\author{ Jean-Baptiste Denis }

\examples{
showClass("probab");
}
\keyword{classes}
