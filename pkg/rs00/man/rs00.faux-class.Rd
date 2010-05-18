\name{faux-class}
\docType{class}
\alias{faux-class}
\alias{print,faux-method}

\title{Class "faux" to handle errors occurring within functions }
\description{ When an error occurred when running some code inside a function, the first step is to stop the process with an error message. Here an attempt is made to catch the error and return the error message through a standardized object which can filtered at any level until the hierarchy of calls.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("faux", ...)}.
	
}
\section{Slots}{
	 \describe{
    \item{\code{level}:}{Object of class \code{"integer"} 0 (no error), 1 (warning), 2 (fatal error) }
    \item{\code{name}:}{Object of class \code{"character"} slot of a /des/ }
    \item{\code{orig}:}{Object of class \code{"character"} slot of a /des/ }
    \item{\code{time}:}{Object of class \code{"character"} slot of a /des/ }
    \item{\code{defi}:}{Object of class \code{"character"} slot of a /des/ }
    \item{\code{role}:}{Object of class \code{"character"} slot of a /des/ }
    \item{\code{comm}:}{Object of class \code{"character"} slot of a /des/ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{des}"}, directly.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "faux")}: see \code{print8faux} for the details. }
	 }
}

\author{ Jean-Baptiste Denis }
\note{ ~~further notes~~ }

\examples{
showClass("faux");
rs003k("reset"); # needed only for R checking, to be forgotten
print(new("faux",level=0,name="No Error"));
}
\keyword{classes}
