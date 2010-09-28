\name{pta-class}
\docType{class}
\alias{pta-class}
\alias{print,pta-method}

\title{Class "pta" to store probability tables }
\description{ Objects of class "pta" store probability tables for a
subset variables conditionally to another subset of variables, and
conditioned by values for another subset of variables. Of course the
three subsets must be non intersecting, and the last two can be
empty. These /pta/ objects are restricted for categoric variables (the
associated /bn/ must be grappa compatible.)}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pta", ...)}.

}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"}: name of
    the probability table.  }
    \item{\code{vam}:}{Object of class \code{"character"}: variable
    names for the marginal dimensions.  }
    \item{\code{vac}:}{Object of class \code{"character"}: variable
    names for the conditional dimensions. }
    \item{\code{vad}:}{Object of class \code{"character"}: variable
    names for the conditioned dimensions. }
    \item{\code{vav}:}{Object of class \code{"character"}: values for
    the variables "vad". }
    \item{\code{kkk}:}{Object of class \code{"numeric"}: integer used
    to print the probabilities (see the details section in \code{print8pta}).    } 
    \item{\code{pro}:}{Object of class \code{"array"}:  array of the
    probabilities (can be proportional) } 
  }
}
\section{Methods}{
  \describe{
    \item{print}{ See \code{print8pta} for the details. }
	 }
}

\author{ J.-B. Denis }
\note{ ~~further notes~~ }

\examples{
showClass("pta")
}
\keyword{classes}
