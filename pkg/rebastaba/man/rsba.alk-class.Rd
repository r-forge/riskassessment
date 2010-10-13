\name{alk-class}
\docType{class}
\alias{alk-class}
\alias{print,alk-method}

\title{Class "alk" to define *a*sked *l*in*k* by the user }
\description{ Link was the former term for node, so asked link means
  asked node in the spirit that a user is defining a node to be
  incorporated to an already initiated /bn/. In fact there are two
  kinds of /alk/: either defined per se or fitted to a given /bn/ (for
  instance to take advantage of the parent properties). In the latter
  case, it is said completed and this is indicated with the @lcomp slot.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("alk")}.
}
\section{Slots}{
	 \describe{
    \item{\code{ldes}:}{Object of class \code{"des"}: description of
    the /alk/ }
    \item{\code{ltype}:}{Object of class \code{"character"}: type(s) of
    the node variable(s)}
    \item{\code{lpara}:}{Object of class \code{"list"}: parameters
    when parametric distribution of the variables }
    \item{\code{lrep}:}{Object of class \code{"numeric"}: number of
    repeated variables of the node. Must be 0 when there are no
    repetitions. }
    \item{\code{lnat}:}{Object of class \code{"character"}: natures of
    the node variables}
    \item{\code{lvar}:}{Object of class \code{"character"}: variate
    names of the node (must be "" when only one implicit variable.  }
    \item{\code{lparent}:}{Object of class \code{"character"}: parents
    of the node.}
    \item{\code{lpod}:}{Object of class \code{"list"}: maximal ranges
    for each variable of the node (lower and upper limit for numeric
    variables; all accepted values for categoric variables). }
    \item{\code{lred}:}{Object of class \code{"list"}: like slot @lpod
    but defines the maximal ranges used in graphs.}
    \item{\code{lcod}:}{Object of class \code{"list"}: like slot @lpod
    but defines the supposed common ranges of the variables (used for 
    presentations).}
    \item{\code{ltransfo}:}{Object of class \code{"character"}:
    easyprogramming defining the possible transformation to apply to
    all variables of the node. }
    \item{\code{ldaf}:}{Object of class \code{"daf"}: when data based
    node defines the related data.frame to the definition of the node. }
    \item{\code{lwin}:}{Object of class \code{"win"}: when data based
    node defined the way to perform the non parametric drawing. }
    \item{\code{lfunct}:}{Object of class \code{"function"}: when user
    programmed node, is the function for random draws of the node.}
    \item{\code{lcomp}:}{Object of class \code{"logical"}: indicates
    if the node is completed or not. }
  }
}
\section{Methods}{
  \describe{
    \item{print}{ See \code{print8alk} for the details. }
	 }
}

\author{ J.-B. Denis }
\note{ ~~further notes~~ }

\examples{
showClass("alk")
}
\keyword{classes}
