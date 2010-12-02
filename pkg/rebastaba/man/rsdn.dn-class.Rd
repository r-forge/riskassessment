\name{dn-class}
\Rdversion{1.1}
\docType{class}
\alias{dn-class}
\alias{plot,dn-method}
\alias{print,dn-method}

\title{Class for data sets associated to bn, most often simulated }
\description{ rebastaba objects describing data associated (or associatable) to Bayesian networks. Each column of each is at the variable level (no distinction between node and variables). The last variable must be a scoring variable with a name given by rbsb.sco}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("dn", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{description}:}{Object of class \code{"des"} Description of the object. }
    \item{\code{nat}:}{Object of class \code{"character"} This vector provides the natures of every variables. }
    \item{\code{pod}:}{Object of class \code{"list"}  This list provides the possible domains of every variables ; each component is associated to a variable since the node structure is not used.}
    \item{\code{red}:}{Object of class \code{"list"} As \code{@pod} but for the representation domains. }
    \item{\code{cod}:}{Object of class \code{"list"} As \code{@pod} but for the common domains. }
    \item{\code{df}:}{Object of class \code{"data.frame"} The data frame itself, its variable names provides the names of the variables. }
    }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "dn")}: Standard plot of some variables. }
    \item{print}{\code{signature(x = "dn")}: Standard print of the object. }
	 }
}
\author{ Jean-Baptiste Denis }

\examples{
showClass("dn")
}
\keyword{classes}
