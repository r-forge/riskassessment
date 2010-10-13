\name{des-class}
\docType{class}
\alias{des-class}
\alias{print,des-method}

\title{Class /des/ to identify and characterize in common language }
\description{
 Objects of class /des/ gives the possibility to store in a standardized
 way, different pieces of information about a data frame, a function, a
 Bayesian network,... anything having got some identity. Standard
 facilities are available to display them. Not all the slots need to be filled, only \code{name} is compulsory.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("des",name="myobject")}.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"}  name of the object being described. It must be a scalar. It is the only compulsory slot, the other ones can be \code{character(0)}. }
    \item{\code{orig}:}{Object of class \code{"character"} scalar giving the origin  }
    \item{\code{time}:}{Object of class \code{"character"} scalar giving the time of creation/modification }
    \item{\code{defi}:}{Object of class \code{"character"} scalar giving the definition }
    \item{\code{role}:}{Object of class \code{"character"} scalar giving the role of the structure }
    \item{\code{comm}:}{Object of class \code{"character"} can be a vector, each component of it will be interpreted as a paragraph.}
  }
}
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8des} for the details. }
         }
}

\author{ Jean-Baptiste Denis }

\examples{
showClass("des");
}
\keyword{classes}
