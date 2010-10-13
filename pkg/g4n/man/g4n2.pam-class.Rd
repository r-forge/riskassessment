\name{pam-class}
\docType{class}
\alias{pam-class}
\alias{print,pam-method}

\title{Class "pam" to define parentship matrices}

\description{	 square matrix with as many rows as nodes/variables;
  parents in rows, children in columns with 1 if a ReLaTion, 0 otherwise.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pam", rlt=matrix(c(0,1,1,rep(0,6)),3))}.
}

\section{Slots}{
 \describe{
  \item{\code{rlt}:}{Object of class \code{"matrix"}: the square
        matrix of relationships between parents (in rows) and children (in
        columns).}
  }
}

\section{Methods}{
  \describe{
   \item{print}{ see \code{print8pam} for the details }
  }
}

\author{ J.-B. Denis }

\examples{
showClass("pam")
}
\keyword{classes}
