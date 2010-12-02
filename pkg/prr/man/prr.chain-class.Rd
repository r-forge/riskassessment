\name{chain-class}
\docType{class}
\alias{chain-class}
\alias{print,chain-method}

\title{Class /chain/ to specify a chain of nodes (in fact a variety of Bayesian networks).}
\description{
 Objects of class /chain/ gives the possibility to store in a
 standardized way, different pieces of information defining a
 chain. This a basic object not to be handle by end-users, usually it is
 created from a text file to be read with the function
 \code{read8chain}. 
 Examples of such text files can be found into the \code{tests} directory.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("chain",name="myobject")}.
}
\section{Slots}{
	 \describe{
    \item{\code{des}:}{Object of class \code{des}  giving useful information about the chain: definition, role, creation date,... }
    \item{\code{nos}:}{Object of class \code{list}, each component of it
      being a node described with a /nod/ object. }
  }
}
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8chain} for the details. }
         }
}

\author{ Jean-Baptiste Denis }

\examples{
showClass("chain");
}
\keyword{classes}
