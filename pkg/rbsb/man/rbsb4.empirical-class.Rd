\name{empirical-class}
\docType{class}
\alias{empirical-class}
\alias{print,empirical-method}
\alias{plot,empirical-method}

\title{Class /empirical/ to empirically specify a probability distribution }
\description{
 Objects of class /empirical/ gives the possibility to store in a standardized
 way, all necessary pieces of information about the empirical definition
 of a probability distribution by means of weights at given values
 and/or a continuous density linearly piecewise defined. These
 distributions can easily be truncated only by changing their
 support. The final user does not have to bother about real (summing to
 one) probability : the \code{normalize8empirical} function was written
 for this purpose.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("empirical",...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{des}:}{Object of class \code{"des"} describing the object. }
    \item{\code{sup}:}{Object of class \code{"numeric(2)"} providing the
    support for the random variable.}
    \item{\code{repa}:}{Object of class \code{"numeric(2)"} giving the
      proportion of the discrete (\code{@repa[1]}) part and the
      continuous (\code{@repa[2]}) one.}
    \item{\code{xw}:}{A matrix with two columns, each row being
      dedicated to a weight: \code{@xw[,1]} for the random variable
      values and \code{@xw[,2]} for the associated non negative weights.}
    \item{\code{xy}:}{A matrix with two columns, each row being
      dedicated to a point of the continuous density: \code{@xw[,1]} for
      the random variable values and \code{@xw[,2]} for the associated
      value of the density. The first and last value of the density must
    be zero}
  }
}
\section{Methods}{
  \describe{
    \item{print:}{ see \code{print8empirical} for the details. }
    \item{plot:}{ see \code{plot8empirical} for the details. }
         }
}

\author{ Jean-Baptiste Denis }

\examples{
  rbsb3k("RESET");
  print.default(rbsb.epi5);
}
\keyword{classes}
