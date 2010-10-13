\name{daf-class}
\docType{class}
\alias{daf-class}
\alias{print,daf-method}

\title{Class "daf" to define a data frame }
\description{ /daf/ objects define data frame given as an object (d), or
  through a function (f) or a text file (t) according to the slot \code{what}.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("daf",des=rbsb3k("nde"),what="d",valu="rbsb.df1")}.
}
\section{Slots}{
	 \describe{
    \item{\code{des}:}{Object of class \code{"des"}: description of the data.frame }
    \item{\code{what}:}{Object of class \code{"character"}: the way the data frame will be provided (*d*ata.frame,  *f*unction, *t*ext file, *c*vs file or *c*vs*2* file).  }
    \item{\code{valu}:}{Object of class \code{"character"}: name of an existing object: *d*ata.frame, *t*ext file, *c*vs type text file, *c2* (cvs2) type text file or *f*unction to be called without any argument. Notice that for the two 'cvs' format, the hash ('#') is introduced as comment character. }
  }
}
\section{Methods}{
  \describe{
    \item{print}{ see \code{print8daf} for the details }
	 }
}

\author{ J.-B. Denis }
\note{ ~~further notes~~ }

\examples{
showClass("daf")
}
\keyword{classes}
