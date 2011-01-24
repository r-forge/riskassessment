\name{look4k}
\alias{look4k}
\title{
looks associated constants to some words
}
\description{
Looks among the constants defined in the different packages which
seems relevant with respect to some proposed keywords.
}
\usage{
look4k(words, where=rbsb.pko, what="dv", strict=TRUE, how=length(words),
	  imp=TRUE)
}
\examples{
 rbsb3k("RESET"); # For R checking compliance
 look4k("print");
 look4k("daf", what=""); 
}
\arguments{
\item{words}{vector of keywords to be investigated.}
\item{where}{names of the packages to be investigated.}
\item{what}{what must be returned? \code{d} for definition and
\code{v} for value.}
\item{strict}{Must the search be an exact (versus fuzzy) search?}
\item{how}{how many positive results (with respect to each keyword
must be displayed?}
\item{imp}{must the result be displayed? if not a list with the
result is returned.}
}
\details{
The investigation is a fuzzy search with \code{agrep} values.\cr
}
\value{
Nothing but the result is displayed.
}
