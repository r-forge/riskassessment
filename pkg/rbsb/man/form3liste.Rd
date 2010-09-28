\name{form3liste}
\alias{form3liste}
\title{
formats a series of names or whatever
}
\description{
returns a scalar character of the names surrounded by any kind of
parenthesis and separated with the same separator.
}
\usage{
form3liste(vcara, none="-", OPA="{", CPA="}", opa="(", cpa=")", sep="+",
	  imp=FALSE, cr=imp)
}
\examples{
 form3liste(letters[1:4])
}
\arguments{
\item{vcara}{Character vector to be considered.}
\item{none}{ The internal result if vcara is length zero. When it is
\code{character(0)} then the global parenthesis are not given
contrary when it is \code{""}.}
\item{OPA}{ The opening parenthesis to surround the entire list.}
\item{CPA}{ The closing parenthesis to surround the entire list.}
\item{opa}{ The opening parenthesis to surround each name.}
\item{cpa}{ The closing parenthesis to surround each name.}
\item{sep}{ The symbol to separate each name.}
\item{imp}{ Must the result be printed (with cat) or returned?}
\item{cr}{ Must a line feed be added?}
}
\value{
A character string or nothing when imp is TRUE
}
\keyword{print}
