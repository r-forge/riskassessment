\name{char2vect}
\alias{char2vect}
\title{
(00) transform a single character into a vector (or matrix)
}
\description{
Just doing a character or numeric vector from a single characters.
sep is used to separate the different items. wh indicates if the
result must be transformed with as numeric but before, an attempt is
done to returned a matrix (instead of a vector) separation subsets of
items (interpreted as rows). The separation for subset are items
identical to mat\_sep.\cr It is important to test the result to know
if the transformation was indeed performed.
}
\usage{
char2vect(cara, sep="", wh="C", mat_sep="//")
}
\examples{
 char2vect("A B C");
 char2vect("1 2 3", " ", "N");
 char2vect("0.1 0.9 // 0.5 0.5 // 0.9 0.1", " ", "C");
}
\arguments{
\item{cara}{ The single character to transform.}
\item{sep}{ Separators for the splitting. when '', no splitting is
done.}
\item{wh}{ Type of conversion: 'C' means character, 'N' means
numeric.}
\item{mat\_sep}{ Separator to be used to indicate subsets associated
to rows of a matrix. When it is '', no subsetting is performed.}
}
\details{
Argument 'sep' is a possible separator. If empty no splitting is
performed. When one of the item is mat\_sep, then an attempt is done
to get a matrix but if the sizes of subsets are not identical, an
erreur is returned. It then must be tested.
}
\value{
a character (or numeric) vector if everything was good. a 'faux'
object otherwise.
}
\keyword{utilities}
