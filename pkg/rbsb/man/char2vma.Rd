\name{char2vma}
\alias{char2vma}
\title{
(00) transforms a character into a vector (or matrix, or array)
}
\description{
from a \code{character} vector, returns a vector, or a matrix, or an
array of characters with possibly names, or dimames. The information
can be supplied in different ways for each of the three
possibilities.
}
\usage{
char2vma(cha, quoi=rbsb.vma["v"], xsep=rbsb.sep1, nat="C")
}
\examples{
 rs003k("reset"); # For R checking convenience
 char2vma(letters, "c");
 char2vma(letters, "C", xsep="e");
 char2vma(letters);
 char2vma(letters, "V");
 char2vma(letters, "u");
 char2vma(c(LETTERS, letters), rbsb.vma["V"]);
 char2vma(c("A", "a", "B", "b", "C", "c"), rbsb.vma["u"]);
 char2vma(c(1:3, "//", 4:6), rbsb.vma["m"]);
 char2vma(c(1:3, "//", 4:6), rbsb.vma["M"]);
 char2vma(c(LETTERS[1:3], "//", 1:3, "//", 4:6), rbsb.vma["n"]);
 char2vma(c(LETTERS[1:3], "//", 1:3, "//", 4:6), "N");
 char2vma(c("a", 1:3, "//", "b", 4:6), "o");
 char2vma(c(c(LETTERS[1:3], "//", "a", 1:3, "//", "b", 4:6)), rbsb.vma["p"]);
 char2vma(c(2:4, "//", 1:24), "a");
 char2vma(c(2:4, "//", "one", "two", "//", LETTERS[1:3], "//", letters[1:4], "//", 1:24), "A");
 char2vma(c(2:4, "//", "one", "two", "//", LETTERS[1:3], "//", letters[1:4], "//", 1:24), "A", nat="N");
}
\arguments{
\item{cha}{ The character to transform.}
\item{quoi}{ Indicates which structure to return. For vectors, the
possibilities are : \code{rbsb.vma["c"]} for a no named character(1);
collapsing is done with \code{rbsb.sep0}. \code{rbsb.vma["C"]} for a
no named character() of any length (components are separated with
\code{xsep} whiwh are removed from the result); collapsing is done
with \code{rbsb.sep0}. \code{rbsb.vma["v"]} for a no named vector;
\code{rbsb.vma["V"]} for a named vector with all names before the
values; then an even number of components must be provided.
\code{rbsb.vma["u"]} for a named vector with names interlaced with
the value (name\_i, value\_i); then an even number of components must
be provided. For matrix, the possibilities are \code{rbsb.vma["m"]}
for a no named matrix given by row, two adjacent rows being separated
with \code{xsep} sequence, introduced as one of the component of
\code{cha}, then for a 2x3 matrix, the length of \code{cha} will be
6+2 = 8. \code{rbsb.vma["n"]} for a matrix with only the columns
names. The expected sequence is the names of columns, then the values
as for \code{rbsb.vma["m"]}; then for a 2x3 matrix, the length of
\code{cha} will be 3+1+8=12. \code{rbsb.vma["o"]} for a matrix with
only rows named. The expected sequence is name of row, values of
rows... Then 2x3 =\eqn{>} a length of 8+2=10. \code{rbsb.vma["p"]}
when names for columns and rows, in a mixed way... T}
 a length of 14. When
\code{rbsb.vma["M"],rbsb.vma["N"],rbsb.vma["o"],rbsb.vma["P"]}, the
same but the matrix will be transposed after being read; said in
another way, the values are given by columns. For an array, the
possibilities are \code{rbsb.vma["a"]} for a no named array, the
dimensions, \code{xsep}, the values in the classical order (varying
the first index the fastest). 2x3 will give a length of 2+1+6=9.
\code{rbsb.vma["A"]} for a named array, the dimension, \code{xsep},
the names of each dimension in the right order also separated and
finished with \code{xsep}. 2x3 will gives a length of
2+1+2+1+3+1+6=16. 

\item{xsep}{ Character sequence used to separate the character vector
into blocks giving information about the structure (see the e}
\item{nat}{ Nature of the returned vector. Can be \code{C} for
character, \code{N} for numeric or \code{L} for}
}
\details{
The processing is done in character mode but the result can be
transformed into numerical values with the help of argument
\code{nat}.
}
\value{
a vector or a matrix or an array according to the inputs
}
\keyword{IO}
