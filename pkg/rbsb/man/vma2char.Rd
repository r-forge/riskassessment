\name{vma2char}
\alias{vma2char}
\title{
(00) transforms a vector (or matrix, or array) into a character
}
\description{
from a vector, or a matrix, or an array, returns a \code{character}
vector. More or less the inverse function of \code{char2vma}.
}
\usage{
vma2char(x, xsep=rbsb.sep1)
}
\examples{
 rs003k("reset"); # For R checking convenience
 vma2char(letters);
 x <- letters; names(x) <- LETTERS;
 xx <- vma2char(x);
 char2vma(xx[[1]], xx[[2]]);
 vma2char(character(0));
 x <- matrix(1:20, 4);
 vma2char(x);
 dimnames(x) <- list(letters[1:4], LETTERS[1:5]);
 vma2char(x);
 x1 <- matrix(NA, 3, 0);
 xx1 <- vma2char(x1);
 char2vma(xx1[[1]], xx1[[2]]);
 dimnames(x1) <- list(c("i", "ii", "iii"), NULL);
 xx1 <- vma2char(x1);
 char2vma(xx1[[1]], xx1[[2]]);
 x <- array(1:24, 2:4);
 vma2char(x);
 dimnames(x) <- list(1:2, c("i", "ii", "iii"), c("I", "II", "III", "IV"));
 vma2char(x, xsep="|||");
 x0 <- array(NA, c(3, 0, 2));
 dimnames(x0) <- list(1:3, NULL, c("i", "ii"));
 xx0 <- vma2char(x0);
 char2vma(xx0[[1]], xx0[[2]]);
}
\arguments{
\item{x}{ The object to transform.}
\item{xsep}{ \code{character(1)} to be use for the separations.}
}
\details{
When some dimnames exist, the possible missing ones will be added.
}
\value{
a list with two components: \code{[[1]]} the coded character vector
and \code{[[2]]} the type according to \code{char2vma}.
}
\keyword{IO}
