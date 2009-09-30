\name{easyp3cut}
\alias{easyp3cut}
\title{
(f0) splits an expression into non nested blocks
}
\description{
x must be a character string which is parsed to extracts \code{n}
type of blocks where n is \code{length(opar)+1}. Something in between
\code{opar[i]} and \code{cpar[i]} is coded \code{i}, everything else
is coded \code{0}. See the examples.
}
\usage{
easyp3cut(x, opar=c("(", "{", "["), cpar=c(")", "}", "]"))
}
\examples{
 easyp3cut("(a+b)^[2]");
 easyp3cut("abs({{*Y*}})*{{A}}^{{B}}", "{{", "}}");
}
\arguments{
\item{x}{character string to split; must be of length one.}
\item{opar}{character defining the opening parentheses.}
\item{cpar}{character defining the closing parentheses (must be of
length \code{lenght(opar)}.}
}
\details{
The braces can contain more than one character, they must be
distinct.
}
\value{
A list of two equal length vectors: 

\item{\$blo}{The vector of character strings (braces are taken off)}
\item{\$typ}{Corresponding codes of the blocks}
}
\keyword{utilities}
