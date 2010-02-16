\name{easyp3cut}
\alias{easyp3cut}
\title{
(00) splits an expression into non nested blocks
}
\description{
x must be a character string which is parsed to extracts \code{n}
types of blocks where n is \code{nrow(pth)+1}. Something in between
\code{pth[i,1]} and \code{pth[i,2]} is coded \code{i}, everything
else is coded \code{0}. See the examples.
}
\usage{
easyp3cut(x, pth=matrix(c("(", "{", "[", ")", "}", "]"), 3))
}
\examples{
 rs003k("RESET"); # for R checking compliance (useless)
 easyp3cut("(a+b)^[2]");
 easyp3cut("abs({{*Y*}})*{{A}}^{{B}}", matrix(c("{{", "}}"), 1));
}
\arguments{
\item{x}{character string to split; must be of length one.}
\item{pth}{character matrix with two columns defining the opening
parentheses in the first column and the closing parentheses in the
second column.}
}
\details{
The braces can contain more than one character, they must be
distinct. Be aware that no check is done about the possible nesting
of the parentheses.
}
\value{
A list of two equal length vectors: 

\item{\$blo}{The vector of character strings (braces are taken off)}
\item{\$typ}{Corresponding codes of the blocks and 0 when outside of
any defined block.}
}
\keyword{utilities}
