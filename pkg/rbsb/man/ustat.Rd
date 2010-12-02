\name{ustat}
\alias{ustat}
\title{
provides function computing usual statistics.
}
\description{
This function is aimed to be an argument for the function
\code{msdistri}. It returns a list of two parallel components: (1) a
vector a names of each statistics and (2) a list of functions
computing the statistics.
}
\usage{
ustat(wh="all")
}
\examples{
 rbsb3k("RESET"); # For R checking compliance
 print(ustat("n"));
}
\arguments{
\item{wh}{Defines which statistics must be used. The different
possibilities are:\cr "all" for all of the following;\cr "n" for
number of non missing values;\cr "m" for the mean;\cr "s" for the
standard deviation;\cr "M" for the median;\cr "i" for the minimum;\cr
"a" for the maximum;\cr "2" for the 0.025 and 0.975 quantiles and
their difference;\cr "5" for the 0.05 and 0.95 quantiles and their
difference;\cr "i" for the 0.25 and 0.75 quantiles and their
difference.}
}
\details{
The statistics are computed after removing the \code{NA} values.\cr
See the code for more details.
}
\value{
A named list, each component being a function taking a vector of
numeric values as input and returning the corresponding statistics.
}
