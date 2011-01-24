\name{rempirical}
\alias{rempirical}
\title{
returns pseudo-random draws from an /empirical/
}
\description{
This function returns \code{n} independent pseudo-random draw from
the /empirical/ object \code{empi}. The user is advised to manage the
pseudo-random seed before calling this function. The object is
normalized before being used (see \code{normalize8empirical}).
}
\usage{
rempirical(n, empi)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 dempirical(100, new("empirical"));
}
\arguments{
\item{n}{Wanted number of draws.}
\item{empi}{The \code{empirical} object to consider.}
}
\details{
Just a call to \code{qempirical} with draw from the basic
\code{runif}. But the bad convention of using \code{length(n)} when
it is greater than 1 is not followed in case of \code{rbsb.mck}.
}
\value{
A vector with length \code{n} containing the drawn values.
}
