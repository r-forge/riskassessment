\name{range5empirical}
\alias{range5empirical}
\title{
returns the ranges of an /empirical/
}
\description{
Returns the different supports of an /empirical/ that is the minimum
segment where stand the variation of the random variable, taking into
account (or not) the \code{@sup} slot. See the section 'value' for
details.
}
\usage{
range5empirical(empi)
}
\examples{
 rbsb3k("RESET"); # for R checking compliance (useless)
 range5empirical(new("empirical"));
}
\arguments{
\item{empi}{The \code{empirical} to consider.}
}
\details{
\code{empi} is not normalized, so the range can exceed the
\code{@sup}.
}
\value{
A list with four components: \code{$rcon}: the range of the
continuous part (code\{c(NA,NA)\} if it does not exist).
\code{$rdis}: the set of possible values for the discrete part
(code\{numeric(0)\} if it does not exist). \code{$remp}: the range of
both parts not taking into account the slot \code{@sup}.
\code{$rang}: the range of both parts taking into account the slot
\code{@sup}, identical to \code{$remp} after normalisation.
}
