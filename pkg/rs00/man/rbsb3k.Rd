\name{rbsb3k}
\alias{rbsb3k}
\title{
(f0) standard constants
}
\description{
defines/returns/prints the constants used within /rbsb/. The
performed action depends on the first argument. The set involved
depends on the second argument.\cr Be aware that there is no checks
about the modification of values... Not recommended for a standard
use.
}
\usage{
rbsb3k(whi="msi", wha='cval', val=NULL)
}
\examples{
 names(rbsb3k("*", "list"));
 rbsb3k('*', 'std');
 rbsb3k('*', 'print');
 rbsb3k('msi', 'nval', 'my rebastaba');
 rbsb3k('msi', 'print');
 rbsb3k("min", "nval", 123);
 rbsb3k("min", "print");
 rbsb3k('*', 'std');
 is.matrix(rbsb3k("cpt"));
}
\arguments{
\item{whi}{(='msi') a character(1) indicating which subset of /rbsb/
constants must be updated/returned/printed. \code{'*'} for all;
\code{'m'} for the miscellaneous subset; \code{'f'} for the file
subset; \code{'c'} for the coding subset; \code{'n'} for the null
object subset. \code{'s'} for the statistics sunset. \cr And constant
name for individual subsets (with three characters).}
\item{wha}{(='cval') What to do with the designated subset. can be
\code{'std'} then the standard values are imposed to the designated
subset of constants. Or \code{'print'} then the designated subset of
constants is printed. Or \code{'list'}, then the designated subset of
constants is returned as a named list. Or \code{'cval'} then the
current value is returned (only one constant). Or \code{'nval'} then
the new value is imposed to the one constant precised with
\code{whi'}. }
\item{val}{(=NULL) any object to be imposed to the designated
constant by \code{whi} when \code{wha} is \code{'nval'}.}
}
\details{
This solution was adopted to replace a set of global constants that I
was not able to make acceptable with R packages standards. It is
recommended to use rbsb3k to change the values of the constants, but
this can be directly done with \code{options}.
}
\value{
According to \code{wa} nothing (with possibly a printing), a list, an
objet.
}
\keyword{misc}
