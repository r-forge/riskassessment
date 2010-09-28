\name{print8dn}
\alias{print8dn}
\title{
prints a /dn/
}
\description{
(dn) prints a dn object with more or less details. If the score is
wanted, first transform the /dn/ with \code{score4dn}.
}
\usage{
print8dn(x, ..., quoi="tu", qui=rbsb.cha0, sorting=rbsb.num0, ndec=3,
	  simu=1:10)
}
\examples{
 rebastaba3k("RESET");
 print(rebastaba.dn2);
 rebastaba.mnu <- 8;
 print(score4dn(rebastaba.dn2));
 print(rebastaba.dn4, quoi="a");
 print(rebastaba.dn4, sorting="A");
}
\arguments{
\item{x}{the dn to print}
\item{\dots}{Further arguments to be passed to the print function.}
\item{quoi}{(\code{character(1)}) a character string indicating what
to print,\cr t: just the size and name,\cr d: description,\cr v:
variable names,\cr D: the domains of every variable,\cr i: some
individuals as indicated by simu (in the natural order if s is not
present)\cr s: indicates that the individuals must be ordered
according to the score.\cr u: univariate statistics,\cr b: bivariate
statistics,\cr a: all options = td}
\item{qui}{(\code{character(0:1)}) The variables which must be
considered for the printing. The default implies all of them, if not
either a character vector providing the nodes or a numeric vector
with the iin (internal numbers of them). Using this argument is a way
to modify the order of displaying the variables when printing.}
\item{sorting}{(\code{numeric} or \code{character}) indicates the
variable upon which the individuals have to be sorted before printing
(has got priority with respect to '}
\item{ndec}{ number of decimals used to print the values of the
numeric variables.}
\item{simu}{, which simulated values to print?\cr when 0: no
simulated values are printed,\cr when a vector: indicates the numbers
of simulations to print (default = the first ten),\cr when a value:
the percentage to be printed (50 = half of the simulations) }
}
\value{
returns nothing but a printing is performed
}
\keyword{print}
